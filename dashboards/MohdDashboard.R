# install.packages(c('shiny','DT','dplyr','ggplot2','plotly','treemap'))
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(treemap)

# Load data
aid_data <- read.csv(
  "AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Q6 helper: find top N donor→recipient pairs
get_top_bilateral <- function(data, n) {
  data %>%
    group_by(donor, recipient) %>%
    summarise(
      Total_Spending = sum(commitment_amount_usd_constant_sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Spending)) %>%
    slice(1:n)
}

ui <- fluidPage(
  titlePanel("🤝 Top Bilateral Aid Pairs"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "top_n", "Number of Pairs to Show:", 
        min = 5, max = 50, value = 10, step = 5
      ),
      helpText("Which donor→recipient pairs have the highest total spending?")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bubble Chart", plotlyOutput("bubble")),
        tabPanel("Sankey Flow", plotlyOutput("sankey")),
        tabPanel("Treemap", plotOutput("treemap")),
        tabPanel("Data Table", DTOutput("table"), textOutput("countText"))
      )
    )
  )
)

server <- function(input, output, session) {
  bilateral <- reactive({
    get_top_bilateral(aid_data, input$top_n)
  })
  
  # 1) Bubble chart: size ~ spending, with controlled px range
  output$bubble <- renderPlotly({
    df <- bilateral() %>%
      mutate(Millions = Total_Spending / 1e6,
             label = paste(donor, "→", recipient))
    
    plot_ly(
      df,
      x = ~donor, 
      y = ~recipient,
      type = 'scatter',
      mode = 'markers',
      size = ~Millions,
      sizes = c(10, 60),            # min 10px, max 60px
      color = ~Millions,
      colors = "Blues",
      text = ~paste(label, "<br>$", scales::comma(Total_Spending)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Top", input$top_n, "Donor→Recipient Bubbles"),
        xaxis = list(title = "Donor"),
        yaxis = list(title = "Recipient")
      )
  })
  
  # 2) Sankey diagram
  output$sankey <- renderPlotly({
    df <- bilateral()
    nodes <- data.frame(name = unique(c(df$donor, df$recipient)), stringsAsFactors = FALSE)
    df2 <- df %>%
      mutate(
        source = match(donor, nodes$name) - 1,
        target = match(recipient, nodes$name) - 1
      )
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(label = nodes$name, pad = 15, thickness = 15),
      link = list(
        source = df2$source,
        target = df2$target,
        value  = df2$Total_Spending
      )
    ) %>%
      layout(title = paste("Sankey: Top", input$top_n, "Flows"))
  })
  
  # 3) Treemap
  output$treemap <- renderPlot({
    tm <- bilateral()
    treemap(
      tm,
      index = c("donor", "recipient"),
      vSize = "Total_Spending",
      title = paste("Treemap of Top", input$top_n, "Pairs"),
      palette = "Blues"
    )
  })
  
  # 4) Data table + count
  output$table <- renderDT({
    bilateral()
  }, options = list(pageLength = input$top_n))
  
  output$countText <- renderText({
    paste("Showing", nrow(bilateral()), "donor→recipient pairs.")
  })
}

shinyApp(ui, server)
