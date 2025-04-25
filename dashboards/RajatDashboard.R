# install.packages('DT')
library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(treemap)
library(igraph)
library(ggraph)
library(tidyverse)
library(circlize)

# Load data
aid_data <- read.csv("AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv", header = TRUE)

get_top_pairs <- function(aid_data, n) {
  top_pairs <- aid_data %>%
    group_by(donor, recipient) %>%
    summarise(Total_Spending = sum(commitment_amount_usd_constant_sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Total_Spending)) %>%
    slice(1:n)
  return(top_pairs)
}

# UI
ui <- fluidPage(
  titlePanel("Donor-Recipient Aid Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_n", "Select Top N Pairs:", min = 5, max = 20, value = 10),
      hr(),
      helpText("Visualizations for international aid by donor-recipient pairs using slider-based input.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotlyOutput("barChart")),
        tabPanel("Heatmap", plotlyOutput("heatmap")),
        tabPanel("Treemap", plotOutput("treemap")),
        tabPanel("Network", plotOutput("network", height = "700px")),
        tabPanel("Chord Diagram", 
                 plotOutput("chord", height = "700px"),
                 br(),
                 helpText("Note: Chord diagram layout may vary slightly each time.")),
        tabPanel("Raw Data", DTOutput("table"), br(), textOutput("tableInfo"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  data_filtered <- reactive({
    get_top_pairs(aid_data, input$top_n)
  })
  
  output$barChart <- renderPlotly({
    df <- data_filtered() %>%
      mutate(Total_Spending_Million = Total_Spending / 1e6)
    
    p <- ggplot(df, aes(x = reorder(paste(donor, recipient, sep = " -> "), Total_Spending_Million), 
                        y = Total_Spending_Million)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_y_continuous(labels = scales::comma) +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "Donor-Recipient Pairs"),
           x = "Donor -> Recipient", y = "Total Spending (Millions USD)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$heatmap <- renderPlotly({
    df <- data_filtered() %>%
      mutate(Total_Spending_Million = Total_Spending / 1e6)
    
    p <- ggplot(df, aes(x = donor, y = recipient, fill = Total_Spending_Million)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "yellow", high = "red", labels = scales::comma) +
      labs(title = "Heatmap of Donor-Recipient Spending",
           x = "Donor", y = "Recipient", fill = "Spending (Millions USD)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$treemap <- renderPlot({
    df <- data_filtered()
    treemap(df,
            index = c("donor", "recipient"),
            vSize = "Total_Spending",
            title = paste("Treemap of Top", input$top_n, "Donor-Recipient Spending"),
            palette = "Blues")
  })
  
  output$network <- renderPlot({
    df <- data_filtered()
    edges <- df
    nodes <- data.frame(
      name = unique(c(df$donor, df$recipient)),
      type = ifelse(unique(c(df$donor, df$recipient)) %in% df$donor, "Donor", "Recipient")
    )
    g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
    ggraph(g, layout = "fr") +
      geom_edge_link(alpha = 0.5, color = "gray") +
      geom_node_point(aes(color = type), size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_void() +
      labs(title = "Donor-Recipient Network") +
      scale_color_manual(values = c("Donor" = "orange", "Recipient" = "blue"))
  })
  
  output$chord <- renderPlot({
    df <- data_filtered()
    connections <- df[, c("donor", "recipient", "Total_Spending")]
    all_labels <- unique(c(connections$donor, connections$recipient))
    grid.col <- setNames(
      c(rep("red", length(unique(connections$donor))), rep("blue", length(unique(connections$recipient)))),
      all_labels
    )
    circos.clear()
    chordDiagram(connections,
                 transparency = 0.5,
                 grid.col = grid.col,
                 annotationTrack = c("grid"),
                 preAllocateTracks = list(track.height = 0.05))
    title(paste("Chord Diagram of Top", input$top_n, "Donor-Recipient pairs"))
    circos.track(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      mean_x <- mean(xlim)
      text_facing <- ifelse(mean_x > 0, "clockwise", "reverse.clockwise")
      circos.text(mean_x, ylim[1] + 0.1, sector.name,
                  facing = text_facing, niceFacing = TRUE, cex = 0.8)
    }, bg.border = NA)
  })
  
  output$table <- renderDataTable({
    data_filtered()
  })
}


# Run the app
shinyApp(ui, server)
