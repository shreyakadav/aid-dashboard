# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(treemapify)
library(viridis)
library(gganimate)
library(gifski)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggalluvial)
library(wordcloud)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(av)

# Load dataset once at global scope to avoid reloading each time
aiddata <- read.csv("C:/Users/skadav/Downloads/AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv")

# Preprocessing
aid_year <- aiddata %>%
  group_by(year) %>%
  summarise(total_aid = sum(commitment_amount_usd_constant_sum, na.rm = TRUE)) %>%
  filter(year != 9999) %>%
  mutate(total_aid_billion = total_aid / 1e9)

aid_country_year <- aiddata %>%
  group_by(donor, year) %>%
  summarise(total_aid = sum(commitment_amount_usd_constant_sum, na.rm = TRUE)) %>%
  filter(year != 9999) %>%
  mutate(total_aid_billion = total_aid / 1e9)

country_donors <- c("Australia", "Austria", "Belgium", "Brazil", "Canada", "Chile", "Colombia", "Cyprus",
                    "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                    "Iceland", "India", "Ireland", "Italy", "Japan", "Korea", "Kuwait", "Latvia", "Liechtenstein",
                    "Lithuania", "Luxembourg", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland",
                    "Portugal", "Qatar", "Romania", "Saudi Arabia", "Slovak Republic", "Slovenia", "South Africa",
                    "Spain", "Sweden", "Switzerland", "Taiwan", "Thailand", "United Arab Emirates", "United Kingdom",
                    "United States")

aid_country_year_filtered <- aid_country_year %>%
  filter(donor %in% country_donors)

top_donors <- aid_country_year_filtered %>%
  group_by(donor) %>%
  summarise(total_aid = sum(total_aid, na.rm = TRUE)) %>%
  arrange(desc(total_aid)) %>%
  slice(1:5) %>%
  pull(donor)

df_top_donors <- aid_country_year %>%
  filter(donor %in% top_donors)

# UI
ui <- fluidPage(
  titlePanel("Global Aid Spending Dashboard"),
  
  # Display the question
  div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #e6f2ff; border-left: 5px solid #007acc; font-weight: bold",
    "Question: Is there an increase or decrease in the aid spending by the countries over the years?"
    ),

  # Navigation Bar
  navlistPanel(
    id = "tabset",
    
    "Aid Over the Years",
    tabPanel("Total Aid Over Time", plotOutput("line_total_aid")),
    tabPanel("Bar Chart by Year", plotOutput("barchart_total_aid")),
    tabPanel("Donor Heatmap", plotOutput("heatmap_donors")),
    tabPanel("Animated Line Chart",
             tags$video(src = "total_aid_over_time.mp4", type = "video/mp4", 
                        controls = NA, autoplay = NA, width = "100%")
    ),
    
    "Top 5 Donor Countries",
    tabPanel("Top Donors Bar", plotOutput("top_donors_bar")),
    tabPanel("Top Donors Line Grid", plotOutput("top_donors_grid")),
    tabPanel("Bubble Plot", plotOutput("bubble_plot")),
    tabPanel("Heatmap Over Time", plotOutput("heatmap_over_time")),
    tabPanel("Static Treemap", plotOutput("treemap")),
    tabPanel("Animation - Top 5 Donors",
             tags$video(src = "aid_animation.mp4", type = "video/mp4", 
                        controls = NA, autoplay = NA, width = "100%")),
  )
)

# Server
server <- function(input, output) {
  
  # Line Chart
  output$line_total_aid <- renderPlot({
    ggplot(aid_year, aes(x = year, y = total_aid_billion)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(title = "Total Aid Spending Over Time", x = "Year", y = "Total Aid (in Billions USD)") +
      theme_minimal()
  })
  
  # Bar Chart
  output$barchart_total_aid <- renderPlot({
    ggplot(aid_year, aes(x = as.factor(year), y = total_aid_billion, fill = total_aid)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Total Aid Spending Per Year", x = "Year", y = "Total Aid (in Billions USD)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Heatmap
  output$heatmap_donors <- renderPlot({
    ggplot(aid_country_year_filtered, aes(x = year, y = reorder(donor, -total_aid_billion), fill = total_aid_billion)) +
      geom_tile() +
      scale_fill_viridis(option = "C", trans = "log") +
      labs(title = "Aid Distribution Across Donor Countries and Years", x = "Year", y = "Donor Country") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Animated Line Chart
  observe({
    req(aid_year)
    
    # Animated plot object
    p <- ggplot(aid_year, aes(x = year, y = total_aid_billion)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(title = "Animated: Total Aid Spending Over Time",
           x = "Year", y = "Total Aid (in Billions USD)") +
      theme_minimal() +
      transition_reveal(year)
    
    # Create `www/` folder if not exists
    if (!dir.exists("www")) dir.create("www")
    
    # Save animation as MP4 into www/
    anim_save("www/total_aid_over_time.mp4", animation = p, renderer = av_renderer())
  })
  
  # Bar Chart
  output$top_donors_bar <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = total_aid_billion, fill = donor)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Top 5 Donors' Contribution Over Time", x = "Year", y = "Total Aid (in Billions USD)") +
      theme_minimal()
  })
  
  # Multiple Line Charts
  output$top_donors_grid <- renderPlot({
    ggplot(aid_country_year_filtered %>% filter(donor %in% top_donors),
           aes(x = year, y = total_aid_billion, color = donor)) +
      geom_line(size = 1) +
      facet_wrap(~ donor, scales = "free_y") +
      labs(title = "Aid Contributions Over Time (Faceted by Donor)", x = "Year", y = "Total Aid (in Billions USD)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bubble Plot
  output$bubble_plot <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = donor, size = total_aid_billion, fill = total_aid_billion)) +
      geom_point(alpha = 0.7, shape = 21, color = "black") +
      scale_size(range = c(2, 15)) +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Aid Contributions Over Time (Bubble Plot)", x = "Year", y = "Donor Country") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Heatmap
  output$heatmap_over_time <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = donor, fill = total_aid_billion)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C", name = "Total Aid (Billions USD)") +
      labs(title = "Heatmap of Aid Contributions Over Time", x = "Year", y = "Donor Country") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Treemap
  output$treemap <- renderPlot({
    donor_totals <- df_top_donors %>%
      group_by(donor) %>%
      summarise(total_aid_billion = sum(total_aid_billion, na.rm = TRUE))
    
    ggplot(donor_totals, aes(area = total_aid_billion, fill = donor, label = donor)) +
      geom_treemap() +
      geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Treemap of Total Aid Contributions (Aggregated by Donor)") +
      theme_minimal()
  })
  
  # Animated Line Chart
  observe({
    req(df_top_donors)
    
    # Create animation
    p <- ggplot(df_top_donors, aes(x = year, y = total_aid_billion, color = donor, group = donor)) +
      geom_line(size = 1) +
      labs(title = "Aid Contributions Over Time",
           x = "Year", y = "Total Aid (Billions USD)") +
      theme_minimal() +
      transition_reveal(year)
    
    # Ensure www folder exists
    if (!dir.exists("www")) dir.create("www")
    
    # Save MP4 in www
    anim_save("www/aid_animation.mp4", animation = p, renderer = av_renderer())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)