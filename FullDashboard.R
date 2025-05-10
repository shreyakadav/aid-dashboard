library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(treemap)
library(treemapify)
library(igraph)
library(ggraph)
library(circlize)
library(scales)
library(viridis)
library(gganimate)
library(gifski)
library(av)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggalluvial)
library(wordcloud)
library(RColorBrewer)
library(sf)

raw_aid_data <- read_csv("data/AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv") %>%
  filter(year != 9999)

world_map <- read_sf("data/World_Countries_(Generalized)_9029012925078512962.geojson")

# Define Broad Aid Categories
aid_data_broad <- raw_aid_data %>%
  mutate(broad_category = case_when(
    # Education
    str_detect(coalesced_purpose_name, regex("education|training|teacher", ignore_case = TRUE)) ~ "Education",
    coalesced_purpose_name == "Basic life skills for youth and adults" ~ "Education",
    
    # Health
    str_detect(coalesced_purpose_name, regex("health|medical|nutrition|STD|HIV|reproductive", ignore_case = TRUE)) ~ "Health",
    coalesced_purpose_name == "Infectious & Parasitic disease control" ~ "Health",
    
    # Government and Civil Society
    str_detect(coalesced_purpose_name, regex("government|civil society|judicial|human rights|election|conflict|peace|demobilisation|security system", ignore_case = TRUE)) ~ "Government and Civil Society",
    coalesced_purpose_name %in% c(
      "Women in development", 
      "Women's equality organisations and institutions", 
      "WOMEN'S EQUALITY ORGANISATIONS AND INSTITUTIONS", 
      "Support to international NGOs", 
      "Support to local and regional NGOs", 
      "Support to national NGOs", 
      "Social/ welfare services", 
      "Employment policy and administrative management", 
      "General budget support", 
      "Economic and development policy/planning", 
      "Reintegration and SALW control"
    ) ~ "Government and Civil Society",
    
    # Economic Infrastructure and Services
    str_detect(coalesced_purpose_name, regex("transport|energy|power|communication|ICT|sanitation|water supply|construction", ignore_case = TRUE)) ~ "Economic Infrastructure and Services",
    coalesced_purpose_name %in% c(
      "Electrical transmission/ distribution", 
      "Gas distribution", 
      "Petroleum distribution and storage", 
      "Housing policy and administrative management", 
      "Economic Infrastructure & Services", 
      "Storage", 
      "Water resources policy and administrative management"
    ) ~ "Economic Infrastructure and Services",
    
    # Production Sectors
    str_detect(coalesced_purpose_name, regex("agricult|fish|forestry|mining|industry|tourism|alternative development|SME", ignore_case = TRUE)) ~ "Production Sectors",
    coalesced_purpose_name %in% c(
      "Cottage industries and handicraft", 
      "Industrial development", 
      "Industrial policy and administrative management", 
      "Mineral/Metal prospection and exploration", 
      "Production Sectors"
    ) ~ "Production Sectors",
    
    # Environment
    str_detect(coalesced_purpose_name, regex("environment|biodiversity|climate|flood|biosphere|waste", ignore_case = TRUE)) ~ "Environment",
    coalesced_purpose_name %in% c(
      "Bio-diversity", 
      "River development", 
      "Water resources protection", 
      "Water Research"
    ) ~ "Environment",
    
    # Humanitarian Aid and Emergency Relief
    str_detect(coalesced_purpose_name, regex("emergency|relief|disaster|refugee|rehabilitation|reconstruction", ignore_case = TRUE)) ~ "Humanitarian Aid and Emergency Relief",
    coalesced_purpose_name %in% c(
      "Land mine clearance", 
      "Food aid/Food security programmes"
    ) ~ "Humanitarian Aid and Emergency Relief",
    
    # Multisector / Cross-cutting
    str_detect(coalesced_purpose_name, regex("multisector|urban|rural|combination of purposes", ignore_case = TRUE)) ~ "Multisector / Cross-cutting",
    coalesced_purpose_name %in% c(
      "Research/scientific institutions", 
      "Technological research and development", 
      "Other Social and Infrastructure Services", 
      "Social Infrastructure and Services"
    ) ~ "Multisector / Cross-cutting",
    
    # Trade and Financial Services
    str_detect(coalesced_purpose_name, regex("trade|bank|financial|monetary|export|debt|SME", ignore_case = TRUE)) ~ "Trade and Financial Services",
    coalesced_purpose_name %in% c(
      "Business and Other Services", 
      "Business support services and institutions", 
      "Import support (capital goods)", 
      "Import support (commodities)", 
      "Privatisation", 
      "Rescheduling and refinancing"
    ) ~ "Trade and Financial Services",
    
    # Population and Reproductive Health
    str_detect(coalesced_purpose_name, regex("population|family planning", ignore_case = TRUE)) ~ "Population and Reproductive Health",
    
    # Administrative & Support
    str_detect(coalesced_purpose_name, regex("administrative cost|development awareness", ignore_case = TRUE)) ~ "Administrative & Support",
    
    # Unspecified / Miscellaneous
    str_detect(coalesced_purpose_name, regex("unspecified|miscellaneous|free flow|site preservation|media|purpose unspecified|combination of activities", ignore_case = TRUE)) ~ "Unspecified / Miscellaneous",
    coalesced_purpose_name == "Sectors not specified" ~ "Unspecified / Miscellaneous",
    
    # Default
    TRUE ~ "Other"
  ))

# Create Mohd and Rajat Dashboard Data Function
get_top_pairs <- function(data, n) {
  data %>%
    group_by(donor, recipient) %>%
    summarise(Total_Spending = sum(commitment_amount_usd_constant_sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Total_Spending)) %>%
    slice(1:n)
}

# Shreya Dashboard Aggregates
aid_year <- raw_aid_data %>%
  group_by(year) %>%
  summarise(total_aid = sum(commitment_amount_usd_constant_sum, na.rm = TRUE)) %>%
  mutate(total_aid_billion = total_aid / 1e9)

aid_country_year <- raw_aid_data %>%
  group_by(donor, year) %>%
  summarise(total_aid = sum(commitment_amount_usd_constant_sum, na.rm = TRUE)) %>%
  mutate(total_aid_billion = total_aid / 1e9)

country_donors <- c(
  "Australia", "Austria", "Belgium", "Brazil", "Canada", "Chile", "Colombia",
  "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
  "Germany", "Greece", "Hungary", "Iceland", "India", "Ireland", "Italy", 
  "Japan", "Korea", "Kuwait", "Latvia", "Liechtenstein", "Lithuania", 
  "Luxembourg", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland", 
  "Portugal", "Qatar", "Romania", "Saudi Arabia", "Slovak Republic", 
  "Slovenia", "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", 
  "Thailand", "United Arab Emirates", "United Kingdom", "United States"
)

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

# Before and After 2008 Aid Aggregation
aid_agg <- raw_aid_data %>%
  group_by(year, donor, recipient) %>%
  summarise(total_commitment = sum(commitment_amount_usd_constant_sum, na.rm = TRUE), .groups = "drop")

aid_beforeafter_donor <- aid_agg %>%
  filter(year >= 2005) %>%
  mutate(period = ifelse(year < 2008, 'Before', 'After')) %>%
  group_by(donor, period) %>%
  summarise(aid_given = sum(total_commitment), .groups = "drop") %>%
  mutate(COUNTRY = donor)

donor_beforeafter_joined <- world_map %>%
  left_join(aid_beforeafter_donor)

df_donor_before <- donor_beforeafter_joined %>% filter(is.na(period) | period == "Before")
df_donor_after <- donor_beforeafter_joined %>% filter(is.na(period) | period == "After")
df_donor <- donor_beforeafter_joined %>% filter(!is.na(aid_given))

aid_beforeafter_recipient <- aid_agg %>%
  filter(year >= 2005) %>%
  mutate(period = ifelse(year < 2008, 'Before', 'After')) %>%
  group_by(recipient, period) %>%
  summarise(aid_received = sum(total_commitment), .groups = "drop") %>%
  mutate(COUNTRY = recipient)

recipient_beforeafter_joined <- world_map %>%
  left_join(aid_beforeafter_recipient)

df_recip_before <- recipient_beforeafter_joined %>% filter(is.na(period) | period == "Before")
df_recip_after <- recipient_beforeafter_joined %>% filter(is.na(period) | period == "After")
df_recip <- recipient_beforeafter_joined %>% filter(!is.na(aid_received))


# UI
ui <- navbarPage("Global Aid Dashboard",
                 
                 tabPanel("Mohd Dashboard",
                          fluidPage(
                            div(style="padding:10px; font-weight:bold;", "Question: What are the largest donor-recipient aid relationships based on top spending?"),
                            sidebarLayout(
                              sidebarPanel(sliderInput("mohd_top_n", "Select Top N Pairs:", min = 5, max = 50, value = 10)),
                              mainPanel(tabsetPanel(
                                tabPanel("Bubble Chart", plotlyOutput("bubble_mohd")),
                                tabPanel("Sankey Flow", plotlyOutput("sankey_mohd")),
                                tabPanel("Treemap", plotOutput("treemap_mohd")),
                                tabPanel("Data Table", DTOutput("table_mohd"))
                              ))
                            )
                          )
                 ),
                 
                 tabPanel("Rajat Dashboard",
                          fluidPage(
                            div(style="padding:10px; font-weight:bold;", "Question: How is international aid distributed among donor-recipient pairs?"),
                            sidebarLayout(
                              sidebarPanel(sliderInput("rajat_top_n", "Select Top N Pairs:", min = 5, max = 20, value = 10)),
                              mainPanel(tabsetPanel(
                                tabPanel("Bar Chart", plotlyOutput("bar_rajat")),
                                tabPanel("Heatmap", plotlyOutput("heatmap_rajat")),
                                tabPanel("Treemap", plotOutput("treemap_rajat")),
                                tabPanel("Network", plotOutput("network_rajat", height = "700px")),
                                tabPanel("Chord Diagram", plotOutput("chord_rajat", height = "700px")),
                                tabPanel("Raw Data", DTOutput("table_rajat"))
                              ))
                            )
                          )
                 ),
                 
                 tabPanel("Shreya Dashboard",
                          fluidPage(
                            div(style="padding:10px; font-weight:bold;", "Question: How has overall aid spending evolved over time and among top donor countries?"),
                            sidebarLayout(
                              sidebarPanel(helpText("Top Donors Aid Trends")),
                              mainPanel(tabsetPanel(
                                tabPanel("Total Aid Over Time", plotOutput("line_total_aid")),
                                tabPanel("Top Donors Bar Chart", plotOutput("top_donors_bar")),
                                tabPanel("Top Donors Line Grid", plotOutput("top_donors_grid")),
                                tabPanel("Bubble Plot", plotOutput("bubble_plot")),
                                tabPanel("Heatmap Over Time", plotOutput("heatmap_over_time")),
                                tabPanel("Static Treemap", plotOutput("treemap_static"))
                              ))
                            )
                          )
                 ),
                 
                 tabPanel("Premann Dashboard",
                          fluidPage(
                            div(style="padding:10px; font-weight:bold;", "Question: Which broad sectors receive the most international aid?"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("years_aid", "Select Year Range:", min = min(aid_data_broad$year), max = max(aid_data_broad$year), value = c(2010, 2020)),
                                numericInput("top_n_aid", "Top Categories:", 10, min = 1)
                              ),
                              mainPanel(plotOutput("broad_categories_plot"))
                            )
                          )
                 ),
                 
                 tabPanel("Marissa Dashboard",
                          fluidPage(
                            div(style="padding:10px; font-weight:bold;", "Question: What are the differences in worldwide aid before and after 2008?"),
                            plotOutput("donors_plot1"),
                            plotOutput("donors_plot2"),
                            selectInput("donors_select", "Select a Donor Country:", choices = unique(df_donor$donor)),
                            plotOutput("donors_plot3"),
                            plotOutput("recip_plot1"),
                            plotOutput("recip_plot2"),
                            selectInput("recip_select", "Select a Recipient Country:", choices = unique(df_recip$recipient)),
                            plotOutput("recip_plot3")
                          )
                 )
)


# SERVER
server <- function(input, output) {
  
  ## Mohd Dashboard Outputs
  mohd_data <- reactive({ get_top_pairs(raw_aid_data, input$mohd_top_n) })
  
  output$bubble_mohd <- renderPlotly({
    df <- mohd_data()
    plot_ly(df, x = ~donor, y = ~recipient, type = 'scatter', mode = 'markers',
            size = ~Total_Spending/1e6, sizes = c(10, 60), color = ~Total_Spending/1e6, colors = "Blues",
            text = ~paste(donor, "to", recipient, "\n$", scales::comma(Total_Spending))) %>%
      layout(title = "Top Donor-Recipient Aid Relationships",
             xaxis = list(title = "Donor Country"),
             yaxis = list(title = "Recipient Country"))
  })
  
  output$sankey_mohd <- renderPlotly({
    df <- mohd_data()
    nodes <- data.frame(name = unique(c(df$donor, df$recipient)))
    df <- df %>% mutate(source = match(donor, nodes$name) - 1, target = match(recipient, nodes$name) - 1)
    plot_ly(type = "sankey",
            node = list(label = nodes$name, pad = 20),
            link = list(source = df$source, target = df$target, value = df$Total_Spending)) %>%
      layout(title = "Sankey Flow of Aid Between Donors and Recipients")
  })
  
  output$treemap_mohd <- renderPlot({
    treemap(mohd_data(),
            index = c("donor", "recipient"),
            vSize = "Total_Spending",
            title = "Treemap of Top Donor-Recipient Aid Flows",
            palette = "Blues")
  })
  
  output$table_mohd <- renderDT({ mohd_data() })
  
  
  ## Rajat Dashboard Outputs
  rajat_data <- reactive({ get_top_pairs(raw_aid_data, input$rajat_top_n) })
  
  output$bar_rajat <- renderPlotly({
    df <- rajat_data()
    p <- ggplot(df, aes(x = reorder(paste(donor, recipient), Total_Spending), y = Total_Spending/1e6)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Donor-Recipient Aid Pairs",
           x = "Donor - Recipient Pair",
           y = "Total Aid (Million USD)")
    ggplotly(p)
  })
  
  output$heatmap_rajat <- renderPlotly({
    df <- rajat_data()
    p <- ggplot(df, aes(donor, recipient, fill = Total_Spending/1e6)) +
      geom_tile(color = "white") +
      scale_fill_viridis(name = "Aid (Million USD)") +
      labs(title = "Heatmap of Donor vs Recipient Total Aid",
           x = "Donor Country",
           y = "Recipient Country")
    ggplotly(p)
  })
  
  output$treemap_rajat <- renderPlot({
    treemap(rajat_data(),
            index = c("donor", "recipient"),
            vSize = "Total_Spending",
            title = "Treemap of Top Donor-Recipient Flows (Rajat Dashboard)",
            palette = "Blues")
  })
  
  output$network_rajat <- renderPlot({
    df <- rajat_data()
    g <- graph_from_data_frame(df, directed = FALSE)
    ggraph(g, layout = "fr") +
      geom_edge_link() +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      labs(title = "Network Graph of Donor-Recipient Aid Relationships")
  })
  
  output$chord_rajat <- renderPlot({
    circos.clear()
    chordDiagram(rajat_data(), annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1))
    title("Chord Diagram of Top Donor-Recipient Aid")
  })
  
  output$table_rajat <- renderDT({ rajat_data() })
  
  
  ## Shreya Dashboard Outputs
  output$line_total_aid <- renderPlot({
    ggplot(aid_year, aes(x = year, y = total_aid_billion)) +
      geom_line(color = "blue") + geom_point(color = "red") +
      labs(title = "Total Global Aid Commitments Over Time",
           x = "Year",
           y = "Total Aid (Billion USD)")
  })
  
  output$top_donors_bar <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = total_aid_billion, fill = donor)) +
      geom_bar(stat = "identity") +
      labs(title = "Top Donors' Total Aid Per Year",
           x = "Year",
           y = "Aid (Billion USD)")
  })
  
  output$top_donors_grid <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = total_aid_billion, color = donor)) +
      geom_line() +
      facet_wrap(~donor) +
      labs(title = "Aid Trends by Top Donors (Faceted)",
           x = "Year",
           y = "Aid (Billion USD)")
  })
  
  output$bubble_plot <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = donor, size = total_aid_billion)) +
      geom_point(alpha = 0.7) +
      labs(title = "Bubble Plot of Top Donor Aid Across Years",
           x = "Year",
           y = "Donor",
           size = "Aid (Billion USD)")
  })
  
  output$heatmap_over_time <- renderPlot({
    ggplot(df_top_donors, aes(x = year, y = donor, fill = total_aid_billion)) +
      geom_tile() +
      scale_fill_viridis(name = "Aid (Billion USD)") +
      labs(title = "Heatmap of Aid by Donor and Year",
           x = "Year",
           y = "Donor")
  })
  
  output$treemap_static <- renderPlot({
    donor_totals <- df_top_donors %>% group_by(donor) %>% summarise(total_aid_billion = sum(total_aid_billion))
    ggplot(donor_totals, aes(area = total_aid_billion, fill = donor, label = donor)) +
      geom_treemap() +
      geom_treemap_text(color = "white", grow = TRUE) +
      labs(title = "Treemap of Top Donors by Total Aid")
  })
  
  
  ## Premann Outputs
  output$broad_categories_plot <- renderPlot({
    aid_data_broad %>%
      filter(year >= input$years_aid[1], year <= input$years_aid[2]) %>%
      group_by(broad_category) %>%
      summarise(total_aid = sum(commitment_amount_usd_constant_sum)) %>%
      arrange(desc(total_aid)) %>%
      slice_head(n = input$top_n_aid) %>%
      ggplot(aes(x = reorder(broad_category, total_aid), y = total_aid)) +
      geom_bar(stat = "identity", fill = "maroon") +
      coord_flip() +
      scale_y_continuous(labels = label_comma()) +
      labs(title = "Top Aid Categories by Commitment Amount",
           x = "Broad Category",
           y = "Total Aid (USD)")
  })
  
  
  ## Marissa Outputs
  output$question_text <- renderText({ "What are the differences in worldwide aid before and after 2008?" })
  
  output$donors_plot1 <- renderPlot({
    ggplot(df_donor_before) +
      geom_sf(aes(fill = aid_given)) +
      scale_fill_gradient(low = "navy", high = "gold", na.value = "grey70", labels = dollar) +
      labs(title = "Donor Aid Commitments: Before 2008 (2005-2007)",
           fill = "Aid Given")
  })
  
  output$donors_plot2 <- renderPlot({
    ggplot(df_donor_after) +
      geom_sf(aes(fill = aid_given)) +
      scale_fill_gradient(low = "navy", high = "gold", na.value = "grey70", labels = dollar) +
      labs(title = "Donor Aid Commitments: After 2008 (2008-2010)",
           fill = "Aid Given")
  })
  
  output$donors_plot3 <- renderPlot({
    df_selected <- filter(df_donor, donor == input$donors_select)
    ggplot(df_selected, aes(x = forcats::fct_rev(period), group = COUNTRY, y = aid_given)) +
      geom_point() + geom_line() +
      labs(title = "Aid Contributions Before vs After 2008 (Selected Donor)",
           x = "Period",
           y = "Aid Given (USD)") +
      scale_y_continuous(labels = dollar) +
      theme_minimal()
  })
  
  output$recip_plot1 <- renderPlot({
    ggplot(df_recip_before) +
      geom_sf(aes(fill = aid_received)) +
      scale_fill_gradient(low = "navy", high = "gold", na.value = "grey70", labels = dollar) +
      labs(title = "Recipient Aid Received: Before 2008 (2005-2007)",
           fill = "Aid Received")
  })
  
  output$recip_plot2 <- renderPlot({
    ggplot(df_recip_after) +
      geom_sf(aes(fill = aid_received)) +
      scale_fill_gradient(low = "navy", high = "gold", na.value = "grey70", labels = dollar) +
      labs(title = "Recipient Aid Received: After 2008 (2008-2010)",
           fill = "Aid Received")
  })
  
  output$recip_plot3 <- renderPlot({
    df_selected <- filter(df_recip, recipient == input$recip_select)
    ggplot(df_selected, aes(x = forcats::fct_rev(period), group = COUNTRY, y = aid_received)) +
      geom_point() + geom_line() +
      labs(title = "Aid Received Before vs After 2008 (Selected Recipient)",
           x = "Period",
           y = "Aid Received (USD)") +
      scale_y_continuous(labels = dollar) +
      theme_minimal()
  })
}


shinyApp(ui = ui, server = server)

