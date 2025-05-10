# app.R

library(shiny)
library(tidyverse)
library(stringr)
library(scales)

# Load and clean data
aid_data <- read_csv("AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv") %>%
  filter(year != 9999) %>%
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

# UI
ui <- fluidPage(
  titlePanel("Aid Distribution by Broad Category"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Select Year Range:",
                  min = min(aid_data$year, na.rm = TRUE),
                  max = max(aid_data$year, na.rm = TRUE),
                  value = c(min(aid_data$year, na.rm = TRUE), max(aid_data$year, na.rm = TRUE)),
                  sep = ""),
      numericInput("top_n", "Number of Top Categories to Show:", value = 10, min = 1)
    ),
    
    mainPanel(
      plotOutput("aidPlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  output$aidPlot <- renderPlot({
    aid_data %>%
      filter(year >= input$years[1], year <= input$years[2]) %>%
      group_by(broad_category) %>%
      summarise(total_aid = sum(commitment_amount_usd_constant_sum, na.rm = TRUE)) %>%
      arrange(desc(total_aid)) %>%
      slice_head(n = input$top_n) %>%
      ggplot(aes(x = reorder(broad_category, total_aid), y = total_aid)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(title = "Total Aid by Broad Category",
           x = "Category",
           y = "USD Committed") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
