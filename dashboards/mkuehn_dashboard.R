library(shiny)
library(tidyverse)
library(sf)

# Read in data
data <- read.csv("AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv")
world_map <- read_sf("World_Countries_(Generalized)_9029012925078512962.geojson")

# Aggregate the data
aid_agg <- data %>% 
  group_by(year,donor,recipient) %>% 
  summarize(total_commitment = sum(commitment_amount_usd_constant_sum))

# Donors
aid_beforeafter_donor <- aid_agg %>% filter(year>=2005) %>% 
  mutate(period=ifelse(year<2008,'Before','After')) %>% 
  group_by(donor,period) %>% 
  summarize(aid_given=sum(total_commitment)) %>% 
  arrange(desc(period)) %>% 
  mutate(COUNTRY=donor)

donor_beforeafter_joined <- world_map %>% 
  left_join(aid_beforeafter_donor)

df_donor_before <- donor_beforeafter_joined %>% filter(is.na(period)|period=="Before")
df_donor_after <- donor_beforeafter_joined %>% filter(is.na(period)|period=="After")

df_donor <- filter(donor_beforeafter_joined,!is.na(aid_given))

# Recipients
aid_beforeafter_recipient <- aid_agg %>% filter(year>=2005) %>% 
  mutate(period=ifelse(year<2008,'Before','After')) %>% 
  group_by(recipient,period) %>% 
  summarize(aid_received=sum(total_commitment)) %>% 
  arrange(desc(period)) %>% 
  mutate(COUNTRY=recipient)

recipient_beforeafter_joined <- world_map %>% 
  left_join(aid_beforeafter_recipient)

df_recip_before <- recipient_beforeafter_joined %>% filter(is.na(period)|period=="Before")
df_recip_after <- recipient_beforeafter_joined %>% filter(is.na(period)|period=="After")

df_recip <- filter(recipient_beforeafter_joined,!is.na(aid_received))

ui <- fluidPage(
  
  textOutput(outputId = "question_text"),
  plotOutput(outputId = "donors_plot1"),
  plotOutput(outputId = "donors_plot2"),
  selectInput(inputId = "donors_select",label="Select a donor country",choices=df_donor$donor),
  plotOutput(outputId = "donors_plot3"),
  plotOutput(outputId = "recip_plot1"),
  plotOutput(outputId = "recip_plot2"),
  selectInput(inputId = "recip_select",label="Select a recipient country",choices=df_recip$recipient),
  plotOutput(outputId = "recip_plot3"),
)

server <-  function(input, output){
  
  # Stating the question
  output$question_text <- renderText("What are the differences in worldwide aid before and after 2008?")
  
  # Donors
  p_before <- ggplot(df_donor_before) +
    geom_sf(aes(fill=aid_given)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Commitments (2005-2007)",fill="Aid Commitment")
  
  p_after <- ggplot(df_donor_after) +
    geom_sf(aes(fill=aid_given)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Commitments (2008-2010)",fill="Aid Commitment")
  
  output$donors_plot1 <- renderPlot(p_before)
  output$donors_plot2 <- renderPlot(p_after)
  
  output$donors_plot3 <- renderPlot({
    df_selected <- filter(df_donor, donor==input$donors_select)
    print(df_selected)
    ggplot(df_selected,aes(x=forcats::fct_rev(period),group=COUNTRY,y=aid_given)) +
      geom_point()+
      geom_line()+
      scale_y_continuous(labels=scales::dollar)+
      labs(y="Aid Contribution (USD2009)",x="Time Period",title="Donor Aid Commitment (2005-2010)",subtitle="Before and After 2008")+
      theme_minimal()
    })
  
  # Recipients
  p_before <- ggplot(df_recip_before) +
    geom_sf(aes(fill=aid_received)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Received (2005-2007)",fill="Aid Received")
  
  p_after <- ggplot(df_recip_after) +
    geom_sf(aes(fill=aid_received)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Received (2008-2010)",fill="Aid Received")
  
  output$recip_plot1 <- renderPlot(p_before)
  output$recip_plot2 <- renderPlot(p_after)
  
  output$recip_plot3 <- renderPlot({
    df_selected <- filter(df_recip, recipient==input$recip_select)
    ggplot(df_selected,aes(x=forcats::fct_rev(period),group=COUNTRY,y=aid_received)) +
      geom_point()+
      geom_line()+
      scale_y_continuous(labels=scales::dollar)+
      labs(y="Aid Recieved (USD2009)",x="Time Period",title="Aid Recieved (2005-2010)",subtitle="Before and After 2008")+
      theme_minimal()
  })
}

shinyApp(ui, server)

