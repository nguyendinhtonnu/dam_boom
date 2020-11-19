#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


server <- function(input, output) {
  
  #read data --> data at bottom 
  
  library(janitor)
  library(readxl)
  library(readr)
  
  #Disasters worldwide
  
  em_dat <- read_excel("raw_data/emdat_public_2020_11_01_query_uid-MSWGVQ.xlsx",
                       skip = 6) %>%
    clean_names()
  
  #aquastat dams stuff
  
  all_dams <- readRDS("raw_data/all_dams.rds")
  
  all_dams %>%
    group_by(region) %>%
    mutate(dam_numbers_region = n()) %>%
    ungroup () %>%
    group_by(ISO3) %>%
    mutate(dam_numbers_country = n()) %>%
    ungroup()
  
#       ***LOAD LIBRARIES***
  
  #tidyverse
  
  library(tidyverse)
  
  #maps
  
  library(maptools)
  data(wrld_simpl)
  library(leaflet)
  
  #do all the cleaning stuff in the RMD 
  
  
#     ***MAKE PLOTS***
  
  #Dam numbers by region and by country
  
  output$dam_region_plot <- renderPlot({
    all_dams %>%
     filter(region == input$selected_region) %>%
                                     ggplot(dam_numbers, aes(country, dam_numbers_country)) + 
                                     geom_col() +
                                     theme(axis.text.x = element_text(angle = 90)) + 
                                     labs(title = "Total number of dams by region", 
                                          x = "Country", 
                                          y = "Number of dams",
                                          caption = "Source: globaldamwatch.org") +
                                     theme_classic()
                                        })
  
  
  output$river_plot <- renderPlot({
    

    #Make plots
    
    em_dat %>%
            filter(disaster_subtype %in% "Riverine flood") %>%
            filter(year == input$selected_year) %>%
            ggplot(aes(country)) +
            geom_bar() + 
            theme(axis.text.x = element_text(angle = 90)) + 
            labs(y = "Count", x = "Country", 
                 title = "Frequency of dam breaking or bank bursting riverine flood",
                 subtitle = "From 1990 to 2015",
                 caption = "Source: Emergency Events Database") + 
            theme_light() 
    })
  
  output$year_message <- renderText({
    paste0("This is the year you choose: ", # this is just a string, so it will never change
           input$selected_year, "!")       # this is based on your input, selected_state defined above.
  })
  
  output$disaster_type_plot <- renderPlot({
  em_dat %>%
      filter(disaster_type == input$selected_type) %>% 
                  group_by(year) %>%
                  mutate(count = n()) %>%
                  filter(count >= 1) %>%
                  ggplot(aes(year, count, group = 1)) +
                  geom_line(colour = "powderblue") +
                  geom_point(size = 0.1) +
                  labs(y = "Count", x = "Year", 
                       title = "Frequency of Recorded Natural Disaster",
                       subtitle = "From 1900 to 2020",
                       caption = "Source: Emergency Events Database") + 
                  scale_x_discrete(breaks = seq(from = 1900, to = 2020, by = 20)) + 
                  theme_light()
  })
  
  output$type_message <- renderText({
    paste0("This is the disaster type you choose: ", # this is just a string, so it will never change
           input$selected_type, "!")       # this is based on your input, selected_state defined above.
  })
  
}

#   ***READ DATA***

#Dams worldwide:

