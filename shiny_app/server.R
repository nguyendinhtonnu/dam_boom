#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


server <- function(input, output) {
  library(tidyverse)
  library(readxl) 
  library(janitor)
  output$scatter_plot <- renderPlot({
      read_excel("raw_data/emdat_public_2020_10_13_query_uid-DXK6pk.xlsx",
                                           skip = 6) %>%
          clean_names() %>%
          select(-disaster_group, -disaster_subsubtype, -disaster_type, -disaster_subgroup, -event_name)%>%
            filter(disaster_subtype %in% "Riverine flood") %>%
            filter(associated_dis %in% "Broken Dam/Burst bank") %>%
            group_by(year) %>%
            mutate(freq = n()) %>%
            ggplot(aes(year, country, size = freq)) +
            geom_point() + 
            theme(axis.text.x = element_text(angle = 90)) + 
            labs(x = "Year", y = "Country", 
                 title = "Frequency of dam breaking or bank bursting riverine flood",
                 subtitle = "From 1990 to 2015",
                 caption = "Source: Emergency Events Database") + 
            theme_light() 
    })
  output$state_message <- renderText({
    paste0("This is the state you chose: ", # this is just a string, so it will never change
           input$selected_state, "!")       # this is based on your input, selected_state defined above.
  })
}
