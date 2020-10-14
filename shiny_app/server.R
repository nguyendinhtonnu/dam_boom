#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$flood <- renderPlot({

        natural_disasters_asia %>%
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

})
