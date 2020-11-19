#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#choices

years <- c(1990:2015)
regions <- c("All", "Middle East", "South America", "South East Asia", "Oceania", 
             "North America", "Europe", "Central America", "Central Asia", "Africa",
             "East Asia", "South Asia")

disaster_types <- c("Drought", "Earthquake", "Volcanic activity", "Storm", "Flood", "Epidemic", "Landslide", "Wildfire", 
                    "Extreme temperature", "Insect infestation")



# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
      "Dams and Development",
               tabPanel(
                   "About",
                   h3("The questionable effectiveness of large dams"),
                   p("In the latter half of the 20th century, the world saw an 
                     explosion in the number of dams being constructed and put to use. 
                     In general, a dam is a wall that obstructs a river made of 
                     durable material. It creates a reservoir for flood control, 
                     hydropower, irrigation, transportation, or even 
                     tourism development. "),
                  p("Though the benefits are obvious, what is often obscured is 
                    who's being affected by these constructions, and who's benefiting
                    from it. Reservoirs, especially those built on indigenous land, 
                    innunadate valuable agricultural land and heritage sites, 
                    forcing people to relocate to places with less favourable 
                    conditions, as well as fragmenting their existent social structure.
                    Dams are also a disruption in the ecosystem where it's placed.
                    Those who benefit from the energy production or the irrigation may 
                    never even see the effects that a dam has on the people and the
                    environment."),
                   p("[purpose] "),
                   p("https://github.com/nguyendinhtonnu/final_project"),
                  ),
               # 
               tabPanel(
                 "Data",
                 sidebarLayout(
                   sidebarPanel(
                     "Riverine floods by country",
                                p("Filtering data from EM-DAT database to 
                                  only show riverine floods in countries 
                                  in the word from 1990 to 2015."),
                                selectInput(
                                  inputId = "selected_year",                 # a name for the value you choose here
                                  label = "Choose a year",   # the name to display on the slider
                                  choices = years)
                                ),
                   mainPanel(
                     plotOutput(outputId = "river_plot"),
                     textOutput("year_message")
                            )
                              ),
                 sidebarLayout(
                   sidebarPanel(
                     "Total number of recorded disasters",
                     p("All disasters recorded in EM-DAT from 1900 to 2020"),
                     selectInput(
                       inputId = "selected_type",                 # a name for the value you choose here
                       label = "Choose a disaster type",   # the name to display on the slider
                       choices = disaster_types, 
                       selected = disaster_types, 
                       multiple = TRUE)
                   ),
                   mainPanel(
                     plotOutput(outputId = "disaster_type_plot"),
                     textOutput("type_message")
                   )
                 )
                        ),
      tabPanel(
        "Dams",
        sidebarLayout(
          sidebarPanel(
            "Total number of recorded disasters",
            p("All disasters recorded in EM-DAT from 1900 to 2020"),
            selectInput(
              inputId = "selected_region",                 # a name for the value you choose here
              label = "Choose a disaster type",   # the name to display on the slider
              choices = regions)
          ),
          mainPanel(
            plotOutput(outputId = "dam_region_plot")
            #textOutput("type_message")
          )
        )
                )
    ))

