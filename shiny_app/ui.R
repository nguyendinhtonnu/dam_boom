
  # Load libraries

library(shiny)
library(shinythemes)
library(tidyverse)
library(purrr)
library(dplyr)
library(janitor)
library(readxl)
library(maptools)
library(leaflet)
library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(grid)
library(wbstats)
library(rstanarm)

  # Read in .rds

all_dams <- readRDS("all_dams.rds") %>%
  drop_na(year, reservoir_area_km2)
all_dams_about <- readRDS("all_dams_about.rds") 
all_wbstats <- readRDS("all_wbstats.rds")
file_path <- readRDS("file_path.rds")
joined <- readRDS("joined.rds")
gridded_data <- readRDS("gridded_data.rds")

  # Create options 

dam_years <- as.numeric(unique(all_dams$year)) 
country_names <- as.character(unique(file_path$country_name))
all_countries <- c("All", as.character(unique(all_dams$country)))
subnational <- c("Population", "PPP", "MER")
  
shinyUI(
    navbarPage(
      theme = shinytheme("superhero"),
      "Dams Data",

      ##################################
      
      ############FIRST PAGE############
               tabPanel(
                 "Introduction",
                 fluidPage(
                   h2("The world's most vital resource"),
                   p("Since antiquity, human beings have been interested in controlling their 
                     water resrouces. Over the course of thousands of years, different civilizations 
                     developed different ways of storing, stopping, or moving water, depending on their needs. 
                     Even at its most rudimentary, technologies of water control tend to be sophisicated and 
                     requires a lot of overhead investment."),
                   fluidRow(
                     column(width = 5, 
                            h3("An explosion in number"),
                            p("In the 20th century, for the first time in world history, people
                            were building the same kind of hydraulic infrastructure everywhere, and in greater
                            numbers than ever before. Starting in the 50s, the number of new dams and 
                              reservoirs put into operation skyrocketed. Asian nations took the lead in this 
                              trend.")),
                    column(width = 7,
                    plotOutput("world_increase"))
                          ),
                   fluidRow(
                     column(width = 5, 
                            h3("The smaller picture"),
                            p("Big dams and reservoirs are often in the news for the detrimental effects it
                              brings onto the people and the environment of a place. Most reservoirs are
                              less than 3 square kilometers in size, though the biggest reservoirs 
                              is over 50,000 square kilometers. Explore this interactive plot 
                              to learn more about dam numbers by year and reservoir size."), 
                            sliderInput(
                              "dam_years",
                              "Time Slider",
                              min = min(dam_years),
                              max = max(dam_years),
                              value = 1960,
                              sep = NULL,
                              step = 1),
                            sliderInput(
                              "reservoir_size",
                              "Size Slider",
                              min = min(all_dams$reservoir_area_km2),
                              max = max(all_dams$reservoir_area_km2),
                             value = c(50, 100), 
                              sep = NULL,
                              step = 1),
                            selectInput(
                              "select_country",
                              "Select country",
                              choices = all_countries, 
                              multiple = TRUE, 
                              selected = "India"
                            )
                           ),
                     column(width = 7,
                            plotOutput("reservoir"))
                            )
                   )
                   ),
 ########################################
 
 ###############SECOND PAGE##############
      tabPanel(
        "A Closer Look",
        fluidPage(
          fluidRow(
            column(
              width = 2, 
              h3("Distribution of dams in each country"),
              p("Explore the interactive maps below to learn more about the distribution of dams"),
              selectInput(
                inputId = "country_name",                 # a name for the value you choose here
                label = "Choose a country",   # the name to display on the slider
                choices = country_names, 
                selected = "India"),
              selectInput(
                inputId = "map_choice",                 # a name for the value you choose here
                label = "Choose the kind of map you want to see",   # the name to display on the slider
                choices = subnational, 
                selected = "Population")
                       ),
            column(width = 10,
                   leafletOutput("country_map", height = 700),
                   plotOutput("dam_country_plot"))
            )
          )
      ),
 ##########################################
 
 ##############THIRD PAGE##################
 tabPanel(
   "Predicting Dams",
   fluidPage(
     fluidRow(
         h3("Modelling dams count based on GDP and population"),
         leafletOutput("basin_map"),
        p("It's clear that you can't build a dam on dry land; the bare necessity is water. However, 
           not all river basins have the same number of dams. What socio-economic factors have an influence
           on the distribution of dams? Using population, GDP, agricultural land, and water availability data, 
           this model seeks to make some predictions.")),
     fluidRow(
       column(width = 7,
              plotOutput("dam_predictions_1")),
       column(width = 5, 
              p("This model seeks to predict the number of dams based on the population 
                of each country in each year. As you can see, it doesn't do a very good job
                of predicting. This is probably because the model is too simple: population
                is not a good predictor for dams count. A better predictor would probably 
                be GDP?"))
     ),
     fluidRow(
       column(width = 7,
              plotOutput("dam_predictions_2")),
       column(width = 5, 
              p("It's still bizarre, but seems to be better than the last one. As we can see, the truth is
                very random, and the model is not good at seeing the pattern that the surge
                came at a particular point in time, and wasn't going to repeat itself."))
     )
   )
 ),
 ############################################
 
 ###############ABOUT PAGE###################
 tabPanel(
   "About",
   fluidPage(
     fluidRow(
       column(
         width = 5, 
         h3("Background"),
         p("Every monsoon season, the central part of Vietnam suffers from heavy flooding which 
         seems to increase in intensity every year. Critics of the government have cited 
           the construction of dams up river to be a major culprit, as it changes the 
           flow regime of rivers. This project is part of my wider research in understanding 
           the relationship between people and water in Vietnam. \n
           This project is mapping heavy, because I believe that a map is the superior illustration 
           and learning tool. The mapping of dams and reservoirs draws on the work of [SEDAC's 
           GRanD project](https://sedac.ciesin.columbia.edu/data/collection/grand-v1), 
           which maps large dams and reservoirs around the world. The population data used for 
           choropleth maps are taken from the [G-Econ project at Yale University]
           (https://gecon.yale.edu/data-and-documentation-g-econ-project), 
           which maps world economic data on a global grid. The shapefiles are taken from 
           [IPUMS International](https://international.ipums.org/international/gis_harmonized_1st.shtml), 
           which specializes in census data. This project hasn't been able to make full use of the data 
           taken from these sources, but I think it's been great putting them to work in one place."),
         h3("The Data"),
         p("This project's data comes from: 
         - World Bank (accessed using wbstats) 
         - EM-DAT: https://www.emdat.be/
         - SEDAC: (https://sedac.ciesin.columbia.edu/data/collection/grand-v1)
         - G-Econ Project at Yale: https://gecon.yale.edu/
         - IPUMS International: https://international.ipums.org/"),
         h3("Acknowledgements"),
         p("I'd like to thank Wyatt Hurt for his help and for being an inspiration for this project, and 
           to all the course staff and all my classmates in GOV 50 for creating such a wonderful and 
           supportive environment."),
         h3("About me"),
         p("I am a junior studying East Asian History at Harvard College. Currently, I'm working on my 
           research project on the environmental history of the Red River delta. 
           I can be reached at nguyendinh_tonnu@college.harvard.edu. The project's github
           repo lives [here](https://github.com/nguyendinhtonnu/final_project)")
       )
     )
   )
 )
 #################END######################
      ))

      
                 
                
                 
                 
               