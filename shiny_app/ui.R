
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
library(gt)
library(gtsummary)

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
           this model seeks to make some predictions using data from 17 countries whose
          shapefiles are found in the A Closer Look tab."), 
        p("These two models seek to predict the relationship between dams, GDP 
          and population. The table shows the results of the stan_glm model,
          and the plot visualizes how these predictions compare with the reality.")
        ),
     fluidRow(
       column(width = 7,
              plotOutput("dam_predictions_1")),
       column(width = 5, 
              tableOutput("table1"),
              p("This model seeks to predict the number of dams based on the population 
                of each country in each year. As the population increases, the model predicts that the number 
                of dams constructed increases as well. Although it fails to predict the 
                period of the most intense increase, the overall trend is correct. As the 
                table shows, the median number of new dam in each country each year is 6.4.
                The population has been divided by 10 million so that stan_glm would work better. 
                The median population for each country each year since 1960 (is a terrible way 
                to put it) is 1.6 million people, which is 0.16 * 10000000. The model 
                predicts that for every increase of 1.6 million in the population, 
                the number of dam would increase by 6.4"))
     ),
     fluidRow(
       column(width = 7,
              plotOutput("dam_predictions_2")),
       column(width = 5, 
              tableOutput("table2"),
              p("This is the same stan_glm model that replaces population with gdp. Here, 
                the median new dam put into operation each year for each country since 1960 
                is 12. The GDP has been divided by 100 billion for ease of calculation.
                The model predicts a negative relationship between Dams and GDP. It 
                predicts that a decrease of 22 billion in total GDP is the median. 
                One way to understand this model is that whenever a decrease in 
                GDP occurs, countries become more tempted to build new dams in 
                order to promote more development."))
       ), 
       fluidRow(
         column(width = 6,
                plotOutput("conclusion1")),
         column(width = 6,
                plotOutput("conclusion2"))
         ),
     fluidRow(
         column(width = 5, 
                h3("Did the hydraulic bargain pay off?"),
                p("These two faceted plots side by side offers a visual comparison
                  of the rate of dam construction and of the growth in GDP. For
                  most of these countries, their dam building peaked before
                  a GDP growth spurt, but the spurt, in most cases, is not 
                  as intense as the the explosion in dams. In the case of 
                  India and Argentina, this is true. For Turkey and Iran, however,
                  an increase in GDP corresponds to increase in dam building. 
                  The only countries where the bargain seems to have paid off
                  are China, and South Africa. The rest barely shows any visible correlation."))
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
         p("This project is part of my wider research in understanding 
           the relationship between people and water in Vietnam.
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

      
                 
                
                 
                 
               