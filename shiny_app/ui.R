

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

all_dams <- readRDS("all_dams.rds")
all_dams_about <- readRDS("all_dams_about.rds") 
all_wbstats <- readRDS("all_wbstats.rds")
file_path <- readRDS("file_path.rds")

shinyUI(
    navbarPage(
      theme = shinytheme("superhero"),
      "Working Title",
      
      ##################################
      
      ############FIRST PAGE############
               tabPanel(
                 "Introduction",
                 h2("Heading"),
                 p("Text"),
      # 2 columns
      
      fluidPage(
        fluidRow(style = 'padding:30px;',
                 column(7,
                        
                        # Histogram of dams
                        
                        plotOutput("world_incrase")),
                 column(
                   5,
                   h3("Heading"),
                   p("Text")
                 )),
        fluidRow(column(
          4,
          h3("Heading"),
          p(
            "Text"
          )
        ),
        column(8,
               # Plot Leaflet map.
               
               leafletOutput("basin_map", height = 700)))
      )
               )))
      
                 
                
                 
                 
               