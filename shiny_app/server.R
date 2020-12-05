###########PREP############
  
  # Load libraries.
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

  # Read in data. 

all_dams <- readRDS("all_dams.rds")
all_dams_about <- readRDS("all_dams_about.rds") 
all_wbstats <- readRDS("all_wbstats.rds")
file_path <- readRDS("file_path.rds")

server <- function(input, output) {
###########FIRST PAGE#############
  
  #Dam numbers by region and by country
  
  output$world_increase <- renderPlot({
    all_dams_about %>% 
      group_by(region) %>%
      drop_na(completed_operational_since) %>%
      mutate(count = n()) %>%
      ggplot(aes(completed_operational_since, fill = fct_reorder(region, count))) + 
      geom_bar(position = "stack") + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_x_discrete(breaks = c(seq(from = 0, to = 2019, by = 50))) + 
      theme_light() +
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(title = "Number of dams and reservoirs in operation by each year", 
                                          x = "Year", 
                                          y = "Number of dams",
                                          caption = "Source: GrandD v1.3") 
                                        })
  
  output$basin_map <- renderLeaflet({
    all_dams_new <- all_dams %>%
      group_by(major_basin) %>%
      mutate(dams_count = n()) %>%
      drop_na(decimal_degree_latitude) 
    
    coordinates(all_dams_new) = c("decimal_degree_longitude","decimal_degree_latitude")
    crs.geo1 = CRS("+proj=longlat")
    proj4string(all_dams_new) = crs.geo1
    
    basin_agg = aggregate(x=all_dams_new["dams_count"],by = basins, FUN = length)
    
    qpal = colorBin("Reds", basin_agg$dams_count, bins=6)
    
    
    leaflet(basin_agg) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE,opacity = 0.5,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~qpal(dams_count)) %>%
      addLegend(values=~dams_count,pal=qpal,title="Number of Dams and Reservoirs") %>% 
      addCircleMarkers(data = all_dams %>%
                         drop_na(decimal_degree_latitude, decimal_degree_longitude), 
                       lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
  })
}
  
  
  