
  # Where I save and test my map functions. 

#Libraries

library(sf)
library(tidyverse)
library(dplyr)
library(leaflet)
library(ggplot2)
library(wbstats)
library(janitor)
library(sp)
library(rgdal)
library(rgeos)
library(grid)
library(readxl)
library(maptools)

  #Useful .rds

all_dams <- read_rds("all_dams.rds")
all_wbstats <- read_rds("all_wbstats.rds")
joined <- read_rds("joined.rds")
subnational <- read_rds("subnational.rds")
file_path <- read_rds("file_path.rds")
gridded_data <- read_rds("gridded_data.rds")

# Make function to retrieve file path. 

path <- function(country){
  
  path <- file_path %>% 
    filter(country_name %in% country) %>%
    select(file_path)
  as.character(path)}

path("World Basins")

# Make a function that creates a choropleth map. 

coordinates(gridded_data) = c("longitude","lat")

make_map_pop <- function(country_name){
  
  country_name <- ifelse(country_name == "World Basins", 
                         all_dams$country, 
                         country_name)
  
  country_dams <- all_dams %>%
    drop_na(decimal_degree_latitude) %>%
    filter(country %in% country_name)
  
  mymap <- readOGR(dsn = path(country_name))
  
  crs.geo1 = CRS("+proj=longlat")
  proj4string(gridded_data) = crs.geo1
  proj4string(gridded_data) <- proj4string(mymap)
  grid_mymap <- gIntersects(gridded_data,mymap,byid = TRUE)
  
  mymap@data$total_pop <- apply(grid_mymap,1,function(x) sum(gridded_data@data$popgpw_2005_40[x]))

  pal <- colorNumeric(palette = "viridis", domain = mymap@data$total_pop, reverse = TRUE)
  
  leaflet(mymap) %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~pal(total_pop))   %>%
    addLegend(values=~total_pop,pal=pal, title = "Population") %>%
    addCircleMarkers(data = country_dams, lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
  #add pop_up info
}

make_map_ppp <- function(country_name){
  
  country_name <- ifelse(country_name == "World Basins", 
                         all_dams$country, 
                         country_name)
  
  country_dams <- all_dams %>%
    drop_na(decimal_degree_latitude) %>%
    filter(country %in% country_name)
  
  mymap <- readOGR(dsn = path(country_name))
  
  crs.geo1 = CRS("+proj=longlat")
  proj4string(gridded_data) = crs.geo1
  proj4string(gridded_data) <- proj4string(mymap)
  grid_mymap <- gIntersects(gridded_data,mymap,byid = TRUE)
  
  mymap@data$total_ppp <- apply(grid_mymap,1,function(x) mean(gridded_data@data$ppp2005_40[x]))
  
  pal <- colorNumeric(palette = "viridis", domain = mymap@data$total_ppp, reverse = TRUE)
  
  leaflet(mymap) %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~pal(total_ppp))   %>%
    addLegend(values=~total_ppp,pal=pal, title = "PPP") %>%
    addCircleMarkers(data = country_dams, lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
  #add pop_up info
}

make_map_mer <- function(country_name){
  
  country_name <- ifelse(country_name == "World Basins", 
                         all_dams$country, 
                         country_name)
  
  country_dams <- all_dams %>%
    drop_na(decimal_degree_latitude) %>%
    filter(country %in% country_name)
  
  mymap <- readOGR(dsn = path(country_name))
  
  crs.geo1 = CRS("+proj=longlat")
  proj4string(gridded_data) = crs.geo1
  proj4string(gridded_data) <- proj4string(mymap)
  grid_mymap <- gIntersects(gridded_data,mymap,byid = TRUE)
  
  mymap@data$total_mer <- apply(grid_mymap,1,function(x) mean(gridded_data@data$mer2005_40[x]))
  
  pal <- colorNumeric(palette = "viridis", domain = mymap@data$total_mer, reverse = TRUE)
  
  leaflet(mymap) %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~pal(total_mer))   %>%
    addLegend(values=~total_mer,pal=pal,title = "MER") %>%
    addCircleMarkers(data = country_dams, lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
  #add pop_up info
}


make_map_pop("World Basins")

# Test functions. 

# Make function for reservoir size plot.

reservoir_size <- function(data, year, size){
  
  years <- c(0:as.numeric(year))
  sizes <- c(as.numeric(size)[1]:as.numeric(size)[2])
  
 data %>%
    group_by(year) %>%
    drop_na(reservoir_area_km2) %>%
    filter(year %in% year) %>%
    filter(reservoir_area_km2 %in% sizes) %>%
    ggplot(aes(reservoir_area_km2)) + 
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 150) + 
    labs(y = "Proportion", 
         x = "Reservoir Area in km2") +
    theme_light()
}

#############PARKING LOT##################
 
 #This is a function that ends up being usable, but inaccurate. 
 
 make_population_choro <- function(country_name){
   country_dams <- all_dams %>%
     drop_na(decimal_degree_latitude) %>%
     filter(country == country_name)  
   
   admin_list <- as.tibble(subnational %>%
                             filter(country_name == country_name) %>%
                             select(admin_name, population) %>%
                             rename(admin_new = admin_name) %>%
                             mutate(count = c(1:nrow(.))))
   
   mymap <- readOGR(dsn = path(country_name))
   
   mymap@data <- mymap@data %>% 
     clean_names()
   
   mymap@data <- mymap@data %>%
     mutate(count = c(1:nrow(.))) %>%
     left_join(admin_list, by = "count") %>%
     mutate(admin_name = admin_new) %>%
     select(-admin_new, -count)
   
   pal <- colorNumeric(palette = "viridis", domain = mymap@data$population, reverse = TRUE)
   
   leaflet(mymap) %>%
     addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~pal(population))   %>%
     addLegend(values=~population,pal=pal,title="Population") %>%
     addCircleMarkers(data = country_dams, lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
   #add pop_up info
 }

# This is a leaflet I ended up removing 

# Basins dams count. 

source("functions.R")
output$basin_map <- renderLeaflet({
  
  all_dams_new <- all_dams %>%
    group_by(major_basin) %>%
    mutate(dams_count = n()) %>%
    drop_na(decimal_degree_latitude) 
  
  basins <- readOGR(path("World Basins"))
  
  coordinates(all_dams_new) = c("decimal_degree_longitude","decimal_degree_latitude")
  crs.geo1 = CRS("+proj=longlat")
  proj4string(all_dams_new) = crs.geo1
  
  basin_agg = aggregate(x=all_dams_new["dams_count"],by = basins, FUN = length)
  
  qpal = colorBin("Reds", basin_agg$dams_count, bins=6)
  
  # create leaflet that shades basin by the number of dams that's in it, based on coordinates.
  
  leaflet(basin_agg) %>%
    addTiles() %>%
    addPolygons(stroke = TRUE,opacity = 0.5,fillOpacity = 0.5, smoothFactor = 0.5, weight = 1, fillColor = ~qpal(dams_count)) %>%
    addLegend(values=~dams_count,pal=qpal,title="Number of Dams and Reservoirs") %>% 
    addCircleMarkers(data = all_dams, 
                     lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1)
})

