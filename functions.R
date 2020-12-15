
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

all_dams <- read_rds("shiny_app/all_dams.rds")
all_wbstats <- read_rds("shiny_app/all_wbstats.rds")
joined <- read_rds("shiny_app/joined.rds")
subnational <- read_rds("shiny_app/subnational.rds")
file_path <- read_rds("shiny_app/file_path.rds")
joined <- readRDS("joined.rds")

# Make function to retrieve file path. 

path <- function(country){
  path <- file_path %>% 
    filter(country_name == country) %>%
    select(file_path)
  as.character(path)}
# Make a function that creates a choropleth map. 

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

# Test functions. 

make_population_choro("Zimbabwe") 

# Prepare for basins map 

basins <- readOGR("raw_data/basins/Major_Basins_of_the_World.shp")

gecon_v4 <- read_excel("raw_data/subnational/spatialecon-gecon-v4-xls/Original data/gecon-v4.xls", 
                       sheet = "Sheet1") %>%
  clean_names() 

coordinates(gecon_v4) = c("longitude","lat")
crs.geo1 = CRS("+proj=longlat")
proj4string(gecon_v4) = crs.geo1
proj4string(gecon_v4) <- proj4string(basins)
grid_basins <- gIntersects(gecon_v4,basins,byid = TRUE)

basins@data$average_pop <- apply(grid_basins,1,function(x) mean(gecon_v4@data$popgpw_2005_40[x]))
basins@data$total_pop <- apply(grid_basins,1,function(x) sum(gecon_v4@data$popgpw_2005_40[x]))

qpal = colorBin(c("yellow", "red"), basins@data$total_pop, bins=10)








