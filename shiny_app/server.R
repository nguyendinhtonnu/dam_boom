###########PREP############
  

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
library(broom.mixed)

# Read in .rds

all_dams <- readRDS("all_dams.rds") %>%
  drop_na(year, reservoir_area_km2)
all_dams_about <- readRDS("all_dams_about.rds") 
all_wbstats <- readRDS("all_wbstats.rds")
file_path <- readRDS("file_path.rds")
joined <- readRDS("joined.rds")
gridded_data <- read_excel("raw_data/subnational/spatialecon-gecon-v4-xls/Original data/gecon-v4.xls", 
                           sheet = "Sheet1") %>%
  clean_names()


# Create options 

dam_years <- as.numeric(unique(all_dams$year)) 
country_names <- as.character(unique(file_path$country_name))
all_countries <- c("All", as.character(unique(all_dams$country)))
subnational <- c("Population", "PPP", "MER")

server <- function(input, output) {
###########FIRST PAGE#############
  
  #Dam numbers by region 
  
  output$world_increase <- renderPlot({
    all_dams_about %>% 
      group_by(region) %>%
      drop_na(completed_operational_since) %>%
      mutate(count = n()) %>%
      ggplot(aes(completed_operational_since, fill = fct_reorder(region, count))) + 
      geom_bar(position = "stack") + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_x_discrete(breaks = c(seq(from = 0, to = 2019, by = 50))) + 
      scale_fill_discrete(name = "Regions") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(title = "Number of dams and reservoirs in operation over time", 
                                          x = "Year", 
                                          y = "Number of dams",
                                          caption = "Source: GrandD v1.3") 
                                        })
  
# Reservoir size plot using resertvoir_size function
  
  source("functions.R")
  output$reservoir <- renderPlot({
    all_dams %>%
      drop_na(reservoir_area_km2) %>%
      filter(country %in% as.character(input$select_country)) %>%
    reservoir_size(., input$dam_years, input$reservoir_size)
  })


  ############SECOND PAGE############
  
  source("functions.R")
output$country_map <- renderLeaflet({
  
  path(input$country_name)
  coordinates(gridded_data) = c("longitude","lat")
 make_map_pop(input$country_name)
 
})

output$dam_country_plot <- renderPlot({
  all_dams %>%
    filter(country == input$country_name) %>%
    ggplot(aes(x = year)) + 
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(title = "Total number of new dams in selected country each year", 
         x = "Year", 
         y = "Number of dams",
         caption = "Source: globaldamwatch.org") +
    theme_classic()
})


############THIRD PAGE############

  # Make model plots

output$dam_predictions_1 <- renderPlot({
  joined_1 <- joined %>%
    filter(country %in% country_names) %>%
    drop_na(pop) %>%
    mutate(pop = as.numeric(pop)) %>%
    
    # divide by 10 million for ease of calculation
    
    mutate(pop = pop/10000000)
  
  dam_model <- stan_glm(data = joined_1, 
                        formula = dams_count ~ pop, 
                        family = gaussian(), 
                        refresh = 0)

  alo_1 <- tibble(truth = joined_1$dams_count, forecast = predict(dam_model), year = joined_1$year)
  
  alo_1 %>%
    pivot_longer(cols = c(truth, forecast),
                 names_to = "source", 
                 values_to = "value") %>%
    ggplot(aes(x = value, y = year, colour = source)) + 
    geom_point(size = 0.3) + 
    coord_flip() + 
    theme_light() 
})

# Make model table

output$table1 <- renderTable({
  joined_1 <- joined %>%
    filter(country %in% country_names) %>%
    drop_na(pop) %>%
    mutate(pop = as.numeric(pop)) %>%
    
    # divide by 10 million for ease of calculation
    
    mutate(pop = pop/10000000)
  
  
  dam_model <- stan_glm(data = joined_1, 
                        formula = dams_count ~ pop, 
                        family = gaussian(), 
                        refresh = 0)
  
  tbl_regression(dam_model, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Yearly Dams Count", 
               subtitle = "The Effect of Population on Dams Count") %>%
    tab_source_note(md("Source: SEDAC GranD"))
  
})

output$dam_predictions_2 <- renderPlot({
  joined_2 <- joined %>%
    filter(country %in% country_names) %>%
    drop_na(gdp) %>%
    mutate(gdp = as.numeric(gdp)) %>%
    
    # gdp divided by 100 billion for ease of calculation
    
    mutate(gdp = gdp/100000000000)
  
  dam_model_2 <- stan_glm(data = joined_2, 
                          formula = dams_count ~ gdp, 
                          family = gaussian(), 
                          refresh = 0)
  
  alo_2 <- tibble(truth = joined_2$dams_count, forecast = predict(dam_model_2), year = joined_2$year)
  
  alo_2 %>%
    pivot_longer(cols = c(truth, forecast),
                 names_to = "source", 
                 values_to = "value") %>%
    ggplot(aes(x = value, y = year, colour = source)) + 
    geom_point(size = 0.3) + 
    coord_flip() + 
    theme_light() 
})

output$table2 <- renderTable({
  joined_2 <- joined %>%
    filter(country %in% country_names) %>%
    drop_na(gdp) %>%
    mutate(gdp = as.numeric(gdp)) %>%
    
    # gdp divided by 100 billion for ease of calculation
    
    mutate(gdp = gdp/100000000000)
  
  dam_model_2 <- stan_glm(data = joined_2, 
                          formula = dams_count ~ gdp, 
                          family = gaussian(), 
                          refresh = 0)
  
  tbl_regression(dam_model_2, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Yearly New Dams Count", 
               subtitle = "The Effect of GDP on Dams Count") %>%
    tab_source_note(md("Source: SEDAC GranD"))
  
})


output$conclusion1 <- renderPlot({
  joined %>%
    filter(country %in% country_names) %>%
    drop_na(gdp) %>%
    mutate(gdp = as.numeric(gdp)) %>%
    ggplot(aes(year, gdp)) + 
    geom_point(size = 0.1) + 
    facet_wrap ( ~ country) + 
    geom_line(colour = "blue") + 
    theme_light()
})

output$conclusion2 <- renderPlot({
  joined %>%
    filter(country %in% country_names) %>%
    filter(year > 1960) %>%
    ggplot(aes(year, dams_count)) + 
    geom_point(size = 0.1) + 
    facet_wrap ( ~ country) + 
    geom_line(colour = "blue") + 
    theme_light()
})

}


  
  