#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


server <- function(input, output) {
  
#       ***LOAD LIBRARIES***
  
  #tidyverse
  
  library(tidyverse)
  
  #read data --> data at bottom 
  
  library(janitor)
  library(readxl)
  
  #maps
  
  library(maptools)
  data(wrld_simpl)
  library(leaflet)
  
#     ***MAKE PLOTS***
  
  #Dam numbers by region and by country
  
  output$dam_region_plot <- renderPlot({
    case_when(
      input$selected_region == "All" ~ ggplot(all_dams, aes(region, dam_numbers_region)) + 
                                       geom_col() +
                                      theme(axis.text.x = element_text(angle = 90)) + 
                                      labs(title = "Total number of dams by region", 
                                           x = "Region", 
                                           y = "Number of dams",
                                           caption = "Source: globaldamwatch.org")
                                      theme_classic(),
      input$selected_region != "All" ~ filter(region == input$selected_region) %>%
                                     ggplot(dam_numbers, aes(region, dam_numbers_region)) + 
                                     geom_col() +
                                     theme(axis.text.x = element_text(angle = 90)) + 
                                     labs(title = "Total number of dams by region", 
                                          x = "Region", 
                                          y = "Number of dams",
                                          caption = "Source: globaldamwatch.org") +
                                     theme_classic())
                                        })
  
  
  output$river_plot <- renderPlot({
    em_dat %>%
            filter(disaster_subtype %in% "Riverine flood") %>%
            filter(year == input$selected_year) %>%
            ggplot(aes(country)) +
            geom_bar() + 
            theme(axis.text.x = element_text(angle = 90)) + 
            labs(y = "Count", x = "Country", 
                 title = "Frequency of dam breaking or bank bursting riverine flood",
                 subtitle = "From 1990 to 2015",
                 caption = "Source: Emergency Events Database") + 
            theme_light() 
    })
  
  output$year_message <- renderText({
    paste0("This is the year you choose: ", # this is just a string, so it will never change
           input$selected_year, "!")       # this is based on your input, selected_state defined above.
  })
  
  output$disaster_type_plot <- renderPlot({
  em_dat %>%
      case_when(input$selected_type != "All" ~ filter(disaster_type == input$selected_type) %>% 
                  group_by(year) %>%
                  mutate(count = n()) %>%
                  filter(count >= 1) %>%
                  ggplot(aes(year, count, group = 1)) +
                  geom_line(colour = "powderblue") +
                  geom_point(size = 0.1) +
                  labs(y = "Count", x = "Year", 
                       title = "Frequency of Recorded Natural Disaster",
                       subtitle = "From 1900 to 2020",
                       caption = "Source: Emergency Events Database") + 
                  scale_x_discrete(breaks = seq(from = 1900, to = 2020, by = 20)) + 
                  theme_light(),
                  input$selected_type == "All" ~ group_by(year) %>%
                    mutate(count = n()) %>%
                    filter(count >= 1) %>%
                    ggplot(aes(year, count, group = 1)) +
                    geom_line(colour = "powderblue") +
                    geom_point(size = 0.1) +
                    labs(y = "Count", x = "Year", 
                         title = "Frequency of Recorded Natural Disaster",
                         subtitle = "From 1900 to 2020",
                         caption = "Source: Emergency Events Database") + 
                    scale_x_discrete(breaks = seq(from = 1900, to = 2020, by = 20)) + 
                    theme_light())
  })
  
  output$type_message <- renderText({
    paste0("This is the disaster type you choose: ", # this is just a string, so it will never change
           input$selected_type, "!")       # this is based on your input, selected_state defined above.
  })
  
}

#   ***READ DATA***

#Dams worldwide:

africa_aquastat <- read_excel("shiny_app/raw_data/dams/africa_aquastat.xlsx", sheet = "Dams", skip = 1, col_types = c("text", 
                                                                                                                      "text", "text", "text", "text", "text", 
                                                                                                                      "text", "text", "text", "text", "numeric", 
                                                                                                                      "numeric", "numeric", "numeric", 
                                                                                                                      "text", "text", "text", "text", "text", 
                                                                                                                      "text", "text", "text", "text", "numeric", 
                                                                                                                      "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Africa")

casia_aquastat <- read_excel("shiny_app/raw_data/dams/casia_aquastat.xlsx", sheet = "Dams", skip = 1, col_types = c("text", 
                                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                                    "text", "text", "text", "text", "numeric", 
                                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                                    "text", "text", "text", "text", "text", 
                                                                                                                    "text", "text", "text", "text", "numeric", 
                                                                                                                    "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Central Asia")

cenamer_aquastat <- read_excel("shiny_app/raw_data/dams/cenamer_aquastat.xlsx", sheet = "Dams", skip = 1,
                               col_types = c("text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "numeric", 
                                             "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Central America")

europe_aquastat <- read_excel("shiny_app/raw_data/dams/europe_aquastat.xlsx", sheet = "Dams", skip = 1,
                              col_types = c("text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Europe")

namer_aquastat <- read_excel("shiny_app/raw_data/dams/namer_aquastat.xlsx", sheet = "Dams", skip = 1, 
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "North America")

oceania_aquastat <- read_excel("shiny_app/raw_data/dams/oceania_aquastat.xlsx", sheet = "Dams", skip = 1) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Oceania")

easia_aquastat <- read_excel("shiny_app/raw_data/dams/seasia_aquastat.xlsx", sheet = "Dams", 
                             skip = 1,
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3)  %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  filter(country %in% c("China", "Japan", "Korea", "Democratic People's Republic of Korea", "Republic of Korea")) %>%
  mutate(region = "East Asia") 

sasia_aquastat <- read_excel("shiny_app/raw_data/dams/seasia_aquastat.xlsx", sheet = "Dams", 
                             skip = 1,
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "numeric", 
                                           "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3)  %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  filter(country %in% c("India", "Bangladesh", "Pakistan", "Sri Lanka", "Bhutan", "Nepal")) %>%
  mutate(region = "South Asia") 

seasia_aquastat <- read_excel("shiny_app/raw_data/dams/seasia_aquastat.xlsx", sheet = "Dams", 
                              skip = 1,
                              col_types = c("text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3)  %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  filter(!country %in% c("China", "Japan", "Korea", "Democratic People's Republic of Korea", "Republic of Korea", "India", "Bangladesh", "Pakistan", "Sri Lanka", "Bhutan", "Nepal")) %>%
  mutate(region = "South East Asia") 

soamer_aquastat <- read_excel("shiny_app/raw_data/dams/soamer_aquastat.xlsx", sheet = "Dams", skip = 1, 
                              col_types = c("text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3)  %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "South America")

midest_aquastat <- read_excel("shiny_app/raw_data/dams/midest_aquastat.xlsx", sheet = "Dams", skip = 1,
                              col_types = c("text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "numeric", 
                                            "numeric", "text", "text", "text")) %>%
  clean_names() %>%
  rename(ISO3 = iso_alpha_3) %>%
  select(country, name_of_dam, ISO3, major_basin, sub_basin, completed_operational_since, dam_height_m) %>%
  mutate(region = "Middle East")

all_dams <- bind_rows(midest_aquastat, soamer_aquastat, seasia_aquastat, oceania_aquastat, namer_aquastat, 
                      europe_aquastat, cenamer_aquastat,  casia_aquastat, africa_aquastat, easia_aquastat, sasia_aquastat, seasia_aquastat) %>%
  group_by(region) %>%
  mutate(dam_numbers_region = n()) %>%
  ungroup () %>%
  group_by(ISO3) %>%
  mutate(dam_numbers_country = n()) %>%
  ungroup()

#Disasters worldwide

em_dat <- read_excel("shiny_app/raw_data/emdat_public_2020_11_01_query_uid-MSWGVQ.xlsx",
                     skip = 6) %>%
  clean_names()
