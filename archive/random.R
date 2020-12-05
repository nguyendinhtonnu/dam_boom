2. Maps

```{r dams}
dam_num <- all_dams %>%
  drop_na(decimal_degree_latitude) %>%
  group_by(country, region) %>%
  summarize(dam_numbers_country = n(), .groups = "drop")  %>%
  arrange(desc(dam_numbers_country)) %>%
  slice(1:20)

dam_num %>% slice(1:10) %>%
  ggplot(aes(country, dam_numbers_country, fill = region)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) 

all_dams_about <- readRDS("shiny_app/all_dams_about.rds") 


all_dams_about %>% 
  group_by(region) %>%
  drop_na(completed_operational_since) %>%
  mutate(count = n()) %>%
  ggplot(aes(completed_operational_since, fill = fct_reorder(region, count))) + 
  geom_bar(position = "stack") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_discrete(name = "Regions") +
  scale_x_discrete(breaks = c(seq(from = 0, to = 2019, by = 50))) + 
  theme_light()

all_dams %>% 
  filter(ISO3 == "VNM", major_basin == "Red") %>%
  group_by(completed_operational_since) %>%
  #drop_na(completed_operational_since) %>%
  #summarize(count = n(), .groups = "drop") 
  
  ggplot(aes(completed_operational_since)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90)) 

unique(all_dams$region)

em_dat <- read_excel("raw_data/emdat_public_2020_11_01_query_uid-MSWGVQ.xlsx",
                     skip = 6) %>%
  clean_names()

```

# for the modelling component, run stan_glm and explain it correctly. 

my_simpl <- wrld_simpl 

my_simpl@data <- left_join(dam_numbers, wrld_simpl@data, by = "ISO3", na.rm = TRUE, 
                           c)

type_convert(
  wrld_simpl@data,
  col_types = "numeric",
  na = c("", "NA"),
  trim_ws = TRUE,
  locale = default_locale()
)

cols(my_simpl@data$dam_numbers = col_double())

#na_simpl@polygons <- na_simpl@polygons %>%
# filter(ID %in% c("CAN", "MEX", "USA"))

#have all the data for all the continents --> replace na_simpl = wrld_simpl 

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = my_simpl@data$dam_numbers
)

leaflet() %>% 
  setView(lng = 0, lat = 0, zoom = 0) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(dam_numbers)) %>%
  addLegend("bottomright", pal = pal, values = ~ dam_numbers,
            title = "Dam numbers",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)

```{r test}
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = all_dams, lat = ~ decimal_degree_latitude, lng = ~ decimal_degree_longitude, radius = 0.1) %>%
  setView(lat = 0, lng = 0, zoom = 2)
```










(PL: 
   In this project, I'm going to create maps of at least 5 
                   major river delta and their basin in Asia (the Irrawaddy, 
                   the Mekong, the Red, the Yellow, the Yangtze, the 
                   Brahmaputra-Ganges, etc), looking at hydrological control 
                   infrastructure in the basin, such as river dikes and large 
                   dams, and compare it against water-related disaster events, 
                   such as drought, flood, famine, over time.
  I'm also interested in population distribution, land use, 
 and topography of each basin. The goal is to illustrate how 
 Asia's most powerful rivers are being managed, their 
efficacy and problems over time, and the population most 
vulnerable to these decisions. Furthermore, is there a 
sacficice zone identifiable in these basins? That's 
 something I hope to be able to shed light on.
 
 Right now I'm looking at data from the Emergency 
                     Events Database (EM-DAT), but I'm also looking at 
 government census for population data. Geographic studies 
 are a good place to figure out the area of delta and 
 topographic information. The most difficult thing is 
 records of disasters, these tend to be all over the place, 
 but I think a good place to start is in comprehensive 
 environmental histories of a river delta, such as 
 David Pietz's book on the Yellow River, Chris Courtney's 
 book on the Yangtze river and the historic 1931 flood, or 
 Arupjyoti Saikia's comprehensive work on the Brahmaputra 
                     river delta.)

fluidPage(
        fluidRow(style = 'padding:30px;',
                 column(7,
                        plotOutput("world_increase")),
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
               leafletOutput("basin_map", height = 700)))
      )
               )))
      