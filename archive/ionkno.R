ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody( # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("world_increase", height = 250)),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  ))
)


server <- function(input, output) {
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
      scale_color_discrete(name = "Regions") +
      labs(title = "Number of dams and reservoirs in operation by each year", 
           x = "Year", 
           y = "Number of dams",
           caption = "Source: GrandD v1.3") 
  })
}

shinyApp(ui, server)
