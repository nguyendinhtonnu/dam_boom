#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
      "Population and Disasters in Asian River Delta",
               tabPanel(
                   "About",
                   h6("What river is this?"),
                   p("In this project, I'm going to create maps of at least 5 major river delta and their basin in Asia (the Irrawaddy, the Mekong, the Red, the Yellow, the Yangtze, the Brahmaputra-Ganges, etc), looking at hydrological control infrastructure in the basin, such as river dikes and large dams, and compare it against water-related disaster events, such as drought, flood, famine, over time.
                     I'm also interested in population distribution, land use, and topography of each basin. The goal is to illustrate how Asia's most powerful rivers are being managed, their efficacy and problems over time, and the population most vulnerable to these decisions. Furthermore, is there a sacficice zone identifiable in these basins? That's something I hope to be able to shed light on."),
                   p("Right now I'm looking at data from the Emergency Events Database (EM-DAT), but I'm also looking at government census for population data. Geographic studies are a good place to figure out the area of delta and topographic information. The most difficult thing is records of disasters, these tend to be all over the place, but I think a good place to start is in comprehensive environmental histories of a river delta, such as David Pietz's book on the Yellow River, Chris Courtney's book on the Yangtze river and the historic 1931 flood, or Arupjyoti Saikia's comprehensive work on the Brahmaputra river delta."),
                   p("Link to my repo: https://github.com/nguyendinhtonnu/final_project")),
               # 
               tabPanel(
                   "Data",
                   h6("Some river disasters in Asia"), 
                       plotOutput(outputId = "scatter_plot")
                   ))
    )
