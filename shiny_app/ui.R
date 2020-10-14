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
    navbarPage("Disaster Info",
               tabPanel(
                   "About",
                   p("Put text here"),
                   h6("Title here")
                   ),
               # 
               tabPanel(
                   "Data",
                  
               )
    ))
