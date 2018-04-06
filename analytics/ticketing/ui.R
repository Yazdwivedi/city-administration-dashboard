#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Curitiba City Administration Dashboard (Powered by Ophidia)"),
  
  navbarPage("CAD",
             id = 'tab',
             tabPanel("Overall",
                      sidebarLayout(
                          sidebarPanel(
                              # Copy the line below to make a select box 
                              selectInput("time.agg.select", label = h3("Time Aggregation"), 
                                          choices = list("Month" = 1, "Week" = 2, "Weekday" = 3, 
                                                         "Weekday Set" = 4, "Day of Month" = 5, "Hour" = 6), 
                                          selected = 1),
                              # Copy the line below to make a slider range 
                              sliderInput("time.range.slider", label = h3("Time Range"), min = as.Date("2017-04-30"), 
                                          max = as.Date("2017-07-17"), value = c(as.Date("2017-04-30"), as.Date("2017-07-17")))
                          ),
                          mainPanel(
                              plotlyOutput("plot"),
                              verbatimTextOutput("event")
                          )
                      )
             ),
             tabPanel("Per Line",
                      verbatimTextOutput("summary")
             ),
             navbarMenu("More",
                        tabPanel("Table",
                                 DT::dataTableOutput("table")
                        )
             )
  )
))
