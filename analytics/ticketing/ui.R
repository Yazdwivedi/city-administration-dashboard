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
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
  
    # Application title
    titlePanel("Curitiba City Administration Dashboard (Powered by Ophidia)"),
    
    hr(),
    
    navbarPage("CAD",
                id = 'tab',
                selected = 'Bus Stop',
                tabPanel("Overall",
                  sidebarLayout(
                      sidebarPanel(
                          # Copy the line below to make a select box 
                          selectInput("time.agg.select", label = h3("Time Aggregation"), 
                                      choices = list("Month" = 'month', "Week" = 'week', "Weekday" = 'weekday', 
                                                     "Weekday Set" = 'weekdayset', "Day" = 'date', "Hour" = 'hour'), 
                                      selected = 'month'),
                          # Copy the line below to make a slider range 
                          dateRangeInput("date.range", label = h3("Date range"),
                                         min=as.Date("2017-04-30"), max=as.Date("2017-07-17"),
                                         start=as.Date("2017-04-30"),end=as.Date("2017-07-17")),
                          radioButtons("line.filter", label = h3("Line Filter"), 
                                       choices = list("All" = 'all', "Top-5" = 'top5',"Bottom-5" = 'bottom5', "Single-Line" = 'single-line'), 
                                       selected = 'all'),
                          uiOutput('bar.lineSelector')
                      ),
                      mainPanel(
                          plotOutput("bar.plot")
                      )
                  )
                ),
               tabPanel("Passenger",
                        sidebarLayout(
                          sidebarPanel(
                            # Copy the line below to make a select box
                            radioButtons("pass.time.agg.select", label = h3("Time Aggregation"),
                                        choices = list("Month" = 'month', "Week" = 'week'),
                                        selected = 'month'),
                            selectInput("pass.metric.select", label = h3("Time Aggregation"), 
                                        choices = list("Minimum" = 'MIN', "Maximum" = 'MAX', 
                                                       "Count" = 'COUNT', "Sum" = 'SUM'),
                                        selected = "SUM")
                          ),
                        mainPanel(
                          plotlyOutput("passenger.bar.plot")
                        )
               )),
               tabPanel("Bus Stop",
                          sidebarLayout(
                            sidebarPanel(
                              # Copy the line below to make a select box 
                              selectInput("stops.time.agg.select", label = h3("Time Aggregation"), 
                                          choices = list("Month" = 'month', "Week" = 'week', "Weekday" = 'weekday', 
                                                         "Weekday Set" = 'weekdayset', "Day" = 'date', "Hour" = 'hour'), 
                                          selected = 'month'),
                              selectInput("stops.time.agg.value", label = h3("Time Aggregation Value"), 
                                          choices = list('4' = 4, '5' = 5, '6' = 6, '7' = 7),
                                          selected = 5),
                              # Copy the line below to make a slider range 
                              dateRangeInput("stops.date.range", label = h3("Date range"),
                                             min=as.Date("2017-04-30"), max=as.Date("2017-07-17"),
                                             start=as.Date("2017-04-30"),end=as.Date("2017-07-17"))
                            ),
                            mainPanel(
                              plotOutput("map.plot")
                            )
                          )
               )
    )
  )
)
