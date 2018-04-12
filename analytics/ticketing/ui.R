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
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    useShinyjs(),
  
    # Application title
    titlePanel("Curitiba City Administration Dashboard (Powered by Ophidia)"),
    
    hr(),
    
    navbarPage("CAD",
                id = 'tab',
                selected = 'Overall Boarding',
                tabPanel("Overall Boarding",
                         
                         hr(),
                         hr(),
                  sidebarLayout(
                      sidebarPanel(
                          selectInput("time.unit", label = h3("Time Unit"), 
                                      choices = list("Month" = 'month', "Week" = 'week', "Weekday" = 'weekday', 
                                                     "Weekday Set" = 'weekdayset', "Day" = 'date', "Hour" = 'hour'), 
                                      selected = 'month'),
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
               tabPanel("Average Passenger Boardings",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("pass.time.unit", label = h3("Time Unit"),
                                        choices = list("Month" = 'month', "Week" = 'week'),
                                        selected = 'month'),
                            selectInput("pass.metric.select", label = h3("Metric"), 
                                        choices = list("Minimum" = 'MIN', "Maximum" = 'MAX', 
                                                       "Count" = 'COUNT', "Sum" = 'SUM'),
                                        selected = "SUM")
                          ),
                        mainPanel(
                          plotlyOutput("passenger.bar.plot")
                        )
               )),
               tabPanel("Bus Stop Crowdedness",
                          sidebarLayout(
                            sidebarPanel(
                              # Copy the line below to make a select box 
                              selectInput("stops.time.unit", label = h3("Time Unit"), 
                                          choices = list("Month" = 'month', "Week" = 'week', "Weekday" = 'weekday', 
                                                         "Weekday Set" = 'weekdayset', "Day" = 'date', "Hour" = 'hour'), 
                                          selected = 'month'),
                              selectInput("stops.time.agg.value", label = h3("Time Aggregation Value"), 
                                          choices = list('4' = '4', '5' = '5', '6' = '6', '7' = '7'),
                                          selected = '4'),
                              # selectInput("stops.metric.select", label = h3("Metric"), 
                              #             choices = list("Minimum" = 'MIN', "Maximum" = 'MAX', 
                              #                            "Average" = 'AVG', "Sum" = 'SUM'),
                              #             selected = "SUM"),
                              dateRangeInput("stops.date.range", label = h3("Date range"),
                                             min=as.Date("2017-04-30"), max=as.Date("2017-07-17"),
                                             start=as.Date("2017-04-30"),end=as.Date("2017-07-17"))
                            ),
                            mainPanel(
                              plotOutput("map.plot")
                            )
                          )
               )
    ),
    hr(),
    print("Note: The data provided for this application comprises several (non-contiguous) days of April (1 day), May (29 days), June (17 days) and July (17 days) in 2017")
  )
)
