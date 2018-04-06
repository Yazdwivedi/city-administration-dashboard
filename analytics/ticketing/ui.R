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
  
  hr(),
  
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
          uiOutput('lineSelector')
      ),
      mainPanel(
          plotlyOutput("plot")
      )
  )
))
