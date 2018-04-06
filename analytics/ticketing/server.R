#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

get.weekdayset <- function(date) {
    weekday = wday(date)
    if (weekday == 7 || weekday == 1) {
        return('Sat/Sun')
    } else if (weekday == 2 || weekday == 6) {
        return('Mon/Fri')
    } else if (weekday > 2 && weekday < 6) {
        return('Tue/Wed/Thu')
    }       
}

data <- read.csv2('/local/tarciso/masters/data/city-admin-dashboard/hourly-lines.csv') %>%
    mutate(DATETIME = ymd_hms(DATETIME),
           month = month(DATETIME),
           week = isoweek(DATETIME),
           weekday = wday(DATETIME,abbr = TRUE),
           date = as.Date(DATETIME),
           hour = hour(DATETIME),
           SUM = as.numeric(SUM))
#     rowwise() %>%
#     mutate(weekdayset = get.weekdayset(DATETIME)) %>%
#     ungroup()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$plot <- renderPlotly({
        # plot_data <- data %>%
        #       group_by(month) %>%
        #       summarise(total_passengers = sum(SUM))
        # barplot(plot_data)
        #plot(cars)
        
        bar_plot <- data %>%
           group_by(month) %>%
           summarise(total_passengers = sum(SUM)) %>%
           ggplot(aes(x=month, y=total_passengers)) +
           geom_bar(stat = "identity") +
           labs(title="Total number of passengers per month",
                x="Month",
                y="Number of passengers")

        return(ggplotly(bar_plot))
    })
    
    output$event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Hover on a point!" else d
    })
    
    output$summary <- renderPrint({
      summary(cars)
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(cars)
    })
    
    # You can access the value of the widget with input$time.agg.select, e.g.
    output$value <- renderPrint({ input$time.agg.select })
    
    output$time.range.slider <- renderPrint({ input$time.range.slider})
      
})
