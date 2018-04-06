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
library(ggplot2)
library(lubridate)
library(plotly)
library(Hmisc)

options(scipen=0)

get.weekdayset <- function(date) {
    weekday_num = wday(date)
    return(ifelse(weekday_num == 7 | weekday_num == 1,'Sat/Sun',
           ifelse(weekday_num == 2 | weekday_num == 6,'Mon/Fri',
                                             'Tue/Wed/Thu')))
}

data <- read.csv2('/local/tarciso/masters/data/city-admin-dashboard/hourly-lines.csv') %>%
    mutate(DATETIME = ymd_hms(DATETIME),
           month = month(DATETIME),
           week = isoweek(DATETIME),
           weekday = wday(DATETIME,label=TRUE,abbr = TRUE,locale = "en_US.utf8"),
           date = as.Date(DATETIME),
           hour = hour(DATETIME),
           SUM = as.numeric(SUM),
           weekdayset = get.weekdayset(DATETIME))

all.lines <- unique(data$CODLINHA)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    get.selected.lines <- function(data,line.filter,selected.line) {
        if (line.filter == 'all') {
            return(all.lines)
        } else if (line.filter == 'top5') {
            selected.lines <- data %>%
                filter(!(CODLINHA %in% c("000","OPC"))) %>%
                group_by(CODLINHA) %>%
                summarise(num.passengers = median(SUM,na.rm=TRUE)) %>%
                top_n(5, num.passengers) %>%
                `$`(CODLINHA)
            return(selected.lines)
        } else if (line.filter == 'bottom5') {
            selected.lines <- data %>%
                filter(!(CODLINHA %in% c("000","OPC"))) %>%
                group_by(CODLINHA) %>%
                summarise(num.passengers = median(SUM,na.rm=TRUE)) %>%
                top_n(-5, num.passengers) %>%
                `$`(CODLINHA)
            return(selected.lines)
        } else {
            return(selected.line)
        }
    }
    
    output$plot <- renderPlotly({
        print(input$time.agg.select)
        print(str(input$date.range))
        filtered_data <- data %>%
            filter((DATETIME >= input$date.range[1]) & (DATETIME <= input$date.range[2]))
        
        # print(get.selected.lines(line.filter = input$line.filter,selected.line = input$selected.line))
        
        if (input$line.filter == 'all') {
            bar_plot <- filtered_data %>%
                filter(CODLINHA %in% get.selected.lines(data = ., line.filter = input$line.filter,selected.line = input$selected.line)) %>%
                group_by_(input$time.agg.select) %>%
                summarise(total_passengers = sum(SUM)) %>%
                ggplot(aes_string(x=input$time.agg.select, y='total_passengers')) +
                geom_bar(stat = "identity") +
                labs(title=paste("Total number of passengers per",capitalize(input$time.agg.select)),
                     x=capitalize(input$time.agg.select),
                     y="Number of passengers")
        } else {
            plot_title_prefix = ifelse(input$line.filter == 'top5','Top-5',
                                ifelse(input$line.filter == 'bottom5','Bottom-5',
                                       paste('Line',input$selected.line)))
            bar_plot <- filtered_data %>%
                filter(CODLINHA %in% get.selected.lines(data = ., line.filter = input$line.filter,selected.line = input$selected.line)) %>%
                group_by_(input$time.agg.select, "CODLINHA") %>%
                summarise(total_passengers = sum(SUM)) %>%
                ggplot(aes_string(x=input$time.agg.select, y='total_passengers', group = 'CODLINHA', color = 'CODLINHA')) +
                geom_line(stat = "identity") +
                labs(title=paste(plot_title_prefix,"Total number of passengers per",capitalize(input$time.agg.select)),
                     x=capitalize(input$time.agg.select),
                     y="Number of passengers")
        }
        
        plotly_chart = bar_plot %>%
            ggplotly() %>%
            layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
        return(plotly_chart)
    })
    
    
    output$lineSelector <- renderUI({
        selectInput("selected.line", label = h3("Selected Line"), 
                    choices = as.list(all.lines), 
                    selected = '000')
    })
    
})
