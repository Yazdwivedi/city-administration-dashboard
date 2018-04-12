#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(Hmisc)
library(ggmap)
library(lazyeval)

options(scipen=0)

AXIS_LABEL_SIZE = 15
TITLE_LABEL_SIZE = 20

agg_function <- list(MIN = 'min', MAX = 'max', AVG = 'median', SUM = 'sum')

get.weekdayset <- function(date) {
    weekday_num = wday(date)
    return(ifelse(weekday_num == 7 | weekday_num == 1,'Sat/Sun',
           ifelse(weekday_num == 2 | weekday_num == 6,'Mon/Fri',
                                             'Tue/Wed/Thu')))
}

data <- read_delim("./hourly-lines.csv", delim = ";") %>%
  mutate(DATETIME = ymd_hms(DATETIME),
         month = month(DATETIME),
         week = isoweek(DATETIME),
         weekday = wday(DATETIME,label=TRUE,abbr = TRUE),
         date = as.Date(DATETIME),
         hour = hour(DATETIME),
         weekdayset = get.weekdayset(DATETIME))

all.lines <- unique(data$CODLINHA)

pass.weekly.data <- read_delim("./weekly-usage_anonymized.csv", delim = ";") %>%
  group_by(DATETIME) %>%
  summarise(MIN = median(MIN),
            MAX = median(MAX),
            COUNT = median(COUNT),
            SUM = median(SUM))

pass.monthly.data <- read_delim("./monthly-usage_anonymized.csv", delim = ";") %>%
  group_by(DATETIME) %>%
  summarise(MIN = median(MIN),
            MAX = median(MAX),
            COUNT = median(COUNT),
            SUM = median(SUM))

stops.data <- read_delim("hourly-stops.csv", delim = ";") %>%
  mutate(DATETIME = ymd_hms(DATETIME),
         month = month(DATETIME),
         week = isoweek(DATETIME),
         weekday = wday(DATETIME,label=TRUE,abbr = TRUE),
         date = as.Date(DATETIME),
         hour = hour(DATETIME),
         weekdayset = get.weekdayset(DATETIME),
         month_str = as.character(month),
         week_str = as.character(week),
         weekday_str = as.character(weekday),
         date_str = as.character(date),
         hour_str = as.character(hour),
         weekdayset_str = as.character(weekdayset))

stops.gtfs <- read_csv("gtfs/stops.txt", col_types = cols(.default = "_",
                                                          stop_id = col_integer(),
                                                          stop_lat = col_double(),
                                                          stop_lon = col_double()))

stops.data.all <- stops.data %>%
  inner_join(stops.gtfs, c("BUSSTOPID" = "stop_id"))

#ctba.map <- get_map(location = "Curitiba", maptype = "satellite", zoom = 12)
#save(ctba.map, file="./ctba-map.rda")
load("./ctba-map.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    get.selected.lines <- function(lines_data,line.filter,selected.line) {
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
    
    output$bar.plot <- renderPlot({
      
        filtered_data <- data %>%
            filter((DATETIME >= input$date.range[1]) & (DATETIME <= input$date.range[2]))
        
        sel_lines = get.selected.lines(lines_data = filtered_data, line.filter = input$line.filter,selected.line = input$bar.selected.line)
        
        filtered_lines <- filtered_data %>%
          filter(CODLINHA %in% sel_lines)
        
        #print(get.selected.lines(filtered_data,line.filter = input$line.filter,selected.line = input$selected.line))
        
        if (input$line.filter == 'all') {
            bar_plot <- filtered_lines %>%
                group_by_(input$time.unit) %>%
                summarise(total_passengers = sum(SUM)) %>%
                ggplot(aes_string(x=input$time.unit, y='total_passengers')) +
                geom_bar(stat = "identity") +
                labs(title=paste("Total number of passengers per",capitalize(input$time.unit)),
                     x=capitalize(input$time.unit),
                     y="Number of passengers") + 
              theme(plot.title = element_text(size=TITLE_LABEL_SIZE, face="bold", margin = margin(10, 0, 10, 0), hjust = 0.5),
                    axis.title=element_text(size=AXIS_LABEL_SIZE, hjust = 0.5))
        } else {
            plot_title_prefix = ifelse(input$line.filter == 'top5','Top-5',
                                ifelse(input$line.filter == 'bottom5','Bottom-5',
                                       paste('Line',input$bar.selected.line)))
            
            bar_plot <- filtered_lines %>%
                group_by_(input$time.unit, "CODLINHA") %>%
                summarise(total_passengers = sum(SUM)) %>%
                ggplot(aes_string(x=input$time.unit, y='total_passengers', group = 'CODLINHA', color = 'CODLINHA')) +
                geom_line(stat = "identity") +
                labs(title=paste(plot_title_prefix,"Total number of passengers per",capitalize(input$time.unit)),
                    x=capitalize(input$time.unit),
                    y="Number of passengers") + 
              theme(plot.title = element_text(size=TITLE_LABEL_SIZE, face="bold", margin = margin(10, 0, 10, 0), hjust = 0.5),
                    axis.title=element_text(size=AXIS_LABEL_SIZE, hjust = 0.5))
        }
        
        return(bar_plot)
    })
    
    output$passenger.bar.plot <- renderPlotly({
      
      if (input$pass.time.unit == "week"){
        passenger.bar.plot <- pass.weekly.data %>%
          ggplot(aes_string(x = "DATETIME", y = input$pass.metric.select)) +
          geom_bar(stat = "identity") + 
          xlab("Week") +
          theme(axis.text.x = element_text(size=8, angle=20))
      } else {
        passenger.bar.plot <- pass.monthly.data %>%
          ggplot(aes_string(x = "DATETIME", y = input$pass.metric.select)) +
          geom_bar(stat = "identity") +
          labs(title=paste("Agerage passenger num. boardings per",capitalize(input$time.unit)),
               x=capitalize(input$time.unit),
               y="Number of boardings") + 
          theme(plot.title = element_text(face="bold", margin = margin(5, 0, 5, 0), hjust = 0.5),
                axis.title=element_text(size=AXIS_LABEL_SIZE, hjust = 0.5))
      }
      
      plotly_chart = passenger.bar.plot %>%
        ggplotly(height = 500) %>%
        layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
      return(plotly_chart)
      
    })
    
    output$map.plot <- renderPlot({
      
      filtered_data <- stops.data.all %>%
        filter_(paste(paste0(input$stops.time.unit,'_str'),'==',paste0('\'',input$stops.time.agg.value,'\'')))
      
      if (input$stops.time.unit != 'date') {
        filtered_data <- filtered_data %>%
          filter((date >= input$stops.date.range[1]) & (date <= input$stops.date.range[2]))  
      }
      
      map_data <- filtered_data %>%
        group_by_(input$stops.time.unit, "stop_lat", "stop_lon") %>%
        summarise(total_passengers = median(SUM))
      
      map_plot <- ggmap(ctba.map) + 
        geom_density2d(data = map_data, aes(x = stop_lon, y = stop_lat), size = 0.3) + 
        stat_density2d(data = map_data, aes(x = stop_lon, y = stop_lat, fill = ..level.., alpha = ..level..), 
                       size = 0.01, bins = 16, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0, 1), guide = FALSE) +
        labs(title=paste("Passengers at bus stops Heatmap")) + 
        theme(plot.title = element_text(size=TITLE_LABEL_SIZE, face="bold", margin = margin(10, 0, 10, 0), hjust = 0.5),
              axis.title=element_text(size=AXIS_LABEL_SIZE, hjust = 0.5))
      
      return(map_plot)
    })
    
    output$bar.lineSelector <- renderUI({
        selectInput("bar.selected.line", label = h3("Selected Line"), 
                    choices = as.list(all.lines), 
                    selected = '000')
    })
    
    observe({ 
      updated_choices = as.character(sort(unique(stops.data[[input$stops.time.unit]])))
      
      updateSelectInput(session, "stops.time.agg.value",
                        choices = updated_choices,
                        selected = updated_choices[1]
      )
    })
    
    observe({
      if (input$stops.time.unit == 'date') {
        disable("stops.date.range")  
      } else {
        enable("stops.date.range")
      }
      
      if (input$line.filter == 'single-line') {
        enable("bar.lineSelector")  
      } else {
        disable("bar.lineSelector")
      }
    })
    
})
