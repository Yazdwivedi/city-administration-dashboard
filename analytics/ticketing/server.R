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

agg_function <- list(MIN = 'min', MAX = 'max', AVG = 'median', SUM = 'sum')

get.weekdayset <- function(date) {
    weekday_num = wday(date)
    return(ifelse(weekday_num == 7 | weekday_num == 1,'Sat/Sun',
           ifelse(weekday_num == 2 | weekday_num == 6,'Mon/Fri',
                                             'Tue/Wed/Thu')))
}

# data <- read_delim("./hourly-lines.csv", delim = ";") %>%
#   mutate(DATETIME = ymd_hms(DATETIME),
#          month = month(DATETIME),
#          week = isoweek(DATETIME),
#          weekday = wday(DATETIME,label=TRUE,abbr = TRUE),
#          date = as.Date(DATETIME),
#          hour = hour(DATETIME),
#          weekdayset = get.weekdayset(DATETIME))
# 
# all.lines <- unique(data$CODLINHA)
# 
# pass.weekly.data <- read_delim("./weekly-usage_anonymized.csv", delim = ";") %>%
#   group_by(DATETIME) %>%
#   summarise(MIN = median(MIN),
#             MAX = median(MAX),
#             COUNT = median(COUNT),
#             SUM = median(SUM))
# 
# pass.monthly.data <- read_delim("./monthly-usage_anonymized.csv", delim = ";") %>%
#   group_by(DATETIME) %>%
#   summarise(MIN = median(MIN),
#             MAX = median(MAX),
#             COUNT = median(COUNT),
#             SUM = median(SUM))

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

# Tentando fazer o mapa aparecer 
# map_data <- stops.data.all %>%
#   group_by_("hour", "stop_lat", "stop_lon") %>%
#   summarise(total_passengers = sum(SUM)) 
# 
# g <- list(
#   scope = 'curitiba',
#   showland = TRUE,
#   landcolor = toRGB("grey83"),
#   subunitcolor = toRGB("white"),
#   countrycolor = toRGB("white"),
#   showlakes = TRUE,
#   lakecolor = toRGB("white"),
#   showsubunits = TRUE,
#   showcountries = TRUE,
#   resolution = 50,
#   projection = list(
#     type = 'conic conformal',
#     rotation = list(lon = -100)
#   ),
#   lonaxis = list(
#     showgrid = TRUE,
#     gridwidth = 0.5,
#     range = c(-140, -55),
#     dtick = 5
#   ),
#   lataxis = list(
#     showgrid = TRUE,
#     gridwidth = 0.5,
#     range = c(20, 60),
#     dtick = 5
#   )
# )
# 
# p <- plot_geo(map_data, lat = ~stop_lat, lon = ~stop_lon) %>%
#   add_markers(
#     text = ~paste(total_passengers, "passengers"),
#     color = ~total_passengers, symbol = I("circle"), size = I(8),
#     hoverinfo = "text"
#   ) %>%
#   layout(title = 'Passenges on stops', geo = g)
#


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
                                       paste('Line',input$bar.selected.line)))
            
            # filtered_data %>%
            #   filter(CODLINHA %in% get.selected.lines(lines_data = ., line.filter = input$line.filter,selected.line = input$bar.selected.line)) %>%
            #   group_by_(input$time.agg.select, "CODLINHA") %>%
            #   summarise(total_passengers = sum(SUM))
            # 
            # print(input$time.agg.select)
            
            # plot_data <- filtered_lines %>%
            #   group_by_(input$time.agg.select, "CODLINHA") %>%
            #   summarise(total_passengers = sum(SUM))
            # 
            # # print(plot_data)
            # p <- plot_data %>%
            # ggplot(data= .,aes_string(x=input$time.agg.select, y='total_passengers', group = 'CODLINHA', color = 'CODLINHA')) +
            #   geom_line(stat = "identity") +
            #   labs(title=paste(plot_title_prefix,"Total number of passengers per",capitalize(input$time.agg.select)),
            #        x=capitalize(input$time.agg.select),
            #        y="Number of passengers")
            #   p %>%
            #   ggplotly() %>%
            #   layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
            
            bar_plot <- filtered_lines %>%
                group_by_(input$time.agg.select, "CODLINHA") %>%
                summarise(total_passengers = sum(SUM)) %>%
                ggplot(aes_string(x=input$time.agg.select, y='total_passengers', group = 'CODLINHA', color = 'CODLINHA')) +
                geom_line(stat = "identity") +
                labs(title=paste(plot_title_prefix,"Total number of passengers per",capitalize(input$time.agg.select)),
                    x=capitalize(input$time.agg.select),
                    y="Number of passengers")
        }
        
        # plotly_chart = bar_plot %>%
        #     ggplotly() %>%
        #     layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
        # return(plotly_chart)
        return(bar_plot)
    })
    
    output$passenger.bar.plot <- renderPlotly({
      
      if (input$pass.time.agg.select == "week"){
        passenger.bar.plot <- pass.weekly.data %>%
          ggplot(aes_string(x = "DATETIME", y = input$pass.metric.select)) +
          geom_bar(stat = "identity") + 
          xlab("Week") +
          theme(axis.text.x = element_text(size=8, angle=20))
      } else {
        passenger.bar.plot <- pass.monthly.data %>%
          ggplot(aes_string(x = "DATETIME", y = input$pass.metric.select)) +
          geom_bar(stat = "identity") +
          xlab("Month")
      }
      
      plotly_chart = passenger.bar.plot %>%
        ggplotly(height = 500) %>%
        layout(margin=list(l = 100), yaxis=list(tickprefix=" "))
      return(plotly_chart)
      
    })
    
    output$map.plot <- renderPlot({
      
      filtered_data <- stops.data.all %>%
        filter_(paste(paste0(input$stops.time.agg.select,'_str'),'==',paste0('\'',input$stops.time.agg.value,'\'')))
      
      if (input$stops.time.agg.select != 'date') {
        filtered_data <- filtered_data %>%
          filter((date >= input$stops.date.range[1]) & (date <= input$stops.date.range[2]))  
      }
      
      map_data <- filtered_data %>%
        group_by_(input$stops.time.agg.select, "stop_lat", "stop_lon") %>%
        summarise_(total_passengers = interp(~median(f(var)), var = as.name(input$stops.metric.select), f = as.name(agg_function[[input$stops.metric.select]])))
      
      map_plot <- ggmap(ctba.map) + 
        geom_density2d(data = map_data, aes(x = stop_lon, y = stop_lat), size = 0.3) + 
        stat_density2d(data = map_data, aes(x = stop_lon, y = stop_lat, fill = ..level.., alpha = ..level..), 
                       size = 0.01, bins = 16, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0, 1), guide = FALSE)
      
      return(map_plot)
    })
    
    output$bar.lineSelector <- renderUI({
        selectInput("bar.selected.line", label = h3("Selected Line"), 
                    choices = as.list(all.lines), 
                    selected = '000')
    })
    
    observe({ 
      updated_choices = as.character(sort(unique(stops.data[[input$stops.time.agg.select]])))
      
      updateSelectInput(session, "stops.time.agg.value",
                        choices = updated_choices,
                        selected = updated_choices[1]
      )
    })
    
    observe({
      if (input$stops.time.agg.select == 'date') {
        disable("stops.date.range")  
      } else {
        enable("stops.date.range")
      }
    })
    
})
