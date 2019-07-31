library(rvest)
library(tidyverse)
library(data.table)
library(tidytext)
library(stringr)
library(ggplot2)
library(ggrepel)
library(shiny)
library(data.table)
library(leaflet)
library(geojsonio)
library(plotly)
library(corrplot)
library(lubridate)
library(readr)
library(reshape2)
library(ggmap)

ggmap::register_google(key = "AIzaSyCWJ8DwoFBs1kYzM82SSA-XAvBDXT8YJRA")

# Original Data Files
go2017 <- read.csv("2017_gobike.csv")
go201906 <- read_csv("201906-baywheels-tripdata.csv")

go201801 <- read_csv("201801-fordgobike-tripdata.csv")
go201802 <- read_csv("201802-fordgobike-tripdata.csv")
go201803 <- read_csv("201803-fordgobike-tripdata.csv")
go201804 <- read_csv("201804-fordgobike-tripdata.csv")
go201805 <- read_csv("201805-fordgobike-tripdata.csv")
go201806 <- read_csv("201806-fordgobike-tripdata.csv")
go201807 <- read_csv("201807-fordgobike-tripdata.csv")
go201808 <- read_csv("201808-fordgobike-tripdata.csv")
go201809 <- read_csv("201809-fordgobike-tripdata.csv")
go201810 <- read_csv("201810-fordgobike-tripdata.csv")
go201811 <- read_csv("201811-fordgobike-tripdata.csv")
go201812 <- read_csv("201812-fordgobike-tripdata.csv")
go201901 <- read_csv("201901-fordgobike-tripdata.csv")
go201902 <- read_csv("201902-fordgobike-tripdata.csv")
go201903 <- read_csv("201903-fordgobike-tripdata.csv")
go201904 <- read_csv("201904-fordgobike-tripdata.csv")
go201905 <- read_csv("201905-baywheels-tripdata.csv")

go2017_month <- go2017 %>%
  mutate(month = substring(start_time, 6, 7))
go2017_month$month <- as.numeric(as.character(unlist(go2017_month[,16])))

# GeoJSON
go2017_geojson <- go2017[,c(1,2,3,6,7,10,11,13)]
go201906_geojson <- go201906[,c(1,2,3,6,7,10,11,13)]

combined.data2 <- rbindlist(list(go201801, go201802, go201803, go201804, go201805, go201806, go201807, go201808, go201809, go201810, go201811, go201812, 
                  go201901, go201902, go201903, go201904, go201905))
combined.data2 <- combined.data2[, c(1,2,3,6,7,10,11,13)]
combined.data2$start_time <- filter.month(combined.data2$start_time)
combined.data2$end_time <- filter.month(combined.data2$end_time)

go201906_geojson$start_time <- filter.month(go201906_geojson$start_time)
go201906_geojson$end_time <- filter.month(go201906_geojson$end_time)
combined.data3 <- rbind(go201906_geojson, combined.data2)

go2017_geojson$start_time <- filter.month(go2017_geojson$start_time)
go2017_geojson$end_time <- filter.month(go2017_geojson$end_time)
combined.data3 <- rbind(go2017_geojson, combined.data3)

sf <- ggmap(get_googlemap(center = c(longitude = -122.42, latitude = 37.77),
                          zoom = 13, scale = 4, 
                          maptype = "roadmap",
                          color = "color"))

## AGE
# 2017
age201706 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 06)
age201706 <- 2017 - as.numeric(as.character(unlist(age201706[,14]))) %>%
  mean()

age201707 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 07)
age201707 <- 2017 - as.numeric(as.character(unlist(age201707[,14]))) %>%
  mean()

age201708 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 08)
age201708 <- 2017 - as.numeric(as.character(unlist(age201708[,14]))) %>%
  mean()

age201709 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 09)
age201709 <- 2017 - as.numeric(as.character(unlist(age201709[,14]))) %>%
  mean()

age201710 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 10)
age201710 <- 2017 - as.numeric(as.character(unlist(age201710[,14]))) %>%
  mean()

age201711 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 11)
age201711 <- 2017 - as.numeric(as.character(unlist(age201711[,14]))) %>%
  mean()

age201712 <- go2017_month %>%
  filter(!is.na(go2017_month[,14])) %>%
  filter(month == 12)
age201712 <- 2017 - as.numeric(as.character(unlist(age201712[,14]))) %>%
  mean()

## 2018 
age201801 <- 2018 - as.numeric(as.character(unlist(go201801[,14] %>%
                                                            filter(!is.na(go201801[,14]))))) %>%
                       mean()

age201802 <- 2018 - as.numeric(as.character(unlist(go201802[,14] %>%
                                                                    filter(!is.na(go201802[,14]))))) %>%
                       mean()
age201803 <- 2018 - as.numeric(as.character(unlist(go201803[,14] %>%
                                                                    filter(!is.na(go201803[,14]))))) %>%
                       mean()
age201804 <- 2018 - as.numeric(as.character(unlist(go201804[,14] %>%
                                                                    filter(!is.na(go201804[,14]))))) %>%
                       mean()
age201805 <- 2018 - as.numeric(as.character(unlist(go201805[,14] %>%
                                                                    filter(!is.na(go201805[,14]))))) %>%
                       mean()
age201806 <- 2018 - as.numeric(as.character(unlist(go201806[,14] %>%
                                                                    filter(!is.na(go201806[,14]))))) %>%
                       mean()
age201807 <- 2018 - as.numeric(as.character(unlist(go201807[,14] %>%
                                                                    filter(!is.na(go201807[,14]))))) %>%
                       mean()
age201808 <- 2018 - as.numeric(as.character(unlist(go201808[,14] %>%
                                                                    filter(!is.na(go201808[,14]))))) %>%
                       mean()
age201809 <- 2018 - as.numeric(as.character(unlist(go201809[,14] %>%
                                                                    filter(!is.na(go201809[,14]))))) %>%
                       mean()
age201810 <- 2018 - as.numeric(as.character(unlist(go201810[,14] %>%
                                                                    filter(!is.na(go201810[,14]))))) %>%
                       mean()
age201811 <- 2018 - as.numeric(as.character(unlist(go201811[,14] %>%
                                                                    filter(!is.na(go201811[,14]))))) %>%
                       mean()
age201812 <- 2018 - as.numeric(as.character(unlist(go201812[,14] %>%
                                                                    filter(!is.na(go201812[,14]))))) %>%
                       mean()
age201901 <- 2019 - as.numeric(as.character(unlist(go201901[,14] %>%
                                                                    filter(!is.na(go201901[,14]))))) %>%
                       mean()
age201902 <- 2019 -  as.numeric(as.character(unlist(go201902[,14] %>%
                                                                    filter(!is.na(go201902[,14]))))) %>%
                       mean()
age201903 <- 2019 - as.numeric(as.character(unlist(go201903[,14] %>%
                                                                    filter(!is.na(go201903[,14]))))) %>%
                       mean()
age201904 <- 2019 - as.numeric(as.character(unlist(go201904[,14] %>%
                                                                    filter(!is.na(go201904[,14]))))) %>%
                       mean()
age201905 <- 2019 -  as.numeric(as.character(unlist(go201905[,14] %>%
                                                                    filter(!is.na(go201905[,14]))))) %>%
                       mean()
age201906 <- 2019 - as.numeric(as.character(unlist(go201906[,14] %>%
                                                                    filter(!is.na(go201906[,14]))))) %>%
                       mean()

# DURATION
# 2017
time201706 <- go2017_month %>%
  filter(month == 06) %>%
  select(duration_sec)
time201706 <- as.numeric(as.character(unlist(time201706[,1]))) %>%
  mean()/60

time201707 <- go2017_month %>%
  filter(month == 07) %>%
  select(duration_sec)
time201707 <- as.numeric(as.character(unlist(time201707[,1]))) %>%
  mean()/60

time201708 <- go2017_month %>%
  filter(month == 08) %>%
  select(duration_sec)
time201708 <- as.numeric(as.character(unlist(time201708[,1]))) %>%
  mean()/60

time201709 <- go2017_month %>%
  filter(month == 09) %>%
  select(duration_sec)
time201709 <- as.numeric(as.character(unlist(time201709[,1]))) %>%
  mean()/60

time201710 <- go2017_month %>%
  filter(month == 10) %>%
  select(duration_sec)
time201710 <- as.numeric(as.character(unlist(time201710[,1]))) %>%
  mean()/60

time201711 <- go2017_month %>%
  filter(month == 11) %>%
  select(duration_sec)
time201711 <- as.numeric(as.character(unlist(time201711[,1]))) %>%
  mean()/60

time201712 <- (go2017_month %>%
  filter(month == 12) %>%
  select(duration_sec))[,1]
time201712 <- as.numeric(as.character(unlist(time201712))) %>%
  mean()/60

# 2018 
time201801 <- as.numeric(as.character(unlist(go201801[,1]))) %>%
  mean()/60
time201802 <- as.numeric(as.character(unlist(go201802[,1]))) %>%
  mean()/60
time201803 <- as.numeric(as.character(unlist(go201803[,1]))) %>%
  mean()/60
time201804 <- as.numeric(as.character(unlist(go201804[,1]))) %>%
  mean()/60
time201805 <- as.numeric(as.character(unlist(go201805[,1]))) %>%
  mean()/60
time201806 <- as.numeric(as.character(unlist(go201806[,1]))) %>%
  mean()/60
time201807 <- as.numeric(as.character(unlist(go201807[,1]))) %>%
  mean()/60
time201808 <- as.numeric(as.character(unlist(go201808[,1]))) %>%
  mean()/60
time201809 <- as.numeric(as.character(unlist(go201809[,1]))) %>%
  mean()/60
time201810 <- as.numeric(as.character(unlist(go201810[,1]))) %>%
  mean()/60
time201811 <- as.numeric(as.character(unlist(go201811[,1]))) %>%
  mean()/60
time201812 <- as.numeric(as.character(unlist(go201812[,1]))) %>%
  mean()/60
time201901 <- as.numeric(as.character(unlist(go201901[,1]))) %>%
  mean()/60
time201902 <- as.numeric(as.character(unlist(go201902[,1]))) %>%
  mean()/60
time201903 <- as.numeric(as.character(unlist(go201903[,1]))) %>%
  mean()/60
time201904 <- as.numeric(as.character(unlist(go201904[,1]))) %>%
  mean()/60
time201905 <- as.numeric(as.character(unlist(go201905[,1]))) %>%
  mean()/60
time201906 <- as.numeric(as.character(unlist(go201906[,1]))) %>%
  mean()/60

## CUSTOMER/SUBSCRIBER
cs201706 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 06) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201707 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 07) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201708 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 08) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201709 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 09) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201710 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 10) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201711 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 11) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201712 <- go2017_month %>%
  filter(!is.na(go2017_month[,13])) %>%
  filter(month == 12) %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201801 <- go201801 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201802 <- go201802 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201803 <- go201803 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201804 <- go201804 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201805 <- go201805 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201806 <- go201806 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201807 <- go201807 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201808 <- go201808 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201809 <- go201809 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201810 <- go201810 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201811 <- go201811 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201812 <- go201812 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201901 <- go201901 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201902 <- go201902 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201903 <- go201903 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201904 <- go201904 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201905 <- go201905 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())
cs201906 <- go201906 %>%
  mutate(subscriber = ifelse(user_type == 'Subscriber', 1, 0)) %>%
  summarize(count = sum(subscriber)/n())

# data frames
cs_month_1 <- do.call("rbind", list(cs201706, cs201707, cs201708, cs201709, cs201710, cs201711, cs201712))
month_1 <- (c('2017-06', '2017-07', '2017-08', '2017-09', '2017-10', '2017-11', '2017-12'))
cs_1.data <- data.frame(month_1, cs_month_1)
names(cs_1.data) <- c("month", "prop.of.subscribers")

cs_month_2 <- do.call("rbind", list(cs201801, cs201802, cs201803, cs201804, cs201805, cs201806, cs201807, cs201808, cs201809, cs201810, cs201811, cs201812,
                                    cs201901, cs201902, cs201903, cs201904, cs201905, cs201906 
))
month_2 <- (c('2018-01', '2018-02', '2018-03', '2018-04', '2018-05', '2018-06' , '2018-07' , '2018-08' , '2018-09' , '2018-10' , '2018-11' , '2018-12' , 
                   '2019-01' , '2019-02' , '2019-03' , '2019-04' , '2019-05' , '2019-06'))
cs_2.data <- data.frame(month_2, cs_month_2)
cs_month_2 <- as.vector(t(cs_month_2))
names(cs_2.data) <- c("month", "prop.of.subscribers")

cs_month <- rbind(cs_1.data, cs_2.data)

age_month <- c(age201706, age201707, age201708, age201709, age201710, age201711, age201712,
               age201801, age201802, age201803, age201804, age201805, age201806, age201807, age201808, age201809, age201810, age201811, age201812, 
               age201901, age201902, age201903, age201904, age201905, age201906)
duration_month <- c(time201706, time201707, time201708, time201709, time201710, time201711, time201712,
                    time201801, time201802, time201803, time201804, time201805, time201806, time201807, time201808, time201809, time201810, time201811, time201812, 
                    time201901, time201902, time201903, time201904, time201905, time201906)

mon <- c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

month <- (c('2017-06', '2017-07', '2017-08', '2017-09', '2017-10', '2017-11', '2017-12', 
                   '2018-01', '2018-02', '2018-03', '2018-04', '2018-05', '2018-06' , '2018-07' , '2018-08' , '2018-09' , '2018-10' , '2018-11' , '2018-12' , 
                   '2019-01' , '2019-02' , '2019-03' , '2019-04' , '2019-05' , '2019-06'))
month_year <- as.Date((c('2017-06-01', '2017-07-01', '2017-08-01', '2017-09-01', '2017-10-01', '2017-11-01', '2017-12-01', 
                         '2018-01-01', '2018-02-01', '2018-03-01', '2018-04-01', '2018-05-01', '2018-06-01' , '2018-07-01' , '2018-08-01' , '2018-09-01' , '2018-10-01' , '2018-11-01' , '2018-12-01' , 
                         '2019-01-01' , '2019-02-01' , '2019-03-01' , '2019-04-01' , '2019-05-01' , '2019-06-01')))
mon_range <- data.frame(mon, month)
colnames(mon_range) <- c("range", "month")
age_duration_gender.data <- data.frame(month, month_year, age_month, duration_month)
age_duration_gender.data <- merge(age_duration_gender.data, cs_month, by = "month", all = TRUE)
age_duration_gender.data <- merge(age_duration_gender.data, mon_range, by = "month", all = TRUE)
colnames(age_duration_gender.data) <- c("Month", "month", "Average Age", "Trip Duration (in minutes)", "Proportion of Subscribers", "range")

### shinyApp
ui <- fluidPage(
  titlePanel("Era of Ford GoBike"),
  # Sidebar with a slider input for number of bins 
    mainPanel(
      tabsetPanel(
      tabPanel("The Demographic Trend of GoBikers",
               sliderInput(inputId = "slider",
                           label = "Slide to preferred month and year",
                           min = as.Date(c('2017-06-01')),
                           max = as.Date(c('2019-06-01')),
                           value = as.Date(c('2017-06-01')),
                           timeFormat = "%b %Y",
                           step = 31,
                           animate = TRUE),
               plotlyOutput(outputId = "monthly.trend"),
               textOutput("SliderText")),
      tabPanel("Popular itinerary/destinations",
               textOutput(outputId = "notes"),
               sliderInput(inputId = "slide",
                           label = "Slide to preferred month and year",
                           min = as.Date(c('2017-06-01')),
                           max = as.Date(c('2019-06-01')),
                           value = as.Date(c('2017-06-01')),
                           timeFormat = "%b %Y",
                           step = 31,
                           animate = 
                             animationOptions(interval = 1500, loop = FALSE)),
               plotOutput(outputId = "monthly.destination.start"),
               plotOutput(outputId = "monthly.destination.end")
      )))
)

# formats cells to date format
end_month_range <- function(y) {
  y <- as.POSIXlt(y)
  substring(y, 1, 10)
  as.Date(y)
}

# change dates to the first day of months
filter.month <- function(z){
  z <- as.POSIXlt(z)
  z$mday <- 1
  as.Date(z)
}

# creates a data frame of selected dates
filter.month.data <- function(m){
  as.data.frame(combined.data3 %>%
    filter(start_time == m))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$monthly.trend <- renderPlotly({
    age_duration_gender.data %>%
      filter(between(month, as.Date('2017-06-01'), filter.month(input$slider))) %>%
      ggplot(aes(x = month,
                 y = `Average Age`,
                 size = `Trip Duration (in minutes)`,
                 color = `Proportion of Subscribers`)) + 
      scale_y_continuous("Median Age", 
                         limits = c(34, 40)) +
      ggtitle("Monthly Trend of GoBikers over time") + 
      xlab("Month") +
      ylab("Average Age") +
      theme(axis.text.x = element_text(angle=270)) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %Y")  +
      geom_point() 
 
    })
  
  output$monthly.destination.start <- renderPlot({
    sf +
      stat_density2d(
        aes(x = start_station_longitude, y = start_station_latitude, alpha = 0.2),
        size = 0.01, bins = 30, data = filter.month.data(filter.month(input$slide)),
        geom = "polygon") + 
      scale_fill_gradient(low = "green", high = "red") + 
      geom_point(data = filter.month.data(filter.month(input$slide)),
                 aes(color = user_type, x = start_station_longitude, y = start_station_latitude), size = 1.2) +
      guides(alpha = FALSE) +
      ggtitle("Starting points")
  })
  
  output$monthly.destination.end <- renderPlot({
    sf +
      stat_density2d(
        aes(x = end_station_longitude, y = end_station_latitude, alpha = 0.2),
        size = 0.01, bins = 30, data = filter.month.data(filter.month(input$slide)),
        geom = "polygon") + 
      scale_fill_gradient(low = "green", high = "red") + 
      geom_point(data = filter.month.data(filter.month(input$slide)),
                 aes(color = user_type, x = end_station_longitude, y = end_station_latitude), size = 1.2) +
      guides(alpha = FALSE) +
      ggtitle("Ending points")
  })
  
  output$notes <- renderText({
    paste("The following maps show starting and ending points over time. Due to the size of data, it may take some time to reload the maps.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

