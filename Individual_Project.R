library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0", "pink", "green", "yellow", "orange", "red")


Data_for_January <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_01.csv")
Data_for_February <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_02.csv")
Data_for_March <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_03.csv")
Data_for_April <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_04.csv")
Data_for_May <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_05.csv")
Data_for_June <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_06.csv")
Data_for_July <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_07.csv")
Data_for_August <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_08.csv")
Data_for_September <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_09.csv")
Data_for_October <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_10.csv")
Data_for_November <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_11.csv")
Data_for_December <- read.csv("/Users/divyanshsingh/R Individual Project/chicago_taxi_trips_2016_12.csv")

           
           data_2016 <- rbind(Data_for_January, Data_for_February, Data_for_March, Data_for_April,Data_for_May, Data_for_June,Data_for_July,Data_for_August,Data_for_September,Data_for_October,Data_for_November,Data_for_December) %>% 
             rename(Lon = pickup_longitude, Lat = pickup_latitude, Base = pickup_community_area)
           
           
           data_2016$trip_start_timestamp <- gsub('-', '/', data_2016$trip_start_timestamp)
           data_2016$trip_start_timestamp <- as.POSIXct(data_2016$trip_start_timestamp, format = "%Y/%m/%d %H:%M:%S")
           
           data_2016$Time <- format(as.POSIXct(data_2016$trip_start_timestamp, format = "%Y/%m/%d %H:%M:%S"), format="%H:%M:%S")
           
           data_2016$trip_start_timestamp <- ymd_hms(data_2016$trip_start_timestamp)
           
           data_2016$day <- factor(day(data_2016$trip_start_timestamp))
           data_2016$month <- factor(month(data_2016$trip_start_timestamp, label = TRUE))
           data_2016$year <- factor(year(data_2016$trip_start_timestamp))
           data_2016$dayofweek <- factor(wday(data_2016$trip_start_timestamp, label = TRUE))           
           
           data_2016$hour <- factor(hour(hms(data_2016$Time)))
           data_2016$minute <- factor(minute(hms(data_2016$Time)))
           data_2016$second <- factor(second(hms(data_2016$Time)))
           
           
           #PLOTTING TRIPS BY HOURS IN THE DAY
           
           hour_data <- data_2016 %>%
             group_by(hour) %>%
             dplyr::summarize(Total = n()) 
           datatable(hour_data)
           
           ggplot(hour_data, aes(hour, Total)) + 
             geom_bar( stat = "identity", fill = "steelblue", color = "red") +
             ggtitle("Trips Every Hour") +
             theme(legend.position = "none") +
             scale_y_continuous(labels = comma)
           
           month_hour <- data_2016 %>%
             group_by(month, hour) %>%
             dplyr::summarize(Total = n())
           
           ggplot(month_hour, aes(hour, Total, fill = month)) + 
             geom_bar( stat = "identity") +
             ggtitle("Trips by Hour and Month") +
             scale_y_continuous(labels = comma)
           
           # PLOTTING DATA BY TRIPS DURING EVERY DAY OF THE MONTH
           
           day_group <- data_2016 %>%
             group_by(day) %>%
             dplyr::summarize(Total = n()) 
           datatable(day_group)
           
           ggplot(day_group, aes(day, Total)) + 
             geom_bar( stat = "identity", fill = "steelblue") +
             ggtitle("Trips Every Day") +
             theme(legend.position = "none") +
             scale_y_continuous(labels = comma)
           
           day_month_group <- data_2016 %>%
             group_by(month, day) %>%
             dplyr::summarize(Total = n())
           
           ggplot(day_month_group, aes(day, Total, fill = month)) + 
             geom_bar( stat = "identity") +
             ggtitle("Trips by Day and Month") +
             scale_y_continuous(labels = comma) +
             scale_fill_manual(values = colors)
           
           #NUMBER OF TRIPS TAKING PLACE DURING MONTHS IN A YEAR
           
           month_group <- data_2016 %>%
             group_by(month) %>%
             dplyr::summarize(Total = n()) 
           datatable(month_group)
           
           ggplot(month_group, aes(month, Total, fill = month)) + 
             geom_bar( stat = "identity") +
             ggtitle("Trips by Month") +
             theme(legend.position = "none") +
             scale_y_continuous(labels = comma) +
             scale_fill_manual(values = colors)
           
           month_weekday <- data_2016 %>%
             group_by(month, dayofweek) %>%
             dplyr::summarize(Total = n())
           
           ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
             geom_bar( stat = "identity", position = "dodge") +
             ggtitle("Trips by Day and Month") +
             scale_y_continuous(labels = comma) +
             scale_fill_manual(values = colors)
           
           ggplot(data_2016, aes(Base)) + 
             geom_bar(fill = "darkred") +
             scale_y_continuous(labels = comma) +
             ggtitle("Trips by Bases")
           
           ggplot(data_2016, aes(Base, fill = dayofweek)) + 
             geom_bar(position = "dodge") +
             scale_y_continuous(labels = comma) +
             ggtitle("Trips by Bases and DayofWeek") +
             scale_fill_manual(values = colors)
           
           day_and_hour <- data_2016 %>%
             group_by(day, hour) %>%
             dplyr::summarize(Total = n())
           
           datatable(day_and_hour)
           
           ggplot(day_month_group, aes(day, month, fill = Total)) +
             geom_tile(color = "white") +
             ggtitle("Heat Map by Month and Day")
           
           
           
           
           #FINDING OUT THE NUMBER OF TRIPS BY BASES
           
           month_base <-  data_2016 %>%
             group_by(Base, month) %>%
             dplyr::summarize(Total = n()) 
           
           day0fweek_bases <-  data_2016 %>%
             group_by(Base, dayofweek) %>%
             dplyr::summarize(Total = n()) 
           
           ggplot(month_base, aes(Base, month, fill = Total)) +
             geom_tile(color = "white") +
             ggtitle("Heat Map by Month and Bases")
           

           
           
           
           
           #HEATMAP VISUALIZATION OF DAY, HOUR AND MONTH

ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
geom_tile(color = "white") +
ggtitle("Heat Map by Bases and Day of Week")           
           
           
           
