install.packages("tidyverse")
library("tidyverse")
library("ggthemes")
library("lubridate")
library("DT")
library("scales")

getwd()
setwd("C:/Users/jatin/Downloads/Uber-dataset")
getwd()

apr_data <- read.csv("uber-raw-data-apr19.csv")
may_data <- read.csv("uber-raw-data-may19.csv")
jun_data <- read.csv("uber-raw-data-jun19.csv")
jul_data <- read.csv("uber-raw-data-jul19.csv")
aug_data <- read.csv("uber-raw-data-aug19.csv")
sep_data <- read.csv("uber-raw-data-sep19.csv")

data_2019 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

data_2019$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2019$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")


#data_2019$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2019$day <- factor(day(data_2019$Date.Time))
data_2019$month <- factor(month(data_2019$Date.Time, label = TRUE))
data_2019$year <- factor(year(data_2019$Date.Time))
data_2019$dayofweek <- factor(wday(data_2019$Date.Time, label = TRUE))

data_2019$hour <- factor(hour(hms(data_2019$Time)))
data_2019$minute <- factor(minute(hms(data_2019$Time)))
data_2019$second <- factor(second(hms(data_2019$Time)))

hour_data <- data_2019 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data_2019 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

day_group <- data_2019 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)


ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

