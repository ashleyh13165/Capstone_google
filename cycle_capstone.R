#To start we need to instal the packages from R that we will need.

install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('readr')
install.packages('dplyr')
install.packages('ggplot2')

#Next we need to load them by using the Library funtion
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(ggplot2)

#Now I need to upload my data files for my computer to R
`2021.01.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.01.td.csv")
`2021.02.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.02.td.csv")
`2021.03.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.03.td.csv")
`2021.04.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.04.td.csv")
`2021.05.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.05.td.csv")
`2021.06.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.06.td.csv")
`2021.07.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.07.td.csv")
`2021.08.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.08.td.csv")
`2021.09.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.09.td.csv")
`2021.10.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.10.td.csv")
`2021.11.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.11.td.csv")
`2021.12.td` <- read.csv("C:/Users/ashle/OneDrive/Desktop/bike_trip_data/2021.12.td.csv")


#Now I am going to rename the files to make it easier to know what they are.
Jan2021 <-`2021.01.td`
Feb2021 <-`2021.02.td`
Mar2021 <-`2021.03.td`
April2021 <-`2021.04.td`
May2021 <-`2021.05.td`
June2021 <-`2021.06.td`
July2021 <-`2021.07.td`
Aug2021 <-`2021.08.td`
Sep2021<-`2021.09.td`
Oct2021<-`2021.10.td`
Nov2021<-`2021.11.td`
Dec2021<-`2021.12.td`

#Now I check to make sure all the data is formatted proprtly.
str(Jan2021)
str(Feb2021)
str(Mar2021)
str(April2021)
str(May2021)
str(June2021)
str(July2021)
str(Aug2021)
str(Sep2021)
str(Oct2021)
str(Nov2021)
str(Dec2021)

#Now I am going to take all of my data frames and combine them using bind_row to make a new data frame and name it.
ride_2021 <- bind_rows(Jan2021, Feb2021, Mar2021, April2021, May2021, June2021, July2021, Aug2021, Sep2021, Oct2021, Nov2021, Dec2021)
                      
#Next I am going to use wday to show what day of the week each tip was made on.
ride_2021$day_of_week <- wday(ride_2021$started_at, label = T, abbr = T)

#Next I am going to make a new column for what month each ride took place.
ride_2021$month <- format(as.Date(ride_2021$started_at), '%m')

#Next I am going to make a new column for the hour that the trip started
ride_2021$starting_hour <- format(as.POSIXct(ride_2021$started_at), '%H')

#Now I am going to figure out the trip durations.
ride_2021$trip_duration <- difftime(ride_2021$ended_at, ride_2021$started_at, units ='sec')

#Now I will make a new data frame for all the cleaned data.I will also be taking any trips that were 0 seconds or less out of that data frame.
ride_2021<- ride_2021[!(ride_2021$trip_duration<=0),]

#Now I am going to remove any blanks from the start station name and end station name.

ride_2021 <- ride_2021 %>%
  filter(
    !(is.na(start_station_name) |
        start_station_name == "")) %>% 
  
  filter(
    !(is.na(end_station_name) |
        end_station_name == ""))

#Here I am making sure there are no duplicutes in my clean data by running a ride_id_check.
ride_id_check <- ride_2021 %>%
  count(ride_id) %>%
  filter(n > 1)


#Next I am going to start my graphs to show the two types of riders, casual and member. I will have one for rides per day of the week and one for rides per month. 
#The first graph is days of the week.
options(scipen = 999)
ggplot(data = ride_2021) +
  aes(x = day_of_week, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Day of week', y = 'Number of rides', fill = 'Member type', title = 'Number of rides by member type',subtitle = 'One week of rides')

#This second graph is ride numbers by month of the year
ggplot(data=ride_2021)+
  aes(x=month,fill=member_casual) +
  geom_bar(position = 'dodge')+
  labs(title= 'Number of rides a month shown by rider type')
