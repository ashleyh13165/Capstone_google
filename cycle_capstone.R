#I
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')

#Load the packages
library(tidyverse)
library(janitor)
library(lubridate)

#import the data from my dektop to RStudio
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

#renaming data to make it easier to identify

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

#str(dataset_name)
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

#Creating a new dataset name <- binding rows(all_your_datasets)
ride_2021 <- bind_rows(Jan2021, Feb2021, Mar2021, April2021, May2021, June2021, July2021, Aug2021, Sep2021, Oct2021, Nov2021, Dec2021)
                      
#Cleaning the names.
ride_2021 <- clean_names(merged_df)

#use removing_empty(dataset_name, by leaving c() empty, it selects rows & columns) to remove any empty spaces
remove_empty(ride_2021, which = c())

#What wday() doesis extract DAY from the Dataframe with the date format

#df_name$your_new_column_name <- wday(df_name$select_column, label = T/F, abbr = T/F)
ride_2021$day_of_week <- wday(ride_2021$started_at, label = T, abbr = T)

#What POSIXct does is extracts a certain TIME HOUR FORMAT from an Dataframe

#df_name$your_new_column_name <- format(as.POSIXct(df_name$select_column, '%time_format')
ride_2021$starting_hour <- format(as.POSIXct(ride_2021$started_at), '%H')

#To convert one datatype into another we used format(). This has to be used with datatypes/date formats.Using as.Date () extracts the DATE from the Dataframe  with the date format. Used in format()

#df_name$your_new_column_name <- format(as.Date(df_name$select_column), '%date_format')
ride_2021$month <- format(as.Date(ride_2021$started_at), '%m')

# In a Dataframe difftime() calculates the time difference between one column and another with a date format.

#df_name$your_new_column_name <- difftime(df_name$usually_end_time_column, df_name$usually_start_time_column, units = 'your_desired_unit')
ride_2021$trip_duration <- difftime(ride_2021$ended_at, ride_2021$started_at, units ='sec')

#Here i will take the merged_df and make a new dataframe named cleaned_df that does not contain any trip_durations of 0 seconds or less. '!' means is not equals to

cleaned_ride_2021 <- ride_2021[!(ride_2021$trip_duration<=0),]

# Here we will be displayed alongside the x- axis.I will use the (position = 'dodge') which will inform the 2nd value (in this case member type) to 'dodge' and not stack ontop. 

ride_2021 <- ride_2021 %>%
  filter(
    !(is.na(start_station_name) |
        start_station_name == "")
  ) %>% 
  
  filter(
    !(is.na(end_station_name) |
        end_station_name == "")
  )

ride_id_check <- ride_2021 %>%
  count(ride_id) %>%
  filter(n > 1)


fwrite(
 ride_2021, 
  "C:\\Users\\ashle\\Documents\\bike_trip_data.csv", 
  col.names = TRUE,
  row.names = FALSE
)
options(scipen = 999)
ggplot(data = ride_2021) +
  aes(x = day_of_week, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Day of week', y = 'Number of rides', fill = 'Member type', title = 'Number of rides by member type',subtitle = 'One week of rides')+
ggsave("number_of_rides_by_member_type.png")

ggplot(data=ride_2021)+
  aes(x=month,fill=member_casual) +
  geom_bar(position = 'dodge')+
  labs(title= 'Number of rides a month shown by rider type')

ggsave("number_of_rides_monthly_by_type.png")





#count will let you count, group & sort the unique values from a dataframe
#filter just filters out data that meets (==) or does not meet(!=) the requirement

                        
annaul_member_2021 <- filter(ride_2021, member_casual=='member')
count(annual_member_2021, start_station_name, sort = T)
count(annual_member_2021, end_station_name, sort = T)
              
casual_member_2021 <- filter(ride_2021, member_casual=='casual')
count(casual_member_2021, start_station_name, sort = T)
count(casual_member_2021, end_station_name, sort = T)
                        


                       
