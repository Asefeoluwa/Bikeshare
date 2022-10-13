
#Install "readxl" package to read excel files. import tidyverse(for data cleaning), lubridate(for datetime datatypes) and ggplot2(for visualizations)
install.packages("readxl")
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

#change the working directory to the file containing the data sets
setwd("C:/Users/Asefeoluwa/Desktop/Data/Google Data analytics/Case Study_Bike/Excel files")

#read excel files into dataframes
aug_22<-read_xlsx("202208-divvy-tripdata.xlsx")
jul_22<-read_xlsx("202207-divvy-tripdata.xlsx")
jun_22<-read_xlsx("202206-divvy-tripdata.xlsx")
may_22<-read_xlsx("202205-divvy-tripdata.xlsx")
apr_22<-read_xlsx("202204-divvy-tripdata.xlsx")
mar_22<-read_xlsx("202203-divvy-tripdata.xlsx")
feb_22<-read_xlsx("202202-divvy-tripdata.xlsx")
jan_22<-read_xlsx("202201-divvy-tripdata.xlsx")
dec_21<-read_xlsx("202112-divvy-tripdata.xlsx")
nov_21<-read_xlsx("202111-divvy-tripdata.xlsx")
oct_21<-read_xlsx("202110-divvy-tripdata.xlsx")
sep_21<-read_xlsx("202109-divvy-tripdata.xlsx")
aug_21<-read_xlsx("202108-divvy-tripdata.xlsx")

#inspect sample data frame
str(aug_22)

##combine all data frames into one data frame "all_trips"##
all_trips<-rbind(aug_22,jul_22,jun_22,may_22,apr_22,mar_22,feb_22,jan_22,dec_21,nov_21,oct_21,sep_21,aug_21)

#import dplyr library to access the glimpse() function
library(dplyr)

#import "skimr" library to access skim_without_charts() function 
library(skimr)


#Exploratory Data analysis
glimpse(all_trips)#lists in a readable format number of rows , number of columns,column names,daatatypes for each column.
skim_without_charts(all_trips)


#Data cleaning
#renaming the column name "member_casual" to "usertype"
all_trips<-rename(all_trips, 
                  usertype="member_casual")
colnames(all_trips)

#Checking that the "usertype" column contains only two types, Casual riders(casual) and Annual Membership holders (member)
unique(all_trips$usertype)

#checking that the types of bicycles are reclining bikes, hand tricycles, and cargo bikes
unique(all_trips$rideable_type)
#the function seems to return three values "electric_bike" "classic_bike"  "docked_bike"


#removing ride_length column
all_trips2<-all_trips[,-14]
colnames(all_trips2)

#creating a column that contains the length of  trip in seconds and renaming it ride_length
all_trips2$ride_length<-difftime(all_trips2$ended_at, all_trips2$started_at)
str(all_trips2)


#change the datatype of the newly created column "ride_length" is numeric
is.factor(all_trips2$ride_length)
all_trips2$ride_length <- as.numeric(as.character(all_trips2$ride_length))
is.numeric(all_trips2$ride_length)
glimpse(all_trips2)


#filtering out records that contain a negative ride_length
all_trips_negative<-filter(all_trips2, ride_length<0)
data.frame(all_trips_negative)

#checking if the trips that crossover into another also have negative ride_length. In case it was entered wrong and just need to switch start and end times in the data.
#filtering for data that 
filter(all_trips2, day_of_week_difference!=0&ride_length<0)

colnames(all_trips2)

#creating a new data frame with the positive ride lengths only
all_trips3 <- subset.data.frame(all_trips2, ride_length>0, select = ride_id:ride_length)
glimpse(all_trips3)



#Descriptive Analysis of all_trips3
mean(all_trips3$ride_length)
min(all_trips3$ride_length)
max(all_trips3$ride_length)
median(all_trips3$ride_length)


#Comparing descriptive values of members to casual riders
aggregate(all_trips3$ride_length~all_trips3$usertype, FUN= mean)
aggregate(all_trips3$ride_length~all_trips3$usertype, FUN= median)
aggregate(all_trips3$ride_length~all_trips3$usertype, FUN= min)
aggregate(all_trips3$ride_length~all_trips3$usertype, FUN= max)
days<-aggregate(all_trips3$ride_length~all_trips3$usertype+all_trips3$day_of_week_end, FUN= mean)



#Change day of week start to characters
all_trips3<-all_trips3%>%
  mutate(day_of_week_start=recode(day_of_week_start
                                  ,"1"="Sunday"
                                  ,"2"="Monday"
                                  ,"3"="Tuesday"
                                  ,"4"="Wednesday"
                                  ,"5"="Thursday"
                                  ,"6"="Friday"
                                  ,"7"="Saturday"))

unique(all_trips3$day_of_week_start)
glimpse(all_trips3)


#Visualizations
all_trips3%>%
  mutate(weekday=wday(started_at,label=TRUE))%>%
  group_by(usertype, weekday)%>%
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(usertype,weekday)%>%
  ggplot(aes(x=weekday, y= number_of_rides, fill=usertype))+
    geom_col(position = "dodge")


#Displays a bar of chart comparing the daily average ride length of members to casual riders
all_trips3%>%
  mutate(weekday=wday(started_at,label=TRUE))%>%
  group_by(usertype, weekday)%>%
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(usertype,weekday)%>%
  ggplot(aes(x=weekday, y= average_duration, fill=usertype))+
  geom_col(position = "dodge")
 #Casual riders superseded regular riders on weekedns sundays and saturdays


#export the data frame all_trips3 to excel
install.packages('writexl')
library(writexl)

#too many rows for excel so exported it to csv
write_csv(all_trips3,"C:/Users/Asefeoluwa/Desktop/Data/Google Data analytics/Case Study_Bike/Excel files\\Bikeshare_trips" )
