library(jsonlite)
library(stringr)
library(tidyr)
library(chron)
library(dplyr)
library(lubridate)
library(ggplot2)

checkin_freq <- read.csv("C:/Users/31586/Desktop/yelp_dataset/checkin_freq.csv",header=TRUE)
head(checkin_freq,10)

#daynight
sum_vec_daynight <- c(sum(checkin_freq$Checkin_day),
                      sum(checkin_freq$Checkin_night))
pie(sum_vec_daynight,
    labels=c("day","night"),
    col=c("red","blue"),
    main="Frequency checkin during day/night")

#weekday
#date_vec <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
date_vec <- factor(date_vec,date_vec)
sum_vec_weekday <- c(sum(checkin_freq$Checkin_Monday),
                     sum(checkin_freq$Checkin_Tuesday),
                     sum(checkin_freq$Checkin_Wednesday),
                     sum(checkin_freq$Checkin_Thursday),
                     sum(checkin_freq$Checkin_Friday),
                     sum(checkin_freq$Checkin_Saturday),
                     sum(checkin_freq$Checkin_Sunday))
df.sum_weekday <- data.frame(date_vec,sum_vec_weekday)
ggplot(df.sum_weekday,aes(x=date_vec,y=sum_vec_weekday))+
  geom_col()+
  geom_text(aes(label=sum_vec_weekday),vjust=-0.35,size=3.3)+
  coord_cartesian(ylim=c(850000,1750000))+
  ggtitle("Frequency checkin for weekdays")+
  labs(x="Weekday",y="Frequency count")

#month
month_vec <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_vec <- factor(month_vec,month_vec)
sum_vec_month <- c(sum(checkin_freq$Checkin_Jan),
                   sum(checkin_freq$Checkin_Feb),
                   sum(checkin_freq$Checkin_Mar),
                   sum(checkin_freq$Checkin_Apr),
                   sum(checkin_freq$Checkin_May),
                   sum(checkin_freq$Checkin_Jun),
                   sum(checkin_freq$Checkin_Jul),
                   sum(checkin_freq$Checkin_Aug),
                   sum(checkin_freq$Checkin_Sep),
                   sum(checkin_freq$Checkin_Oct),
                   sum(checkin_freq$Checkin_Nov),
                   sum(checkin_freq$Checkin_Dec))
df.sum_month <- data.frame(month_vec,sum_vec_month)
ggplot(df.sum_month,aes(x=month_vec,y=sum_vec_month))+
  geom_col()+
  geom_text(aes(label=sum_vec_month),vjust=-0.35,size=3.3)+
  coord_cartesian(ylim=c(600000,770000))+
  ggtitle("Frequency checkin for all month")+
  labs(x="Month",y="Frequency count")

#year
year_vec <- as.character(2010:2018)
year_vec <- factor(year_vec,year_vec)
sum_vec_year <- c(sum(checkin_freq$Checkin_2010),
                  sum(checkin_freq$Checkin_2011),
                  sum(checkin_freq$Checkin_2012),
                  sum(checkin_freq$Checkin_2013),
                  sum(checkin_freq$Checkin_2014),
                  sum(checkin_freq$Checkin_2015),
                  sum(checkin_freq$Checkin_2016),
                  sum(checkin_freq$Checkin_2017),
                  sum(checkin_freq$Checkin_2018))
df.sum_year <- data.frame(year_vec,sum_vec_year)
ggplot(df.sum_year,aes(x=year_vec,y=sum_vec_year))+
  geom_col()+
  geom_text(aes(label=sum_vec_year),vjust=-0.35,size=3.3)+
  ggtitle("Frequency checkin for all years")+
  labs(x="Year",y="Frequency count")
