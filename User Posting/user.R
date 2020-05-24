#library(readxl)
#library(anytime)
#user <- read_excel("C:/Users/jared_000/Desktop/SGH/1st Semester/Data Visualization/user.xlsx")
#
#user <- user[order(user$dateline),]
#user[1,2] <- 1281496979
#
#user <- as.data.frame(user)
#
#system.time(
#  user[,3] <- anytime(user[,2])
#)
#colnames(user)[colnames(user)=="V3"] <- "date"
#saveRDS(user, file="C:/Users/jared_000/Desktop/SGH/1st Semester/Data Visualization/user.rds")

#START NEW CODE#
library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(lubridate)

user <- readRDS(file = "C:/Users/jared_000/Desktop/SGH/1st Semester/Data Visualization/user.rds")

chosen = 792
chosenu = "Soul"
user1 <- user[user$userid == chosen,]

grouped <- user1['date']
group <- grouped %>% group_by(Day=format(grouped$date, "%Y-%m-%d"))
group['date'] = NULL
group <- count(group)

groupw <- grouped %>% group_by(Day=format(grouped$date, "%Y-%m-%d"))
groupw['date'] = NULL
groupw$week <- cut(as.Date(groupw$Day), breaks = "week")
groupw %>% 
  group_by(week) %>% 
  summarise(n = n()) -> 
  groupw

groupm <- grouped %>% group_by(Month=format(grouped$date, "%m-%Y"))
groupm$Month <- as.Date(paste('01', groupm$Month), "%d %m-%Y")
groupm['date'] = NULL
groupm <- count(groupm)


#, limits = c(as.Date('10-01-2010', "%m-%d-%Y"), as.Date('01-01-2012', "%m-%d-%Y"))

ggplot(group, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(Day), y=n)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(limits = c(0, 160), expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Day For User",
       subtitle = "(10-08-2010 to 01-30-2019)",
       caption = "Data from forums.novociv.org",
       tag = chosenu,
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")

ggplot(groupw, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(week), y=n)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(limits = c(0, 160), expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Week For User",
       subtitle = "(10-08-2010 to 01-30-2019)",
       caption = "Data from forums.novociv.org",
       tag = chosenu,
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")

ggplot(groupm, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(Month, "%Y-%m-%d"), y=n)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Month For User",
       subtitle = "(10-08-2010 to 01-30-2019)",
       caption = "Data from forums.novociv.org",
       tag = chosenu,
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")


