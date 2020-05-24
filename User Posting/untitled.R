#Personal Charts

library(anytime)
library(ggplot2)
library(dplyr)
library(scales)

setwd("C:/Users/jared_000/Desktop/SGH/Thesis")
post <- read.csv("post_new.csv", header=T, na.strings=c("","NA"))

post$dateline[post$postid==20700] <- 1282196303

system.time(
  post$date <- anytime(post$dateline)
)

chosen = 851
chosenu = "Roty"
user1 <- post[post$userid == chosen,]

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
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Day For User",
       subtitle = "(10-08-2010 to 05-27-2019)",
       caption = "Data from forums.novociv.org",
       tag = chosenu,
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")

ggplot(groupw, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(week), y=n)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Week For User",
       subtitle = "(10-08-2010 to 05-27-2019)",
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



stats <- read.csv("stats.csv")

stats$date <- anytime(stats$dateline)
stats$Day <- format(stats$date, "%Y-%m-%d")

ggplot(stats, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(Day), y=ausers)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(limits=c(0,NA), expand = c(0, 0))+
  labs(title = "Newciv Active Users per Day",
       subtitle = "(08-2010 to 05-2019)",
       caption = "Data from forums.novociv.org",
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")
