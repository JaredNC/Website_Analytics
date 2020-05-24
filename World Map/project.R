library(ggplot2)
library(tidyverse)
library(plyr)
library(scales)
library(plotrix)
library(gridExtra)

setwd("C:/Users/jared_000/Desktop/SGH/1st Semester/Data Visualization/Data Visualization/")

posts2 <- readRDS(file = "Map-1/posts2.rds")
posts2[,7] <- 1
colnames(posts2)[colnames(posts2)=="V7"] <- "freq"

pts <- ddply(posts2,"city_name",summarize,longitude=mean(longitude),latitude=mean(latitude),freq=sum(freq))
pts <- pts[order(pts$freq),]
pts[nrow(pts),1] <- "Bots"
pts[nrow(pts),2:3] <- c(0,-55)

wrld <- tbl_df(map_data("world"))
wrld <- filter(wrld, region != "Antarctica")

ggplot() +
  geom_map(
    map = wrld, data = wrld, aes(long, lat, map_id=region),
    color = "grey", fill ="white", size=0.1
  ) +
  geom_point(
    data = pts, aes(longitude, latitude, size = freq), 
    shape=21, fill = "red", color = "white", stroke=0.01
  ) +
  scale_size(name = "# IPs", label=scales::comma, range = c(2, 12)) +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_map() +
  theme(legend.justification = "center") +
  theme(legend.position = "bottom") +
  labs(title="NewCiv Post frequency 08-2010 to 12-2018") +
  annotate("text", x = 0, y = -60, label = "Bots")

grouped <- posts2['date']
group <- grouped %>% group_by(Day=format(grouped$date, "%Y-%m-%d"))
group['date'] = NULL
group <- count(group)

ggplot(group, aes(x=x, y=y)) +
  geom_line(aes(x=as.Date(Day), y=freq)) +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  scale_y_continuous(limits=c(0,4000), expand = c(0, 0))+
  labs(title = "Newciv Total Posts per Day",
       subtitle = "(08-2010 to 12-2018)",
       caption = "Data from forums.novociv.org",
       tag = "Figure ??",
       x = "Date",
       y = "Count of Posts",
       colour = "Decades")

years <- group %>% group_by(Year=format(as.Date(group$Day), "%Y"))
years <- ddply(years,"Year",summarize,freq=sum(freq))

ggplot(years) +
  geom_bar(aes(x=Year, y=freq), stat="identity")+
  labs(title = "Newciv Total Posts per Year",
       subtitle = "(08-2010 to 12-2018)",
       caption = "Data from forums.novociv.org",
       tag = "Figure ??",
       x = "Date",
       y = "Count of Posts")

countries <- readRDS(file = "countries.rds")
cts <- countries[order(-countries$freq),]
cts2 <- cts[1:20,]
cts2[20,1] <- "Other"
cts2[20,2] <- sum(cts[20:55,2])

ggplot(cts2, aes(x=factor(country_name, levels = c(unique(cts2$country_name))), y=freq)) +
  geom_bar(stat="identity")+ 
  scale_y_continuous(trans='sqrt', labels = scales::comma, breaks = cts2$freq, expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Newciv Total Posts per Country",
       subtitle = "(08-2010 to 12-2018)",
       caption = "Data from forums.novociv.org",
       tag = "Figure ??",
       x = "Country",
       y = "Count of Posts")

gap.barplot(cts2$freq, 
            gap=c(80000,700000),
            xaxlab = cts2$country_name, 
            ytics = c(200,500,1000,3000,9000,77000,710000),
            xlab = "Country",
            ylab = "Number of Posts")
title(main="Posts per Country",sub="Gap was removed from 80,000 to 700000")

grid.table(cts2)
