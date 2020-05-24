library(iptools)
library(rgeolocate)
library(tidyverse)
library(readxl)
library(plyr); library(dplyr)
library(rworldmap)
library(ggmap)
library(rworldxtra)
library(anytime)
post <- read_excel("post.xlsx")

## grab my ips as a vector
#ips <- ip_random(1000000)
#And, format them

#Take the IP column
ips <- unlist(post[,2], use.names=FALSE)

#Geolocte them
system.time(
rgeolocate::maxmind(
ips, "~/R/GeoLite2-City.mmdb", c("city_name","longitude", "latitude")
) -> xdf
)

test <- xdf
test[,4] <- post$dateline
test[,5] <- post$ipaddress
colnames(test)[colnames(test)=="V4"] <- "dateline"
colnames(test)[colnames(test)=="V5"] <- "ipaddress"
saveRDS(test, file="posts.rds")
posts <- readRDS(file = "posts.rds")

test2 <- test[order(test$dateline),]
test2[1,4] <- 1281496979

system.time(
test2[,6] <- anytime(test2[,4])
)
colnames(test2)[colnames(test2)=="V6"] <- "date"

saveRDS(test2, file="posts2.rds")
posts2 <- readRDS(file = "posts2.rds")

system.time(
  rgeolocate::maxmind(
    ips, "~/R/GeoLite2-City.mmdb", c("country_name")
  ) -> countries
)
cts <- count(countries)

countries %>% 
  count("country_name") -> cts
cts[55,1] <- "Bots"
saveRDS(cts, file="countries.rds")
cts2 <- cts[order(-cts$freq),]
cts3 <- cts2[1:20,]
cts3[20,1] <- "Other"
cts3[20,2] <- sum(cts2[20:55,2])
barplot(cts3[2:20,2],1:19, log="y", space=0.2, width=0.8, names=cts3[2:20,1], las=2)
options(scipen=5)
barplot(cts3[,2],1:20, log="y", space=0.2, width=0.8, names=cts3[,1], las=2)

#user  system elapsed 
#6.04    0.02    6.05 

#xdf %>% 
#  mutate(
#    longitude = (longitude %/% 1) * 1,
#    latitude = (latitude %/% 1) * 1
#  ) %>%  
##  count(longitude, latitude) -> pts

#xdf %>% 
#  count("city_name","longitude",xdf$latitude) -> pts

pts <- count(xdf)
pts[1,1] <- "Bots"
pts[1,2] <- 0
pts[1,3] <- -55
pts2 <- aggregate(freq ~ city_name,data=pts,FUN=sum())

cities <- pts2[order(-pts2$freq),]

#PTS4 is ideal
pts4 <- ddply(pts,"city_name",summarize,longitude=(longitude),latitude=max(latitude),freq=sum(freq))
pts4 <- pts4[order(pts4$freq),]
pts4[1836,1] <- "Bots"
pts4[1836,2] <- 0
pts4[1836,3] <- -55

pts3 <- pts %>% 
  group_by(city_name,latitude,longitude) %>% 
  summarise(n = sum(n))

pts3 <- merge(pts2,pts, by = c("city_name", "freq"))

wrld <- tbl_df(map_data("world"))
wrld <- filter(wrld, region != "Antarctica")


ggplot() +
  geom_map(
    map = wrld, data = wrld, aes(long, lat, map_id=region),
    color = "grey", fill ="white", size=0.1
  ) +
  geom_point(
    data = pts[order(pts$freq),], aes(longitude, latitude, size = freq), 
    shape=21, fill = "red", color = "white", stroke=0.01
  ) +
  scale_size(name = "# IPs", label=scales::comma, range = c(2, 12)) +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_map() +
  theme(legend.justification = "center") +
  theme(legend.position = "bottom") +
  labs(title="NewCiv Post frequency 08-2010 to 10-2018") +
  annotate("text", x = 0, y = -60, label = "Bots")

ggplot() +
  geom_map(
    map = wrld, data = wrld, aes(long, lat, map_id=region),
    color = "grey", fill ="white", size=0.1
  ) +
  geom_point(
    data = pts4, aes(longitude, latitude, size = freq), 
    shape=21, fill = "red", color = "white", stroke=0.01
  ) +
  scale_size(name = "# IPs", label=scales::comma, range = c(.5, 10)) +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_map() +
  theme(legend.justification = "center") +
  theme(legend.position = "bottom") +
  labs(title="NewCiv Post frequency 08-2010 to 10-2018") +
  annotate("text", x = 0, y = -60, label = "Bots")

#create map

newmap <- getMap(resolution = "high")
plot(newmap)
#And, plot them:

ggplot(pts4) +
  geom_point(
    aes(longitude, latitude, size = freq), 
    shape=21, fill = "steelblue", color = "white", stroke=0.25
  ) +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_map() +
  theme(legend.justification = "center") +
  theme(legend.position = "bottom")
  

  
  newmap <- getMap(resolution = "low")
  
  ggmap(newmap) +
    geom_point(data = pts4, aes(x = longitude, y = latitude, size=freq), 
               shape=21, fill = "steelblue", color = "white", stroke=0.25) 

  
  newmap <- getMap(resolution = "low")
  ggmap(newmap)+geom_point(aes(x=longitude,y=latitude,size=freq),data=pts4)
  
  ggplot()+geom_point(aes(x=longitude,y=latitude,size=n),data=pts)