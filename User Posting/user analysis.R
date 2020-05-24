setwd("C:/Users/jared_000/Desktop/SGH/Thesis")
options(scipen=999)
library(anytime)
library(dplyr)
library(data.table)
library(BCA)
library(ggplot2)
library(scales)
library(foreign)

#read data
dane <- read.csv("user.csv", header=T, na.strings=c("","NA"))
post <- read.csv("post.csv", header=F, na.strings=c("","NA"))
inflation <- read.csv("inflation.csv", header=T, na.strings=c("","NA"))
mention <- read.csv("mention.csv", header=T, na.strings=c("","NA"))
specialbuy <- read.csv("specialbuy.csv", header=T, na.strings=c("","NA"))
stats <- read.csv("stats.csv", header=T, na.strings=c("","NA"))


post <- post[2:900965,]
colnames(post)[colnames(post)=="V1"] <- "postid"
colnames(post)[colnames(post)=="V2"] <- "threadid"
colnames(post)[colnames(post)=="V3"] <- "userid"
colnames(post)[colnames(post)=="V4"] <- "dateline"
colnames(post)[colnames(post)=="V5"] <- "post_thanks_amount"

system.time(
  post$date <- anytime(post$dateline)
)
system.time(
  inflation$date <- anytime(inflation$dateline)
)
system.time(
  mention$date <- anytime(mention$dateline)
)
system.time(
  specialbuy$date <- anytime(specialbuy$dateline)
)
system.time(
  stats$date <- anytime(stats$dateline)
)

chosen = 17
chosenu = "Kaos"
pu <- post[post$userid == chosen,]
mu <- mention[mention$mentionedid == chosen,]

grouped <- pu['date']
group <- grouped %>% group_by(Day=format(grouped$date, "%Y-%m-%d"))
group['date'] = NULL
group <- count(group)

groupedt <- post['date']
groupt <- groupedt %>% group_by(Day=format(groupedt$date, "%Y-%m-%d"))
groupt['date'] = NULL
groupt <- count(groupt)
groupt <- groupt[2:3088,]

groupedm <- mention['date']
groupm <- groupedm %>% group_by(Day=format(groupedm$date, "%Y-%m-%d"))
groupm['date'] = NULL
groupm <- count(groupm)

groupedmu <- mu['date']
groupmu <- groupedmu %>% group_by(Day=format(groupedmu$date, "%Y-%m-%d"))
groupmu['date'] = NULL
groupmu <- count(groupmu)

groupeds <- specialbuy['date']
groups <- groupeds %>% group_by(Day=format(groupeds$date, "%Y-%m-%d"))
groups['date'] = NULL
groups <- count(groups)

post$day <- format(post$date, "%Y-%m-%d")
stats$day <- format(stats$date, "%Y-%m-%d")
mention$day <- format(mention$date, "%Y-%m-%d")
inflation$day <- format(inflation$date, "%Y-%m-%d")
specialbuy$day <- format(specialbuy$date, "%Y-%m-%d")

DT <- data.table(post)
unique <- DT[, .(number = length(unique(userid))), by = day]

n_occur <- data.frame(table(stats$day))
#gives you a data frame with a list of ids and the number of times they occurred.
n_occur[n_occur$Freq > 1,]

stats <- stats[-1063,]

stats2 <- stats[,c(3,4,5,7)]

main <- data.frame(stats[,7])
colnames(main)[colnames(main)=="stats...7."] <- "Day"

main <- merge(x = main, y = group, by.x='day', by.y='Day', all.x = TRUE)
colnames(main)[colnames(main)=="n"] <- "upost"
main <- merge(x = main, y = groupm, by.x='day', by.y='Day', all.x = TRUE)
colnames(main)[colnames(main)=="n"] <- "mention"
main <- merge(x = main, y = groupmu, by.x='day', by.y='Day', all.x = TRUE)
colnames(main)[colnames(main)=="n"] <- "umention"
main <- merge(x = main, y = groups, by.x='day', by.y='Day', all.x = TRUE)
colnames(main)[colnames(main)=="n"] <- "trans"
main <- merge(x = main, y = groupt, by.x='day', by.y='Day', all.x = TRUE)
colnames(main)[colnames(main)=="n"] <- "post"
main <- merge(x = main, y = inflation, by.x='day', by.y='day', all.x = TRUE)
main$logid <- NULL
main$dateline <- NULL
main$date <- NULL
main <- merge(x = main, y = unique, by.x='day', by.y='day', all.x = TRUE)
colnames(main)[colnames(main)=="number"] <- "unique"
main <- merge(x = main, y = stats2, by.x='day', by.y='day', all.x = TRUE)
main$upost[is.na(main$upost)] <- 0
main$mention[is.na(main$mention)] <- 0
main$umention[is.na(main$umention)] <- 0
main$trans[is.na(main$trans)] <- 0
main$post[is.na(main$post)] <- 0
main$unique[is.na(main$unique)] <- 0

variable.summary(main)

saveRDS(main, file="C:/Users/jared_000/Desktop/SGH/Thesis/main.rds")


main$week <- cut(as.Date(main$day), breaks = "week")
weekp <- aggregate(main$post, by=list(Category=main$week), FUN=sum)
weeknp <- aggregate(main$npost, by=list(Category=main$week), FUN=sum)
weekp$nx <- weeknp$x

ggplot(weekp) +
  geom_line(aes(x=as.Date(Category), y=x), colour="red") +
  geom_line(aes(x=as.Date(Category), y=nx), colour="blue") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  labs(title = "Newciv Total Posts per Week, Log vs Count",
       subtitle = "(10-08-2010 to 04-15-2019)",
       caption = "Data from forums.novociv.org",
       x = "Date",
       y = "Count of Posts")

inflation2 <- inflation[inflation$dateline > 1554076800,]

ggplot(inflation2) +
  geom_line(aes(x=as.Date(day), y=totalactivewealth), colour="red") +
  geom_line(aes(x=as.Date(day), y=totalwealth), colour="blue") +
  geom_hline(yintercept=966742, linetype="dashed", color = "black") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 month", minor_breaks = "1 day")+
  labs(title = "Newciv Inflation, Total vs Actual",
       subtitle = "(10-08-2010 to 06-25-2019)",
       caption = "Data from forums.novociv.org",
       x = "Date",
       y = "inflation")

ggplot(stats) +
  geom_line(aes(x=as.Date(day), y=ausers), colour="red") +
  geom_hline(yintercept=18, linetype="dashed", color = "black") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "1 year", minor_breaks = "1 month")+
  labs(title = "Newciv Active Users",
       subtitle = "(10-08-2010 to 06-25-2019)",
       caption = "Data from forums.novociv.org",
       x = "Date",
       y = "Active Users")

write.foreign(main, "C:/Users/jared_000/Desktop/SGH/Thesis/main.txt", 
              "C:/Users/jared_000/Desktop/SGH/Thesis/main.sas", package="SAS")

test <- user[is.na(user$ipaddress),]
user$ponzi[user$ponzi!=0] <- 1
user$styleid <- as.factor(user$styleid)


write.foreign(user, "C:/Users/jared_000/Desktop/SGH/Thesis/user2.txt", 
              "C:/Users/jared_000/Desktop/SGH/Thesis/user2.sas", package="SAS")

user$Active <- ifelse(user$lastvisit>1546905600, 1, 0)
user$inactive <- round((1554742612 - user$lastactivity)/(60*60*24))
user$days <- round((1554742612 - user$joindate)/(60*60*24))
user$life <- user$days - user$inactive


library(BCA)
library(relimp)
library(car)
library(RcmdrMisc)
user$log.posts <- with(user, log(posts+.001))
user$log.timespentonline <- with(user, log(timespentonline+.001))
user$log.ucash <- with(user, log(abs(ucash)+.001))
user$log.bank <- with(user, log(abs(market_bank1)+.001))

mod <- glm(Active ~ log.posts + log.ucash + log.timespentonline + log.bank + styleid + post_thanks_thanked_posts + customtitle + 
             market_gift_access + usergroupid + dbtech_usertag_mentioncount, data = user, family = binomial(logit))
Anova(mod)

mod <- glm(Active ~ ucash + timespentonline + styleid + post_thanks_thanked_posts + customtitle + 
             usergroupid, data = user, family = binomial(logit))
Anova(mod)
summary(mod)


lmod <- lm(inactive ~ ucash + posts, 
           data = user)
Anova(lmod)
summary(lmod)

lmod2 <- step(lm(inactive ~ userid + usergroupid + styleid + showvbcode + showbirthday + customtitle + daysprune + posts + 
                   timezoneoffset + pmpopup + maxposts + startofweek + referrerid + pmtotal + pmunread + profilevisits + 
                   friendcount + friendreqcount + vmunreadcount + ucash + wikiedits + wikicreations + gameroom_cash + 
                   timespentonline + market_donate_history + market_gift_access + market_gifts + market_purchases + 
                   market_refund + market_gambling + market_bank1 + market_coupon + dbtech_usertag_mentioncount + 
                   dbtech_usertag_tagcount + dbtech_usertag_mentions + dbtech_usertag_tags + dbtech_usertag_quotecount + 
                   dbtech_usertag_quotes + ponzi + partyid + post_thanks_user_amount + post_thanks_thanked_posts + 
                   post_thanks_thanked_times, data=user))
Anova(lmod2)
summary(lmod2)

library(ggplot2)
ggplot() +
  geom_point(
    data = user, aes(inactive, posts), 
    shape=21, fill = "red"
  )
  labs(title=paste("Activity vs Posts"))
  
ggplot() +
    geom_point(
      data = user, aes(inactive, ucash), 
      shape=21, fill = "red"
    )
  labs(title=paste("Activity vs Posts"))
  
ggplot() +
    geom_point(
      data = user, aes(inactive, days), 
      shape=21, fill = "red"
    )
  labs(title=paste("Activity vs Posts"))
  
ggplot() +
    geom_point(
      data = user, aes(life, timespentonline), 
      shape=21, fill = "red"
    )
  labs(title=paste("Activity vs Posts"))