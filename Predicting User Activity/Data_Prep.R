# I hate scientific notation
options(scipen=999)

# Libraries and why I use them
library(ggplot2) # Nice graphs
library(BCA) # use of variable.summary
library(dplyr) # userful data manipulation functions
library(rgeolocate) # geolocate users
library(MASS) # exponent function

# Begin by importing the .csv file which was exported by the database.
setwd("C:/Users/jared_000/Desktop/SGH/2nd Semester/Big Data/Project")
user <- read.csv("user.csv", header=T, na.strings=c("","NA"))

# The robot should not be included in the analysis
user <- user[-which(user$userid==15),]

# Dataset was created at 1558638316 unix time
user$active <- ifelse(user$lastvisit>(1558638316-(60*60*24*30)), 1, 0)
user$inactive <- round((1558638316 - user$lastactivity)/(60*60*24))
user$days <- round((1558638316 - user$joindate)/(60*60*24))
user$life <- user$days - user$inactive # Unused in this project

# Plot gives visual perspective of data
ggplot() +
  geom_point(
    data = user, aes(inactive, days), 
    shape=21, fill = "red"
  ) + 
  geom_vline(xintercept=30) + 
  labs(title = 'Days since registration vs Days since last login', subtitle = 'Every dot is a user')

head(user,20)
# Observe state of dataset
variable.summary(user)

# Remove NA columns
user <- user[, -which(colMeans(is.na(user)) > 0.1)]

# Remove irrelevant user specific info
user <- subset(user, select=-c(password,passworddate,email,emailstamp,salt))

# Remove highly predictive variables
user <- subset(user, select=-c(lastvisit,lastactivity,lastpost,lastpostid,joindate))

# Remove columns with 99-100% 0 values
zeroes <- lapply(user, function(x){ length(which(x==0))/length(x)})
zeroes[order(unlist(zeroes), decreasing=TRUE)]
user <- subset(user, select=-c(avatarrevision,profilepicrevision,sigpicrevision,threadedmode,infractiongroupid,vmmoderatedcount,
                               socgroupinvitecount,pcunreadcount,gmmoderatedcount,fbjoindate,isfirstfriend,market_steal_protect,
                               market_post_bold,market_post_italics,market_xperience,dbtech_usertag_excluded,
                               dbtech_usertag_settings,dbtech_usertag_hashcount,socgroupreqcount,market_username_subscript,
                               market_threadstrike,market_usertitle_subscript,market_bank2,adminoptions,businessID,
                               dbtech_usertag_quotecount,avatarid,warnings,market_refund,ipoints,ponzi,infractions,hill_coins,
                               market_coupon,reputation,market_gifts,fortID,market_post_fontface,market_post_color,
                               market_threadcolor,daysprune,displaygroupid,pcmoderatedcount,autosubscribe))

# Remove more irrelevant or redundant variables
user <- subset(user, select=-c(usertitle,preposts,reputationlevelid,birthday_search,languageid,logintype,invites,caninvite,
                               hurt_heal,hh_time,tcg_free,partyid,pokeballs,poke_team,options,timezoneoffset))


# Confirm that no more variables with 0 variance
colvar0<-apply(user,2,function(x) var(x,na.rm=T)==0)
names(user)[colvar0]

# Used to find factor variables
out <- user %>% 
  lapply(table) %>% 
  lapply(as.data.frame) %>% 
  Map(cbind,var = names(user),.) %>% 
  rbind_all() %>% 
  group_by(var) %>% 
  mutate(pct = Freq / sum(Freq))

# Prepare factor variables
user$usergroupid[!user$usergroupid %in% c(2,15)] <- "Other"
user$usergroupid[user$usergroupid == 2] <- "Basic"
user$usergroupid[user$usergroupid == 15] <- "Full"

user$styleid[user$styleid != 0] <- 1

user$showvbcode[user$showvbcode != 1] <- 2
user$showvbcode <- user$showvbcode-1

user$showbirthday[user$showbirthday != 0] <- 1

user$customtitle[user$customtitle != 0] <- 1

user$pmpopup[user$pmpopup != 0] <- 1

user$maxposts[user$maxposts != -1] <- 1
user$maxposts[user$maxposts == -1] <- 0

user$startofweek[user$startofweek != -1] <- 1
user$startofweek[user$startofweek == -1] <- 0

user$referrerid[user$referrerid != 0] <- 1

# Change columns to factor
cols <- which(colnames(user) %in% c("usergroupid","styleid","showvbcode","showbirthday","customtitle","pmpopup","maxposts",
                                    "startofweek","referrerid","market_donate_history","market_steal_history"
                                    ,"market_gift_access","market_gambling"))
for(k in cols){
  user[,k] <- as.factor(user[,k])
}

variable.summary(user)


# Geolocate
user$ipaddress <- as.character(user$ipaddress)
missing <- user$userid[is.na(user$ipaddress)]
paste(missing,collapse=",")

ipfix <- read.csv("ip.csv", header=T, na.strings=c("","NA"))
for(id in ipfix$userid){
  user$ipaddress[user$userid == id] <- as.character(ipfix$ipaddress[ipfix$userid == id])
}
user$ipaddress


ips <- user$ipaddress
system.time(
  rgeolocate::maxmind(
    ips, "~/R/GeoLite2-City.mmdb", c("country_name")
  ) -> countries
)

user$country <- countries$country_name
user$country[is.na(user$country)] <- "Missing"

table(user$country,user$active)

# reduce the number of country categories
user$country[user$country %in% c("Belgium","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Ireland","Latvia",
                                 "Netherlands","Poland","Portugal","Romania","Slovenia","Spain","Sweden","United Kingdom")] <- "Europe"

user$country[!user$country %in% c("United States","Europe","Canada","Missing")] <- "Other"

user$country <- as.factor(user$country)
user <- within(user, country <- relevel(country, ref = "United States"))

GLM.Test <- glm(active ~ country, data=user, family=binomial(logit))
summary(GLM.Test)

user$country[user$country %in% c("Europe","Canada","Missing")] <- "United States"
user$country <- as.factor(as.character(user$country))
levels(user$country) <- c("Other","First World")
user <- within(user, country <- relevel(country, ref = "First World"))
GLM.Test2 <- glm(active ~ country, data=user, family=binomial(logit))
summary(GLM.Test2)
exp(cbind(coef(GLM.Test2), confint(GLM.Test2))) 

user$ipaddress <- NULL

# birthday year
hist(as.numeric(substr(as.character(user$birthday),7,10)))
user$birthday <- as.numeric(substr(as.character(user$birthday),7,10))
user$birthday[user$birthday < 1980] <- 0
user$birthday[user$birthday > 2005] <- 0
user$birthday[is.na(user$birthday)] <- 0
user$birthday <- floor(user$birthday/10) * 10
user$birthday <- as.factor(user$birthday)
table(user$birthday)

variable.summary(user)

saveRDS(user, file="C:/Users/jared_000/Desktop/SGH/2nd Semester/Big Data/Project/user_23_05.rds")

test <- user
test$username <- "User"
test$userid <- as.numeric(sample(1134))
test2 <- test[order(test$userid),]
rownames(test2) <- NULL
saveRDS(test2, file="C:/Users/jared_000/Desktop/SGH/2nd Semester/Big Data/Project/user_obfuscated.rds")