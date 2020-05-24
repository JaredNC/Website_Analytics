# I hate scientific notation
options(scipen=999)

# Libraries and why I use them
library(ggplot2) # Nice graphs
library(BCA) # use of variable.summary
library(MASS) # exponent function
library(reshape2) # reshape
library(tree) # Decision tree
library(rattle) # better decision tree
library(rpart) # graph rattle
library(randomForest) # good prediction tool
library('RcmdrPlugin.BCA') # helpful for neural network
library(nnet) # neural net
library(sqldf) # Use SQL queries
#library(dplyr) # useful transformations

# Begin by importing the .RDS file which I previously prepared.
setwd("C:/Users/jared_000/Desktop/SGH/2nd Semester/Big Data/Project")
user <- readRDS(file="user.rds")
user <- readRDS(file="user_23_05.rds")

variable.summary(user_old)
variable.summary(user)

# dbtech_usertag_tagcount + dbtech_usertag_mentions + dbtech_usertag_tags + dbtech_usertag_quotes + post_thanks_user_amount + 
# post_thanks_thanked_posts + market_purchases
boxplot(market_purchases ~ active, data=user)

# First Logistic Regression
GLM <- glm(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
             startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
             wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
             market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
             post_thanks_thanked_times + days + country, data=user, family=binomial(logit))

Anova(GLM)
summary(GLM)
exp(cbind(coef(GLM), confint(GLM))) 

# Second Logistic Regression
GLM.Step <- step(glm(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
             startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
             wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
             market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
             post_thanks_thanked_times + days + country, data=user, family=binomial(logit)),direction="both")

GLM.2 <- glm(active ~ usergroupid + showbirthday + maxposts + startofweek + pmtotal + friendcount + friendreqcount + wikiedits + 
               timespentonline + market_steal_history + market_gift_access + market_bank1 + post_thanks_thanked_times + days + 
               country, data=user, family=binomial(logit))
Anova(GLM.2)
summary(GLM.2)
exp(cbind(coef(GLM.2), confint(GLM.2))) 


#ANALYSIS
test_graph <- function(pred,valid) {
  obs <- length(pred)
  out <- list()
  for(i in 1:10){
    thresh <- i/10 - 0.05
    predvals <- as.numeric(pred>thresh)
    out[[i]] <- matrix(table(predvals,valid$active),2,2)
  }
  return(out)
}
melt_fun <- function(pred,valid) {
  melt_all <- list()
  melt <- melt(test_graph(pred,valid))
  melt$type <- ifelse(melt$Var1==1 & melt$Var2==1, "TN", 
                      ifelse(melt$Var1==2 & melt$Var2==2, "TP", 
                             ifelse(melt$Var1==1 & melt$Var2==2, "FN", 
                                    ifelse(melt$Var1==2 & melt$Var2==1, "FP", "NA"))))
  melt2 <- melt[melt$type %in% c("TP","TN"),]
  melt2$sum <- ave(melt2$value, melt2$L1, FUN=sum)
  melt2 <- unique(melt2[c("L1","sum")])
  melt_all[['melt']] <- melt
  melt_all[['melt2']] <- melt2
  melt_all[['obs']] <- length(pred)
  return(melt_all)
}



# Sample my data
set.seed(1)
rand <- sample(1:nrow(user), .7*nrow(user))
train <- user[rand,]
valid <- user[-rand,]

#Pred with GLM.2
GLM.2 <- glm(active ~ usergroupid + showbirthday + maxposts + startofweek + pmtotal + friendcount + friendreqcount + wikiedits + 
               timespentonline + market_steal_history + market_gift_access + market_bank1 + post_thanks_thanked_times + days + 
               country, data=train, family=binomial(logit))

pred <- predict(GLM.2,valid, type = "response")
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Logistic Regression')


#Pred with GLM
GLM <- glm(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
             startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
             wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
             market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
             post_thanks_thanked_times + days + country, data=train, family=binomial(logit))

pred <- predict(GLM,valid, type = "response")
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Logistic Regression')


#Pred with GLM.mixed
# prepare log variables
user$logposts <- with(user, log(posts+1))
user$logpmtotal <- with(user, log(pmtotal+1))
user$logprofilevisits <- with(user, log(profilevisits+1))
user$logucash <- with(user, log(abs(ucash)+1))
user$loggameroom_cash <- with(user, log(gameroom_cash+1))
user$logtimespentonline <- with(user, log(timespentonline+1))
user$logmarket_bank1 <- with(user, log(market_bank1+1))
user$logdays <- with(user, log(days+1))

# Sample my data
set.seed(123)
rand <- sample(1:nrow(user), .7*nrow(user))
train <- user[rand,]
valid <- user[-rand,]

GLM.mixed <- glm(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + logposts + pmpopup + birthday + maxposts + 
             startofweek + referrerid + logpmtotal + pmunread + logprofilevisits + friendcount + friendreqcount + vmunreadcount + 
             logucash + wikiedits + wikicreations + loggameroom_cash + logtimespentonline + market_donate_history + market_steal_history + 
             market_gift_access + market_gambling + logmarket_bank1 + dbtech_usertag_mentioncount + 
             post_thanks_thanked_times + logdays + country, data=train, family=binomial(logit))

pred <- predict(GLM.mixed,valid, type = "response")
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Logistic Regression')


#mixed with selection
GLM.mixed.step <- step(glm(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + logposts + pmpopup + birthday + maxposts + 
                   startofweek + referrerid + logpmtotal + pmunread + logprofilevisits + friendcount + friendreqcount + vmunreadcount + 
                   logucash + wikiedits + wikicreations + loggameroom_cash + logtimespentonline + market_donate_history + market_steal_history + 
                   market_gift_access + market_gambling + logmarket_bank1 + dbtech_usertag_mentioncount + 
                   post_thanks_thanked_times + logdays + country, data=train, family=binomial(logit)),direction="both")

GLM.mixed.2 <- glm(active ~ styleid + showbirthday + customtitle + logposts + pmpopup + maxposts + friendreqcount + loggameroom_cash + 
                     dbtech_usertag_mentioncount + post_thanks_thanked_times + logdays, data=train, family=binomial(logit))

pred <- predict(GLM.mixed.2,valid, type = "response")
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Logistic Regression')


#DECISION TREES
tree <- tree(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
             startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
             wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
             market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
             post_thanks_thanked_times + days + country + market_purchases, data=train)
plot(tree)
text(tree)

pred <- predict(tree,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Decision Tree')


RPART.1 <- rpart(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                   startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                   wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                   market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                   post_thanks_thanked_times + days + country, data=train, cp=0.00001)

fancyRpartPlot(RPART.1)
pred <- predict(RPART.1,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Decision Tree')


#Random Forest
mod <- randomForest(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                      startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                      wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                      market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                      post_thanks_thanked_times + days + country, data=train, ntrees=100)

pred <- predict(mod,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, NA) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Random Forest')

test <- seq(1,1000,by=50)
b <- c(rep(0,length(test)))
j <- 1
for(i in test) {
  forest <- randomForest(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                           startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                           wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                           market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                           post_thanks_thanked_times + days + country, data=train, ntree=i)
  a<-predict(forest, newdata=valid)
  a<-as.numeric(a>0.85)
  b[j] <- sum(abs(a-valid$active))
  j <- j+1
}
plot(test,b)
title('ntree vs #errors')


test <- rep(seq(1,10,by=1),20)
b <- c(rep(0,length(test)))
j <- 1
for(i in test) {
  forest <- randomForest(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                           startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                           wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                           market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                           post_thanks_thanked_times + days + country, data=train, ntree=100, nodesize=i)
  a<-predict(forest, newdata=valid)
  a<-as.numeric(a>0.85)
  b[j] <- sum(abs(a-valid$active))
  j <- j+1
}
plot(test,jitter(b,amount=0.1))
title('nodesize vs #errors')
table(b,test)


test <- seq(1,20,by=1)
b <- c(rep(0,length(test)))
j <- 1
for(i in test) {
  forest <- randomForest(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                           startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                           wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                           market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                           post_thanks_thanked_times + days + country, data=train, ntree=100, nodesize=5, corr.bias=TRUE)
  a<-predict(forest, newdata=valid)
  a<-as.numeric(a>0.85)
  b[j] <- sum(abs(a-valid$active))
  j <- j+1
}
plot(test,b)
title('#errors 20 cases with correlation bias')
table(b)

forest.final <- randomForest(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                 startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                 wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                 market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                 post_thanks_thanked_times + days + country, data=train, ntree=100, nodesize=5, corr.bias=FALSE)


#Neural Networks
NNET <- Nnet(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
               startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
               wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
               market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
               post_thanks_thanked_times + days + country, data=train, decay=0.10, size=4)

pred <- predict(NNET,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, 25) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Neural Network')


test <- seq(1,15,by=1)
b <- c(rep(0,length(test)))
j <- 1
for(i in test) {
  NNET <- Nnet(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                 startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                 wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                 market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                 post_thanks_thanked_times + days + country, data=train, decay=0.10, size=i)
  
  a<-predict(NNET, newdata=valid)
  a<-as.numeric(a>0.65)
  b[j] <- sum(abs(a-valid$active))
  j <- j+1
}
plot(test,b)
title('#neurons vs #errors')

ggplot()+
  geom_point(aes(x=test,y=b)) +
  scale_y_continuous(breaks = seq(1,30,by=1))
  
test <- rep(seq(4,8,by=1),20)
b <- c(rep(0,length(test)))
j <- 1
for(i in test) {
  NNET <- Nnet(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                 startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                 wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                 market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                 post_thanks_thanked_times + days + country, data=train, decay=0.10, size=i)
  
  a<-predict(NNET, newdata=valid)
  a<-as.numeric(a>0.65)
  b[j] <- sum(abs(a-valid$active))
  j <- j+1
}
plot(test,jitter(b,amount=0.1))
title('neurons vs #errors')
table(b,test)

NNET.6 <- Nnet(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
               startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
               wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
               market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
               post_thanks_thanked_times + days + country, data=train, decay=0.10, size=6)

pred <- predict(NNET.6,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, 25) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Neural Network')



#Lift charts
user$active <- as.factor(user$active)
train$active <- as.factor(train$active)
valid$active <- as.factor(valid$active)
lift.chart(c("GLM.mixed.2","tree","forest.final","NNET" ), train, "1", 0.07495591, 
           "cumulative", "Training Data")
#Error in lift.chart(c("GLM.mixed.2", "tree", "forest.final", "NNET"),  : 
# Models can only be estimated using glm, rpart, or nnet.

# training subset
# Validation subset
lift.chart(c("GLM.mixed.2","RPART.1","NNET" ), train, "1", 0.07495591, 
           "cumulative", "Training Data")

# Validation subset
lift.chart(c("GLM.mixed.2","RPART.1","NNET" ), valid, "1", 0.07495591, 
           "cumulative", "Validation Data")

# test bad models all data
GLM.test <- glm(active ~ country, data=train, family=binomial(logit))
GLM.test2 <- glm(active ~ country + posts + ucash, data=train, family=binomial(logit))
GLM.test3 <- glm(active ~ country + posts + ucash + referrerid + pmtotal, data=train, family=binomial(logit))
lift.chart(c("GLM.mixed.2","RPART.1","GLM.test","GLM.test2","GLM.test3" ), user, "1", 0.07495591, 
           "cumulative", "All Data")


#****************************************************
# TOP 10% CHURN RANKING
#****************************************************
# Setting ActiveDataSet variable. Its importand for functions
# rankScore() i rawProbScore()
ActiveDataSet('user')

# Adding new variable Score allow ranking creation. 
# Functions rankScore and rawProbScore are defined in package RcmdrPlugin.BCA
user$Score <- rankScore("NNET.6", user, "1")
user$Pr <- rawProbScore("NNET.6", user, "1")
user$Score <- rankScore("GLM.mixed.2", user, "1")
user$Pr <- rawProbScore("GLM.mixed.2", user, "1")
user$Score <- rankScore("RPART.1", user, "1")
user$Pr <- rawProbScore("RPART.1", user, "1")



# data sorting
user.sorted <- sqldf('SELECT userid, username, active, Score, Pr FROM user WHERE active like "1" ORDER BY "Pr" ASC')
head(user.sorted,20)


# What if I didn't include days?
NNET.6.alt <- Nnet(active ~  usergroupid + styleid + showvbcode + showbirthday + customtitle + posts + pmpopup + birthday + maxposts + 
                 startofweek + referrerid + pmtotal + pmunread + profilevisits + friendcount + friendreqcount + vmunreadcount + ucash + 
                 wikiedits + wikicreations + gameroom_cash + timespentonline + market_donate_history + market_steal_history + 
                 market_gift_access + market_gambling + market_bank1 + dbtech_usertag_mentioncount + 
                 post_thanks_thanked_times + country, data=train, decay=0.10, size=6)

GLM.mixed.2 <- glm(active ~ styleid + showbirthday + customtitle + logposts + pmpopup + maxposts + friendreqcount + loggameroom_cash + 
                     dbtech_usertag_mentioncount + post_thanks_thanked_times, data=train, family=binomial(logit))


pred <- predict(NNET.6.alt,valid)
melt <- melt_fun(pred,valid)
ggplot(melt[['melt']][melt[['melt']]$type %in% c("FP","FN"),], aes(x=(L1/10 -0.05), y=value, group=type, colour=type)) +
  geom_line() +
  ylim(-5, 25) +
  scale_x_continuous(breaks = seq(.05,.95,by=.1))+
  geom_text(data=melt[['melt2']],aes(x=(L1/10 -0.05),y=-3,label = paste0("Error",'\n',"= ", melt[['obs']]-sum)), inherit.aes = FALSE) + 
  labs(title = 'Error Graph', subtitle = 'Neural Network')

ActiveDataSet('user')

# Adding new variable Score allow ranking creation. 
# Functions rankScore and rawProbScore are defined in package RcmdrPlugin.BCA
user$Score <- rankScore("GLM.mixed.2", user, "1")
user$Pr <- rawProbScore("GLM.mixed.2", user, "1")

# data sorting
user.sorted <- sqldf('SELECT userid, username, active, Score, Pr FROM user WHERE active like "1" ORDER BY "Pr" ASC')
head(user.sorted,20)
