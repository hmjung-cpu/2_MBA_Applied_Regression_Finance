setwd("~/Documents/coding")
library(tidyverse)
##data cleaning
amazon <- read.csv("amazon.csv")
amazon$region<-factor(amazon$region, labels=c("northeast", "midwest", "south", "west"))
amazon$oldest <-factor(amazon$oldest, labels=c("18-20", "21-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"))
amazon$income <-factor(amazon$income, labels=c("15k-", "15-25k", "25-35k", "35-50k", "50-75k", "75-100k", "100k+"))
amazon$race <-factor(amazon$race, labels=c("white", "black", "asian", "other/mixed"))

##identifying outliers
summary(amazon)
summary(amazon.reg1 <- lm(bought ~ ., data=amazon))
summary(amazon.reg1 <- lm(log(bought) ~ ., data=amazon))
par(mfrow=c(1,3))
plot(amazon.reg1$fitted.values, amazon.reg1$residuals)
hist(rstudent(amazon.reg1),col=4)
qqnorm(rstudent(amazon.reg1),col=4)
abline(a=0,b=1)

##deleting outliers
amazon <- amazon %>%
  filter(bought>0.50) 

##normality assumption diagnostic
summary(amazon.reg2 <- lm(log(bought) ~ ., data=amazon))
par(mfrow=c(1,3))
plot(amazon.reg2$fitted.values, amazon.reg2$residuals)
hist(rstudent(amazon.reg2),col=4)
qqnorm(rstudent(amazon.reg2),col=4)
abline(a=0,b=1)

## define the scope from small model to big
n<-nrow(amazon)
null <- lm(log(bought) ~ 1, data=amazon)
full <- lm(log(bought) ~ . + .^2, data=amazon)
## build a regression model with the BIC
fwdBIC <- step(null, scope=formula(full), direction="forward", k=log(n))

summary(lm(log(bought) ~ connection + region, data=amazon))
