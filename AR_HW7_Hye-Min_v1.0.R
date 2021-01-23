rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

#weekly orange juice sales at various locations
OJ <- read.csv("OJ.csv")
summary(OJ)

par(mfrow=c(2,4))
plot(OJ$minutead, OJ$minutevol)
plot(OJ$minuteprice, OJ$minutevol)
plot(OJ$tropicprice, OJ$minutevol)
plot(OJ$dmnckprice, OJ$minutevol)

plot(OJ$tropicprice, OJ$minuteprice)
plot(OJ$tropicprice, OJ$dmnckprice)
plot(OJ$minuteprice, OJ$dmnckprice)

# 'pairs' scatterplot matrix
par(mfrow=c(1,1))
pairs(OJ[,1:7], pch = 19)

install.packages("psych")
library(psych)
pairs.panels(OJ[,1:7], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

reg.1 <- lm(log(minutevol) ~ log(minuteprice), data=OJ)
summary(reg.1)
reg.2 <- lm(log(minutevol) ~ log(minuteprice) + minutead, data=OJ)
summary(reg.2)
reg.3 <- lm(log(minutevol) ~ log(minuteprice) + log(tropicprice) + log(dmnckprice) + minutead, data=OJ)
summary(reg.3)
reg.4 <- lm(log(minutevol) ~ log(minuteprice) + log(tropicprice) + log(dmnckprice) + minutead + tropicad, data=OJ)
summary(reg.4)
reg.5 <- lm(log(minutevol) ~ log(minuteprice) + log(tropicprice) + log(dmnckprice) + minutead + tropicad + dmnckad, data=OJ)
summary(reg.5)

## define the scope from small model to big
null <- lm(log(minutevol) ~ 1, data=OJ)
full <- lm(log(minutevol) ~ ., data=OJ)
## build a regression model with the BIC
fwdBIC <- step(null, scope=formula(full), direction="forward", k=log(1000))
