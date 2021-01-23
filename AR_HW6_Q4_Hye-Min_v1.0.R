rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
burrito <- read.csv("burrito.csv")
#(i)  First, start with the null model and use the forward stepwise selection procedure 
#in the step function in R to find the best model. Use the BIC to select the model. 
#Describe the model you fit and assess its quality. What are the key predictors of overall score? 
reg.bur <- lm(overall ~ .,data=burrito)
summary(reg.bur)
#Meat, Fillings, Meat.filling, Synergy, Wrap, Guac

null <- lm(overall ~ 1, data=burrito)
full <- lm(overall ~. + .^2, data=burrito)
n <- nrow(burrito)
fwd <- step(null, scope=formula(full), direction="forward", k=log(n))

#(ii)
null <- lm(overall ~ 1, data=burrito)
full2 <- lm(overall ~. , data=burrito)
n <- nrow(burrito)
fwd <- step(null, scope=formula(full2), direction="backward", k=log(n))

#(iii)
claire<-read.csv("clairemont.csv")
view(claire)
n2<-nrow(claire)
null3<-lm(formula=overall~1,data = claire)
full3<-lm(formula = overall~.,data = claire)
step(null3,scope=formula(full3),direction="forward",k=log(n2))
