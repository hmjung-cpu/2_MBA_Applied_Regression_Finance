setwd("~/Documents/coding")
library(tidyverse)

wine <- read.csv("winequality.csv")
wine2=subset(wine, select = -c(citric.acid, chlorides))
null.2 <- lm(quality ~ 1, data=wine2)
full.2 <- lm(quality ~. + .^2, data=wine2)
n.2 <- nrow(wine2)
fwd.2 <- step(null.2, scope=formula(full.2), direction="forward", k=log(n))

fwd.22 <- lm(quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
     color + free.sulfur.dioxide + total.sulfur.dioxide + density + 
     pH + fixed.acidity + volatile.acidity:residual.sugar + alcohol:free.sulfur.dioxide + 
     free.sulfur.dioxide:total.sulfur.dioxide + color:free.sulfur.dioxide + 
     sulphates:total.sulfur.dioxide + sulphates:free.sulfur.dioxide + 
     volatile.acidity:color + alcohol:density + alcohol:volatile.acidity + 
     residual.sugar:free.sulfur.dioxide + color:density + residual.sugar:total.sulfur.dioxide + 
     color:pH + sulphates:pH + residual.sugar:pH + pH:fixed.acidity, data=wine2)
summary(fwd.22)

fwd.22.minus2 <- lm(quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
                      color + free.sulfur.dioxide + total.sulfur.dioxide + 
                      pH + fixed.acidity + alcohol:free.sulfur.dioxide + 
                      free.sulfur.dioxide:total.sulfur.dioxide + color:free.sulfur.dioxide + 
                      sulphates:total.sulfur.dioxide + sulphates:free.sulfur.dioxide + 
                      volatile.acidity:color + alcohol:density + alcohol:volatile.acidity + 
                      residual.sugar:free.sulfur.dioxide + color:density + residual.sugar:total.sulfur.dioxide + 
                      color:pH + sulphates:pH + residual.sugar:pH + pH:fixed.acidity, data=wine2)
summary(fwd.22.minus2)



#2-data exploration
reg.wine <- lm(quality ~ . , data=wine)
summary(reg.wine)
reg.wine2 <- lm(quality ~ . -citric.acid -chlorides , data=wine)
summary(reg.wine2)

null <- lm(quality ~ 1, data=wine)
full <- lm(quality ~. + .^2, data=wine)
n <- nrow(wine)
fwd <- step(null, scope=formula(full), direction="forward", k=log(n))

reg.wine3 <- lm(quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
                  color + free.sulfur.dioxide + total.sulfur.dioxide + chlorides + 
                  density + volatile.acidity*residual.sugar + alcohol*free.sulfur.dioxide + 
                  free.sulfur.dioxide*total.sulfur.dioxide + color*free.sulfur.dioxide + 
                  sulphates*total.sulfur.dioxide + sulphates*free.sulfur.dioxide + 
                  alcohol*chlorides + volatile.acidity*color + alcohol*volatile.acidity + 
                  sulphates*chlorides + alcohol*density + residual.sugar*free.sulfur.dioxide, data=wine)
summary(reg.wine3)

#full regression
reg.wine <- lm(quality ~ . , data=wine)
par(mfrow=c(1,3))
plot(reg.wine$fitted.values, reg.wine$residuals)
hist(rstudent(reg.wine))
qqnorm(rstudent(reg.wine),col=4)
abline(a=0,b=1)
#removed non influential full 
reg.wine2 <- lm(quality ~ . -citric.acid -chlorides , data=wine)
par(mfrow=c(1,3))
plot(reg.wine2$fitted.values, reg.wine2$residuals)
hist(rstudent(reg.wine2))
qqnorm(rstudent(reg.wine2),col=4)
abline(a=0,b=1)
#stepwise 
reg.wine3 <- lm(quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
                  color + free.sulfur.dioxide + total.sulfur.dioxide + chlorides + 
                  density + volatile.acidity*residual.sugar + alcohol*free.sulfur.dioxide + 
                  free.sulfur.dioxide*total.sulfur.dioxide + color*free.sulfur.dioxide + 
                  sulphates*total.sulfur.dioxide + sulphates*free.sulfur.dioxide + 
                  alcohol*chlorides + volatile.acidity*color + alcohol*volatile.acidity + 
                  sulphates*chlorides + alcohol*density + residual.sugar*free.sulfur.dioxide, data=wine)
par(mfrow=c(1,3))
plot(reg.wine3$fitted.values, reg.wine3$residuals)
hist(rstudent(reg.wine3))
qqnorm(rstudent(reg.wine3),col=4)
abline(a=0,b=1)

#removed non influential stepwise
reg.wine4 <- lm(quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
                  color + free.sulfur.dioxide + total.sulfur.dioxide + chlorides + 
                  density + volatile.acidity*residual.sugar + alcohol*free.sulfur.dioxide + 
                  free.sulfur.dioxide*total.sulfur.dioxide + color*free.sulfur.dioxide + 
                  sulphates*total.sulfur.dioxide + sulphates*free.sulfur.dioxide + 
                  alcohol*chlorides + volatile.acidity*color + alcohol*volatile.acidity + 
                  sulphates*chlorides + alcohol*density + residual.sugar*free.sulfur.dioxide
                -volatile.acidity*residual.sugar , data=wine)
par(mfrow=c(1,3))
plot(reg.wine4$fitted.values, reg.wine4$residuals)
hist(rstudent(reg.wine4))
qqnorm(rstudent(reg.wine4),col=4)
abline(a=0,b=1)

boxplot(wine$quality ~ wine$color=="white")
install.packages("corrplot")
install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)
M <-cor(quality)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- wine[, c(2,3,4,5,6,7,8,9,10,11,12,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
boxplot(wine$quality ~ wine$residual.sugar)

#skewness
plot(reg.wine$fitted.values, reg.wine$residuals)
hist(rstudent(reg.wine))
qqnorm(rstudent(reg.wine),col=4)
abline(a=0,b=1)

plot(wine$fixed.acidity,wine$quality) #1
plot(wine$volatile.acidity,wine$quality) #2
plot(wine$citric.acid,wine$quality) #3
plot(wine$residual.sugar,wine$quality) #######4
plot(wine$chlorides,wine$quality) ############5
plot(wine$free.sulfur.dioxide,wine$quality) #######6
plot(wine$total.sulfur.dioxide,wine$quality) #7
plot(wine$density,wine$quality)###########8
plot(wine$pH,wine$quality) #9
plot(wine$sulphates,wine$quality) #10
plot(wine$alcohol,wine$quality) #11
plot(wine$color,wine$quality) #12

par(mfrow=c(1,1))
qqnorm(rstudent(reg.wine1),col=4)
abline(a=0,b=1)

par(mfrow=c(3,4))
reg1 <- lm(wine$quality~wine$fixed.acidity) ##############
hist(rstudent(reg1))
plot(reg1$fitted.values,reg1$residuals)
reg4 <- lm(wine$quality~wine$residual.sugar)
hist(rstudent(reg4))
plot(reg4$fitted.values,reg4$residuals) ####
reg5 <- lm(wine$quality~wine$chlorides) ##############
hist(rstudent(reg5))
plot(reg5$fitted.values,reg5$residuals) ##
reg6 <- lm(wine$quality~wine$free.sulfur.dioxide)
hist(rstudent(reg6))
plot(reg6$fitted.values,reg6$residuals) ####
reg7 <- lm(wine$quality~wine$total.sulfur.dioxide)
hist(rstudent(reg7))
plot(reg7$fitted.values,reg7$residuals) #
reg8 <- lm(wine$quality~wine$density)
hist(rstudent(reg8))
plot(reg8$fitted.values,reg8$residuals) ####
