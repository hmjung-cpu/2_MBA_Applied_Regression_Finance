rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)

#1-i
beef <- read.csv("beef.csv")
reg.beef <- lm(YES ~ SIZE + VAL, data=beef)
plot(reg.beef$fitted.values, reg.beef$residuals)
hist(rstudent(reg.beef))
qqnorm(rstudent(reg.beef),col=4)
abline(a=0,b=1)

#1-ii
reg.beef2 <- lm(YES ~ SIZE + log(VAL), data=beef)

#1-iii
reg.beef3 <- lm(SIZE ~ log(VAL), data=beef)
summary(reg.beef3)
plot(beef$SIZE, log(beef$VAL))


#3-i
nutrition <- read.csv("nutrition.csv")
reg.nutrition <- lm(woh ~ age, data=nutrition)
summary(reg.nutrition)
anova(reg.nutrition)
par(mfrow=c(1,3))
plot(reg.nutrition$fitted.values, reg.nutrition$residuals)
qqnorm(rstudent(reg.nutrition),col=4)
abline(a=0,b=1)
hist(rstudent(reg.nutrition))

reg.nutrition2 <- lm(woh ~ age + I(age^2) + I(age^3) + I(age^4), data=nutrition)
par(mfrow=c(1,3))
plot(reg.nutrition2$fitted.values, reg.nutrition2$residuals)
qqnorm(rstudent(reg.nutrition2),col=4)
abline(a=0,b=1)
hist(rstudent(reg.nutrition2))
summary(reg.nutrition2)
anova(reg.nutrition2)
anova(reg.nutrition, reg.nutrition2)

#3-ii
summary(reg.nutrition)
summary(reg.nutrition2)

#3-iii E[woh|age] = β0 + β1age + β21{age > 7} + β3age × 1{age > 7}
nutrition <- nutrition %>%
  mutate(age7 = ifelse(age>=7,1,0))

reg.nutrition3 <- lm(woh ~ age + I(age7) + I(age*age7), data=nutrition)
par(mfrow=c(1,3))
plot(reg.nutrition3$fitted.values, reg.nutrition3$residuals)
qqnorm(rstudent(reg.nutrition3),col=4)
abline(a=0,b=1)
hist(rstudent(reg.nutrition3))

par(mfrow=c(3,3))
plot(reg.nutrition$fitted.values, reg.nutrition$residuals, col="black", main="just age model")
plot(reg.nutrition2$fitted.values, reg.nutrition2$residuals, col="red", main="polynomial model")
plot(reg.nutrition3$fitted.values, reg.nutrition3$residuals, col="blue", main="age>7 model")

qqnorm(rstudent(reg.nutrition),col="black", main="just age model")
abline(a=0,b=1)
qqnorm(rstudent(reg.nutrition2), col="red", main="polynomial model")
abline(a=0,b=1)
qqnorm(rstudent(reg.nutrition3), col="blue", main="age>7 model")
abline(a=0,b=1)

#par(mfrow=c(1,3))
hist(rstudent(reg.nutrition), col="black", main="just age model")
hist(rstudent(reg.nutrition2), col="red", main="polynomial model")
hist(rstudent(reg.nutrition3), col="blue", main="age>7 model")

#3-iv
par(mfrow=c(1,1))
plot(nutrition$age, nutrition$woh)

lines(nutrition$age,reg.nutrition$fitted.values,col="Black")
lines(nutrition$age,reg.nutrition2$fitted.values,col="Red")
lines(nutrition$age,reg.nutrition3$fitted.values,col="Blue")

range(nutrition$age)
newx <- seq(0.5, 71.5, by=0.05)
pred_interval <- predict(reg.nutrition, newdata=data.frame(age=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="grey", lty=2)
lines(newx, pred_interval[,3], col="grey", lty=2)

# iv.
par(mfrow=c(1,1))
plot(nutrition$age, nutrition$woh)

nutripred1 <- predict(reg.nutrition, interval="prediction", level=0.95)

lines(nutrition$age, nutripred1[,"fit"], col="black", lty=2)
lines(nutrition$age, nutripred1[,"lwr"], col="black", lwd=2)
lines(nutrition$age, nutripred1[,"upr"], col="black", lwd=2)

nutripred2 <- predict(reg.nutrition2, interval="prediction", level=0.95)

lines(nutrition$age, nutripred2[,"fit"], col="red", lty=2)
lines(nutrition$age, nutripred2[,"lwr"], col="red", lwd=2)
lines(nutrition$age, nutripred2[,"upr"], col="red", lwd=2)

nutripred3 <- predict(reg.nutrition3, interval="prediction", level=0.95)

lines(nutrition$age, nutripred3[,"fit"], col="blue", lty=2)
lines(nutrition$age, nutripred3[,"lwr"], col="blue", lwd=2)
lines(nutrition$age, nutripred3[,"upr"], col="blue", lwd=2)




#lm2.noint <- lm(dist ~ -1 + speed + I(speed^2))
#coef(lm2.noint)
coef(reg.nutrition2)
#X <- model.matrix(lm2.noint)
X <- model.matrix(reg.nutrition2)
head(X)
#head(X)
df.new <- data.frame(age=(0.5:71.5))
#df.new <- data.frame(speed=(0:27))
conf.dist <- predict(reg.nutrition2, newdata = df.new, interval="confidence", level=1-alpha) 
pred.dist <- predict(reg.nutrition2, newdata = df.new, interval="prediction", level=1-alpha) 
#conf.dist <- predict(lm2.noint, newdata = df.new, interval="confidence", level=1-alpha) 
#pred.dist <- predict(lm2.noint, newdata = df.new, interval="prediction", level=1-alpha) 
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
pl <- ggplot(nutrition) + geom_point(aes(x=age, y=woh), size=2, colour="#993399") + 
  xlab("age") + ylab("woh")  
print(pl)
pl +   
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1)

#pred_interval2 <- predict(nutrition$age + I(nutrition$age^2) + I(nutrition$age^3) + I(nutrition$age^4), newdata=data.frame(age=newx), interval="prediction",
#                         level = 0.95)
#lines(newx, pred_interval[,2], col="blue", lty=2)
#lines(newx, pred_interval[,3], col="blue", lty=2)

#pred_interval3 <- predict(reg.nutrition3, newdata=data.frame(age=newx), interval="prediction",
#                          level = 0.95)
#lines(newx, pred_interval[,2], col="red", lty=2)
#lines(newx, pred_interval[,3], col="red", lty=2)

#library(ggplot2)
#ggplot(nutrition, aes(x=age, y=woh))+
#  geom_point()+
#  geom_smooth(method=lm, se=TRUE)
#temp_var <- predict(reg.nutrition, interval="prediction")
#new_df <- cbind(nutrition, temp_var)
#ggplot(new_df, aes(age, woh))+
#  geom_point() +
#  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
#  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
#  geom_smooth(method=lm, se=TRUE)


