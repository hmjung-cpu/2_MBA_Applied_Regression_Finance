setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

### Telemarketing example: "calls per day" vs "length of employement".
## Fit a model with Y=calls vs. X=months
## from slide 39
attach(telemkt <- read.csv("telemarketing.csv") )
tele1 <- lm(calls~months) 
xgrid <- data.frame( months = 10:30 )
par(mfrow=c(1,2)); plot(months, calls, pch=20, col=4)
lines(xgrid$months, predict(tele1, newdata=xgrid) )
r <- rstudent(tele1)
plot(months, r, pch=20, col=4); abline(h=0, lty=2)

## from slide 40
months2 <- months^2
summary(tele2 <- lm(calls~ months + months2) )

## from slide 41
par(mfrow=c(1,2)); plot(months, calls, pch=20, col=4)
lines(months,fitted(tele2))

# or as before, use 
# xgrid <- data.frame( months = 10:30, months2 = (10:30)^2 )
# lines(xgrid$months, predict(tele2, newdata=xgrid) )
plot(months, rstudent(tele2), pch=20, col=4); abline(h=0, lty=2)


## from slide 47
summary(tele3 <- nls(calls~ beta0+beta1*months + beta2*months*months, start=list(beta0=1,beta1=1,beta2=1)))
par(mfrow=c(1,2)); plot(months, calls, pch=20, col=4)
lines(months,fitted(tele3))
summary(tele4 <- nls(calls~ beta0+(beta1-beta0)*exp(-exp(beta2)*months),start=list(beta0=3,beta1=-6,beta2=-2)))
plot(months, calls, pch=20, col=4)
lines(months,fitted(tele4))

summary(tele5 <- nls(calls~ beta0+beta1*months^(1/2)+beta2*months, start=list(beta0=1,beta1=1,beta2=1)))
# or equivalently…
summary(tele6 <- nls(calls~ SSasymp(months,beta0,beta1,beta2)))


##nonparametric regression
# slides 51
attach(telemkt <- read.csv("telemarketing.csv") )
par(mfrow=c(1,2));
loessreg<-loess(calls ~ months,data=telemkt)
plot(months,calls)
lines(months,fitted(loessreg),col=4)
plot(months, calls-fitted(loessreg), pch=20, col=4,ylab='calls'); abline(h=0, lty=2)

##nonparametric regression (span)
par(mfrow=c(1,2));
loessreg<-loess(calls ~ months,data=telemkt,span=0.7)
plot(months,calls)
lines(months,fitted(loessreg),col=4)
plot(months, calls-fitted(loessreg), pch=20, col=4,ylab='calls'); abline(h=0, lty=2)

## Nonparametric Kernel Smoother
par(mfrow=c(1,3));
plot(months,calls,main="Bandwidth = 2")
lines(ksmooth(months,calls,"normal",bandwidth=2),col=4)
plot(months,calls,main="Bandwidth = 4")
lines(ksmooth(months,calls,"normal",bandwidth=4),col=4)
plot(months,calls,main="Bandwidth = 6")
lines(ksmooth(months,calls,"normal",bandwidth=6),col=4)
