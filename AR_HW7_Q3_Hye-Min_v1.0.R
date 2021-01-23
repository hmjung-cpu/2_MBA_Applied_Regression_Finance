rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)
bike <- read.csv("bikeSharing.csv")

bike2 <- subset(bike, bike$humidity!=0)
bike2<-bike2 %>%
  mutate(string=substr(bike2$datetime,12,13)) 

bike2<-bike2 %>%
  mutate(dummy0=ifelse(bike2$string=="00",1,0),
         dummy1=ifelse(bike2$string=="01",1,0),
         dummy2=ifelse(bike2$string=="02",1,0),
         dummy3=ifelse(bike2$string=="03",1,0),
         dummy4=ifelse(bike2$string=="04",1,0),
         dummy5=ifelse(bike2$string=="05",1,0),
         dummy6=ifelse(bike2$string=="06",1,0),
         dummy7=ifelse(bike2$string=="07",1,0),
         dummy8=ifelse(bike2$string=="08",1,0),
         dummy9=ifelse(bike2$string=="09",1,0),
         dummy10=ifelse(bike2$string=="10",1,0),
         dummy11=ifelse(bike2$string=="11",1,0),
         dummy12=ifelse(bike2$string=="12",1,0),
         dummy13=ifelse(bike2$string=="13",1,0),
         dummy14=ifelse(bike2$string=="14",1,0),
         dummy15=ifelse(bike2$string=="15",1,0),
         dummy16=ifelse(bike2$string=="16",1,0),
         dummy17=ifelse(bike2$string=="17",1,0),
         dummy18=ifelse(bike2$string=="18",1,0),
         dummy19=ifelse(bike2$string=="19",1,0),
         dummy20=ifelse(bike2$string=="20",1,0),
         dummy21=ifelse(bike2$string=="21",1,0),
         dummy22=ifelse(bike2$string=="22",1,0),
         dummy23=ifelse(bike2$string=="23",1,0))
bike2$string = as.numeric(bike2$string)

bike2 <- bike2 %>%
  mutate(commuting1=ifelse(bike2$string>=6&string<=9,1,0)) %>%
  mutate(day=ifelse(bike2$string>=10&string<=16,1,0)) %>%
  mutate(commuting2=ifelse(bike2$string>=17&string<=19,1,0)) %>%
  mutate(evening=ifelse(bike2$string>=20&string<=23,1,0)) %>%
  mutate(night=ifelse(bike2$string>=0&string<=5,1,0))

names(bike2)
#subset
XY <- bike2[,]
index <- sample(1:nrow(bike2),7242)
bike2 <- XY[index,]
validation <- XY[-index,]

names(bike_24)
bike_24 <- bike2[,-c(1,38:42)]

nrow(bike2)*2/3 #7242
#(iv) 24hours 

bike_24_casual <- bike_24[,-c(10,11,12)]
names(bike_24_casual)

n_bike<-nrow(bike_24_casual)
null_bike_24_train <-lm(formula=casual~1, data = bike_24_casual)
full_bike_24_train <-lm(formula =casual ~.+.^2, data = bike_24_casual)
fwd_bike_24_train <-step(null_bike_24_train, scope=formula(full_bike_24_train), direction="forward",k=log(n_bike))



bike4 <- bike2[,-c(1,13:37)]


nrow(bike2)*2/3 #7242
# using subset function 
XY <- bike2[,]
index <- sample(1:nrow(bike2),7242)
training <- XY[index,]
validation <- XY[-index,]

summary(lm(casual ~ ., data=training))
summary(lm(casual ~ .+.^2, data=training))
summary(lm(registered ~ ., data=training))
summary(lm(registered ~ .+.^2, data=training))
summary(lm(count ~ ., data=training))
summary(lm(count~ .+.^2, data=training))

names(bike5)
#bike5 <- bike2[,-c(1,38:42)]
XY5 <- bike5[,]
index <- sample(1:nrow(bike2),7242)
training <- XY[index,]
validation <- XY[-index,]
par(mfrow=c(1,1))
plot(bike2$string,bike2$count)

names(training)
#training <- training[,-c(1,13:37)]

n_bike4<-nrow(training)
null_bike4_train <-lm(formula=casual~1, data = training)
full_bike4_train <-lm(formula =casual ~.+.^2, data = training)
fwd_bike4_train <-step(null_bike4_train, scope=formula(full_bike4_train), direction="forward",k=log(n_bike4))

n_bike<-nrow(bike2)
null_bike_casual<-lm(formula=casual~1,data = bike_casual)
full_bike_casual<-lm(formula =casual~.+.^2,data = bike_casual)
fwd_bike_casual<-step(null_bike_casual,scope=formula(full_bike_casual),direction="forward",k=log(n_bike))

null_bike_register<-lm(formula=registered~1,data = bike_registered)
full_bike_register<-lm(formula=registered~.+.,data = bike_registered)
fwd_bike_regist<-step(null_bike_register,scope=formula(full_bike_register),direction="forward",k=log(n_bike))





null_bike_register<-lm(formula=registered~1,data = bike_registered)
full_bike_register<-lm(formula=registered~.,data = bike_registered)
fwd_bike_regist<-step(null_bike_register,scope=formula(full_bike_register),direction="forward",k=log(n_bike))




#(iv)
bike_casual1 <- bike2[,-c(1,11:13)]
bike_casual1 <- bike_casual1[,-c(34:38)]
names(bike_casual1)
view(bike_casual1)

########
count(bike, windspeed==0)

bike3 <- bike2[c(1:10864),-c(10,12,13)]

bike_casual <- bike2[,-c(1,11:13)]
bike_registered <- bike2[,-c(1,10,12,13)]

bike_casual <- bike_casual %>%
  mutate(commuting1=ifelse(dummy6==1,1,0))


#most important hours of the day for rental demand
summary(lm(casual ~ ., data=bike_casual))
summary(lm(registered ~ ., data=bike_registered))
summary(lm(casual ~ .+.^2, data=bike_casual))
summary(lm(registered ~ .+.^2, data=bike_registered))

#(iii)
nrow(bike2)*2/3 #7242

n_bike<-nrow(bike2)
null_bike_casual<-lm(formula=casual~1, data = bike_casual)
full_bike_casual<-lm(formula =casual~., data = bike_casual)
fwd_bike_casual<-step(null_bike_casual, scope=formula(full_bike_casual), direction="forward",k=log(n_bike))

null_bike_register<-lm(formula=registered~1,data = bike_registered)
full_bike_register<-lm(formula=registered~.,data = bike_registered)
fwd_bike_regist<-step(null_bike_register,scope=formula(full_bike_register),direction="forward",k=log(n_bike))

#  mutate(commute1=ifelse(time>=06 & time <=09, 1, 0)) %>%
#  mutate(day=ifelse(time>=10 & time <= 16, 1, 0)) %>%
#  mutate(commute2=ifelse(time>=17 & time<=20, 1, 0)) %>%
#  mutate(evening=ifelse(time>=20 & time<=23, 1, 0)) %>%
#  mutate(night=ifelse(time<=05, 1, 0))
 


plot(bike$datetime, bike$count)

#commuting 6~9 (3)
#day 10~16 (6)
#commuting 17~20 (3)
#evening 21~23 (2)
#night 24~5 (5)


par(mfrow=c(2,3))
plot(bike$season, bike$count, pch=20)
plot(bike$weather, bike$count, pch=20)
plot(bike$temp, bike$count, pch=20)
plot(bike$atemp, bike$count, pch=20)
plot(bike$humidity, bike$count, pch=20)
plot(bike$windspeed, bike$count, pch=20)


nrow(bike)*2/3 #7257

# using subset function 
XY <- bike[,]
index <- sample(1:nrow(bike),7257)
training <- XY[index,]
validation <- XY[-index,]

n <- nrow(bike)
null <- lm(count ~ 1, data=bike)
full <- lm(count ~ ., data=bike)
fwd <- step(null, scope=formula(full), direction="forward", k=log(n))

keep = substr(x, first, last)

# dummy for hour
install.packages("lubridate")
library(lubridate)

plot(bike$datetime,bike$count)




reg <- lm(count ~ weather + temp + atemp + humidity + windspeed, data=bike)

