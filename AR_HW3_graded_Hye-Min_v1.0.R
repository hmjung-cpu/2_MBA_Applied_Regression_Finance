rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)

1.690067-2*0.136379;1.690067+2*0.136379
abs(1.690067-1.52)/0.136379 

SSE <- 0.03734*101 
R <- 0.6033
SST=3.77134/0.3967
SST
SSR <- 9.506781*0.6033
SSR
(0.0374)^2*101
3.77134*(101/102)
1.6900067*(0.136379/0.003680)
0.03/52

#SST
0.1412748/(1-0.6033) 
#SSR
0.356125 *0.6033

0.356125/102
sqrt(0.6033)

0.03/52+1.6900067*(2.7-0.03/52)
#1.2
data1 <- read.csv("okun.csv")
#compute the annual changes in the unemployment rate ∆ut = ut − ut−1.
delta_unemp <- as.data.frame(diff(data1$UNEMPLOYMENT))


#Present a visual summary of the data (both unemployment and GDP). 
plot(data2$diff.data1.UNEMPLOYMENT,data2$diff.data1.GDP, main = "Okun's Law",
     xlab = "annual changes in the unemployment rate", ylab = "changes in GDP growth rates", pch = 20)
#Describe any outliers that you might observe in the data.

model <- lm(data1$diff.data-1.GDP~data2$diff.data1.UNEMPLOYMENT)   #b0=-0.01838, b1=-0.81343  
summary(model)
names(model)
hist(model$residuals, main = "residuals from model fit", xlab = "residuals", ylab = "frequency")

