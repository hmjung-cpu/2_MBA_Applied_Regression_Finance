rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)

okun7 <- read.csv("okunIV.csv")
summary(okun7)
nOkun <- nrow(okun7)

okun1 <- okun7[-1,]
okun1$GDP <- okun7$GDP[2:nOkun]
okun1$UNEMPLOYMENT <- (okun7$UNEMPLOYMENT[2:nOkun]-okun7$UNEMPLOYMENT[1:(nOkun-1)])

plot(okun1$UNEMPLOYMENT, okun1$GDP,
     xlab="% Change Unemployment",
     ylab="% Change GDP",
     main="% Change GDP vs. % Change. Unemployment", pch=19)

okunreg <- lm(UNEMPLOYMENT~GDP, data=okun1)
summary(okunreg)

relevance.reg1 <- lm(GDP~PRIVATE_CONSUMP, data=okun1)
relevance.reg2 <- lm(GDP~PRIVATE_FIXED_INV, data=okun1)
relevance.reg3 <- lm(GDP~PUBLIC_CONSUMP, data=okun1)
summary(relevance.reg1)
summary(relevance.reg2)
summary(relevance.reg3)

#TSLS
IV1 <- lm(okun1$UNEMPLOYMENT ~ relevance.reg1$fitted.values)
IV2 <- lm(okun1$UNEMPLOYMENT ~ relevance.reg2$fitted.values)
IV3 <- lm(okun1$UNEMPLOYMENT ~ relevance.reg3$fitted.values)
summary(IV1)
summary(IV2)
summary(IV3)

