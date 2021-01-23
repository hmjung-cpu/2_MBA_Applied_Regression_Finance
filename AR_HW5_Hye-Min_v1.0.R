rm(list=ls())
setwd("~/Documents/coding")
library(tidyverse)
library(dplyr)
library(tidytext)
trump <- read.csv("trump.csv")

#i
reg.fav <- lm(favoriteCount ~ source, data=trump)
reg.ret <- lm(retweetCount ~ source, data=trump)
summary(reg.fav)
summary(reg.ret)

#ii??????????????????
pp312den_q2 <- read_csv("pp312den.csv") %>%
  filter(age>=25, age<=55, (empstat=="Employed"|empstat=="Unemployed")) %>%
  mutate(hispanic = ifelse(hispan!="Not Hispanic",1,ifelse(race=="White" & hispan=="Not Hispanic",2,NA))) %>%
  mutate(employed = ifelse(empstat=="Employed",1,0))

trump.mutate <- trump %>%
  mutate(exclamation_2 = ifelse(exclamation=="Yes",1,0)) %>%
  mutate(source_2 = ifelse(source=="Android",1,0)) %>%
  mutate(hashtag_2 = ifelse(hashtag=="Yes",1,0))

reg.fav.ii <- lm(exclamation_2 ~ source_2, data=trump.mutate)
reg.ret.ii <- lm(hashtag_2 ~ source_2, data=trump.mutate)
summary(reg.fav.ii)
summary(reg.ret.ii)

#iii
reg.fav2 <- lm(favoriteCount ~ exclamation + hashtag, data=trump)
reg.ret2 <- lm(retweetCount ~ exclamation + hashtag, data=trump)
summary(reg.fav2)
summary(reg.ret2)

reg.fav2a <- lm(favoriteCount ~ exclamation + hashtag + hour, data=trump)
reg.ret2a <- lm(retweetCount ~ exclamation + hashtag + hour, data=trump)
summary(reg.fav2a)
summary(reg.ret2a)

reg.fav2a <- lm(favoriteCount ~ exclamation + hashtag + hour, data=trump)
reg.ret2a <- lm(retweetCount ~ exclamation + hashtag + hour, data=trump)
summary(reg.fav2a)
summary(reg.ret2a)

reg.fav3 <- lm(favoriteCount ~ exclamation + hashtag + picture + hour, data=trump)
reg.ret3 <- lm(retweetCount ~ exclamation + hashtag + picture + hour, data=trump)
summary(reg.fav3)
summary(reg.ret3)

#iv
reg.fav4 <- lm(favoriteCount ~ exclamation + hashtag + source_2 + exclamation*source_2 + hashtag*source_2, data=trump.mutate)
reg.ret4 <- lm(retweetCount ~ exclamation + hashtag + source_2 + exclamation*source_2 + hashtag*source_2, data=trump.mutate)
summary(reg.fav4)
summary(reg.ret4)

reg.fav5 <- lm(favoriteCount ~ exclamation + hashtag + exclamation*source_2 + hashtag*source_2, data=trump.mutate)
#reg.ret4 <- lm(retweetCount ~ exclamation + hashtag + source_2 + exclamation*source_2 + hashtag*source_2, data=trump.mutate)
summary(reg.fav5)
#summary(reg.ret4)

