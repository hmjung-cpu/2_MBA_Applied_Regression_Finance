setwd("~/Documents/coding")
##################################1.1 Simulation from the Simple Linear Regression(SLR) Model
###1.(i) show the scatter plot of Y vs X along with the true regression line
set.seed(222)
X.i <- rnorm(n = 100, mean = -1.0, sd = sqrt(2.5))  #generate 100 samples of X~N(-1.0,2.5) and for each draw,
E.i <- rnorm(n = 100, mean = 0, sd = sqrt(3))       #Ei~N(0,3) iid
Y.i = 2.5 + 2.0*X.i + E.i
#simulate Yi from the SLR model Yi=2.5+2.0Xi+Ei where iid Ei~N(0.3)
?rnorm
plot(X.i,Y.i, main = "Y ~ X", col=c("red", "blue"), pch = 20) #scatter plot of Y vs X
true.reg <- lm(Y.i ~ X.i) #regression line
abline(true.reg)                                              #fitting true regression line(black)
abline(subset.25_true.reg, col=c("red"))                   #fitting true regression line(red) 25
abline(subset.75_true.reg, col=c("green"))                 #fitting true regression line(green) 75
#scatter.smooth(X.i,Y.i, main = "Y vs X") #difference?

###1.(ii) split sample into 2 subsets and run regression of Y on X for each subset
X.i_subset.25 = X.i[1:25];Y.i_subset.25 = Y.i[1:25];E.i_subset.25 = E.i[1:25]
X.i_subset.75 = X.i[26:100];Y.i_subset.75 = Y.i[26:100];E.i_subset.75 = E.i[26:100]

#simulate Yi_subset.25(yellow) from the SLR model Yi=2.5+2.0Xi+Ei where iid Ei~N(0.3)
Y.i_subset.25 = 2.5 + 2.0*X.i_subset.25 + E.i_subset.25                          
#plot(X.i_subset.25,Y.i_subset.25, main = "subset 25 Y ~ X", col=c("red", "blue"), pch = 20) #scatter plot of Y vs X
subset.25_true.reg <- lm(Y.i_subset.25 ~ X.i_subset.25)                                     #regression line
abline(subset.25_true.reg, col=c("red"))                                              #fitting true regression line

#simulate Yi_subset.25(green) from the SLR model Yi=2.5+2.0Xi+Ei where iid Ei~N(0.3)
Y.i_subset.75 = 2.5 + 2.0*X.i_subset.75 + E.i_subset.75                          
#plot(X.i_subset.75,Y.i_subset.75, main = "subset 75 Y ~ X", col=c("red", "blue"), pch = 20) #scatter plot of Y vs X
subset.75_true.reg <- lm(Y.i_subset.75 ~ X.i_subset.75)                                     #regression line
abline(subset.75_true.reg, col=c("green"))                                              #fitting true regression line

###1.(iii)Marginal sample mean for Y and true marginal mean
mean(Y.i_subset.25); mean(Y.i_subset.75); mean(Y.i)


###1.(iv)Add bounds of the true 85% prediction interval to your plot
lb <- qnorm(0.15/2, mean=mean(Y.i), sd=sd(E.i))
hb <- qnorm(1-0.15/2, mean=mean(Y.i), sd=sd(E.i))
L <- Y.i+lb
H <- Y.i+hb
X.i
newdata=data.frame(X.i,L,H,Y.i)
##summary(X.i)
?seq
newX.i1 <- seq(-4.657762, 5.757825, by=0.05)
length(newdata$X.i)
##newX.i <- seq(-4.67218, 3.22175, by=0.05)
plot(X.i,Y.i, main = "Y ~ X", col=c("red", "blue"), pch = 20) #scatter plot of Y vs X
abline(true.reg, col="black")


#conf_interval <- predict(true.reg, newdata=data.frame(X.i=newX.i), interval="confidence",
#                         level = 0.85)
#lines(newX.i, conf_interval[,2], col="blue", lty=2)
#lines(newX.i, conf_interval[,3], col="blue", lty=2)

#pred_interval <- predict(true.reg, newdata=data.frame(X.i=newX.i1), interval="prediction",
#                         level = 0.85)
#lines(newX.i1, pred_interval[,2], col="orange", lty=2)
#lines(newX.i1, pred_interval[,3], col="orange", lty=2)

lines(newdata$X.i, lm(H~X.i), col="orange", lty=2)
lines(newdata$X.i, lm(L~X.i), col="orange", lty=2)
abline(lm(H~X.i), col="orange", lty=2)
abline(lm(L~X.i), col="orange", lty=2)
summary(lm(H~X.i))

8.3180 +2.0825*(-0.3243632)

?lines
##################################1.2 Leverage Effect

###2.(i) create plot of returns of the SPX RSPX vs returns of the VIX RVIX
data2 <- read.csv("leverage.csv")
RSPX <- data2$SPX
RVIX <- data2$VIX
plot(RVIX,RSPX, main = "RSPX ~ RVIX", xlab = "RVIX", ylab = "RSPX", 
     col=c("red", "blue"), pch = 20)

###2.(ii) find the least squres fit to the model RSPXi=bo+b1*RVIXi+ei
#(a)using lm command
lm(RSPX ~ RVIX) #b0=3180.43, b1=-21.89
#(b)by calculating a correlation and standard deviations 
b1 <- cor(RSPX,RVIX)*sd(RSPX)/sd(RVIX) #cor(X,Y)*sd(Y)/sd(X)
b0 <- mean(RSPX) - mean(RVIX)*b1
cbind(b0,b1)
#and overlay the fitted line on the plot from part (i)
fit <- lm(RSPX ~ RVIX)
abline(fit, col=c("black"))

###2.(iii) what is variance decomposition for the regression model fit in (ii) SST,SSE,SSR?
fit <- lm(RSPX ~ RVIX)
anova(fit)
SSE <- 7715280
SSR <- 4016285
SST <- SSE+SSR
#what is R-square for the regression?
R.square <- SSR/SST 
R.square
###2.(iv) calculate correlation between RSPX and RVIX using regresion R^2
#and verify whether this delivers the same result as your correlation calculation in (ii)(b)
sqrt(R.square)
cor(RSPX,RVIX)

###2.(v) Suppose you see 10% VIX increase. Using values obtained in (ii), 
#what would you expect the return of S&P Index to be? 
b0 + b1*10
#If we extend the model so that
#RSPXi=B0+B1*RVIXi+Ei, Ei~N(0,sigma^2) (Ei is iid)
#where B0=b0, B1=b1, sigma=0.025, what is the 90% predictive interval for the S&P 500 returns?
e.i <- rnorm(n = 503, mean = 0, sd = 0.025)
RSPX= b0 + b1*RVIX + e.i 
extend_model <- lm(RSPX ~ RVIX)

b0;b1
mean(e.i)
mean(b1 + b0*10 +e.i); b1 + b0*10 + mean(e.i)

#90% predictive interval for data point
pred_interval_3 <- predict(extend_model, newdata=data.frame(RVIX=10), interval="prediction",
                           level = 0.90)
pred_interval_3
###fit      lwr      upr
###1 2961.553 2961.514 2961.593

conf_interval_3 <- predict(extend_model, newdata=data.frame(RVIX=10), interval="confidence",
                           level = 0.90)
conf_interval_3
###fit     lwr      upr
###1 2961.553 2961.55 2961.557

##################################1.3 Vanguard ETF
#We will be modeling individual Vanguard ETF and the S&P500 index. 
corp <- read.csv("vanguard.csv")
len<-dim(corp)[1]
RetSPX<-diff(log(corp$SPX.INDEX))*52-corp$TBILL[2:(len)]/100       ## or log(corp$SPX[2:len])-log(corp$SPX[1:len-1])
RetBND<-diff(log(corp$BND))*52-corp$TBILL[2:(len)]/100        
plot(RetSPX,RetBND,xlab="SPX Returns",ylab="BND Returns")
reg<-lm(RetBND~RetSPX)
xx <- seq(range(RetBND)[1],range(RetBND)[2],length=4)
lines(xx, reg$coeff[1] + reg$coeff[2]*xx, col=2)

Retcorp<-52*log(corp[2:len,2:(dim(corp)[2]-2)])-52*log(corp[1:len-1,2:(dim(corp)[2]-2)])-corp$TBILL[2:(len)]/100     

## Run the regression for each of the corps
print(CAPM <- lm(as.matrix(Retcorp) ~ RetSPX))
print(CAPM$coeff[2,]) #beta
print(CAPM$coeff[1,]) #alpha
beta <- CAPM$coeff[2,]
alpha <- CAPM$coeff[1,]
plot(beta, alpha, pch=19, col="darkgreen")

text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(corp)[2:(dim(corp)[2]-2)], 
     col=2, cex=0.65, pos=3)
abline(h=0,v=1,col=8,lwd=2) 

beta>1.0
library(dplyr)
data3<-cbind(alpha,beta)
view(data3)
ETF <-as.data.frame(data3)
ETF <- ETF %>% 
  mutate(alpha_over0=ifelse(alpha>=0,"TRUE","FALSE")) %>%
  mutate(beta_vs1=ifelse(beta>1,"Larger","Smaller"))

ETF <- ETF[,-(4)]
