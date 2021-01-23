setwd("~/Documents/coding")
##################################1.2 Measuring Yield Curve Movements
data2 <- read.csv("treasury.csv")
names(data2)
summary(data2)
View(data2)

#calculating correlation between movements in short vs. long-term interest rates
par(mfrow=c(1,2))
plot(data2$X1.Mo, data2$X3.Mo, xlab="1 month rate", ylab="3 month rate", col=c("red", "blue"))
plot(data2$X1.Mo, data2$X10.Yr, xlab="1 month rate", ylab="10 year rate", col=c("red", "green"))
cor(data2$X1.Mo, data2$X3.Mo)
cor(data2$X1.Mo, data2$X10.Yr)

#computing differences in consecutive daily interest rates for each maturity
data2_omit_date <- data2[,-1]
names(data2_omit_date) <- c("1.Mo", "2.Mo", "3.Mo", "6.Mo", "1.Yr", "2.Yr", "3.Yr", "5.Yr", "7.Yr", "10.Yr", "20.Yr", "30.Yr")
diff_int_rate <- data2_omit_date[2:250,]-data2_omit_date[1:249,]
names(diff_int_rate) <- c("1.Mo", "2.Mo", "3.Mo", "6.Mo", "1.Yr", "2.Yr", "3.Yr", "5.Yr", "7.Yr", "10.Yr", "20.Yr", "30.Yr")

#computing pairwise correlation matrix for the changes in the interest rates
install.packages("corrplot")
library(corrplot)
?corrplot
diff_cor <- cor(diff_int_rate)
corrplot(diff_cor, type = "full", order = "hclust", addrect = 2, 
         tl.col = "black", tl.srt = 60)
#diff_cor2 <- cor(data2_omit_date)
#corrplot(diff_cor2, type = "upper", order = "hclust", addrect = 2, 
#         tl.col = "black", tl.srt = 60)

#creating scatter plot matrix using 1mo, 2mo, 3mo, 20ryr, 30yr changes in interest rates
data2_selected <-data.frame(data2$X1.Mo, data2$X2.Mo, data2$X3.Mo, data2$X20.Yr, data2$X30.Yr)
names(data2_selected) <- c("1.Mo", "2.Mo", "3.Mo", "20.Yr", "30.Yr")
pairs(data2_selected[,1:5], pch=20)

##################################1.3 The AIG Stock Price
data3 <- read.csv("aig.csv")
names(data3)
summary(data3)
View(data3)
#estimate for variance when prices are i.i.d with mean=3.721
mean(data3$Price)
var(data3$Price)

#estimate for variance when prices are i.i.d with mean=E(pricei)=4-0.005*(timei-93500)
time <- c(93500:93559)
price <- 4-0.005*(time-93500)
mean(price)
var(price)

#find correlation between price and time
Y <- data3$Price
X <- data3$Time
cor(X, Y)
round(b1 <- cor(X,Y)*sd(Y)/sd(X),4)
round(b0 <- mean(Y) - (mean(X)-93500)*b1,4)
plot(data3$Time, data3$Price, xlab = "Time", ylab = "Price", pch = 20)
xx <- seq(93500, 93560, length=60)
lines(xx, b0 + b1*(xx-93500), col=2)

#average of the residuals from fit
e <- Y - (b0+b1*(X-93500))
mean(e)

#use the residuals to estimate variance in this fitted model
var(e)
var(time)
price_var <- (0.005)^2*var(time)+var(e)
price_var

#different model, E(pricei)=pricei-1, calculate variance and compare it with other results
model6_Price <- c(NA, data3$Price[2:60])
model6_Price
var(model6_Price, na.rm = TRUE)

?diff
variances <- c(0.018632, 0.018453, 0.007625, 0.008177)
diff(variances)

##################################1.4 Rent Data Exploratory Analysis
data4 <- read.csv("rent.csv")
names(data4)
summary(data4)
View(data4)

#producing margianl distribution and conditional distribution boxplots for Rent, Rent given AC, Rent given rooms
par(mfrow=c(1,3))
boxplot(data4$Rent, ylab = "Rent", main="Marginal distribution of Rent")
boxplot(data4$Rent ~ data4$AC, ylab = "Rent", xlab = "level of AC", col=8, main="Rent given each level of AC")
boxplot(data4$Rent ~ data4$Rooms, ylab = "Rent", xlab = "numbers of Rooms", col=8, main="Rent given numbers of Rooms")

#do 2 ANOVAs to find the SSR, SSE, SST for Rent grouped either by AC and Rooms
print(model1 <- lm(data4$Rent ~ data4$AC))
anova(model1)
round(806435/(38511146+806435),3)

print(model2 <- lm(data4$Rent ~ data4$Rooms))
anova(model2)
round(4579134/(4579134+34738446),3)

#calculate the correlation between SqFt and Rent and use this to fit the regression line
YY <- data4$Rent
XX <- data4$SqFt
cor(XX, YY)
round(b11 <- cor(XX,YY)*sd(YY)/sd(XX),4)
round(b00 <- mean(YY) - (mean(XX))*b11,4)
par(mfrow=c(1,1))
plot(data4$SqFt, data4$Rent, main = "Rent against SqFt", xlab = "SqFt", ylab = "Rent", pch = 20)
xx <- seq(0, 87, length=4)
lines(xx, b00 + b11*(xx), col=2)

#average of the residuals from fit
ee <- YY - (b00+b11*(XX))
mean(ee)
hist(ee, 
     main="Histogram of Residuals", 
     xlab="Residuals", 
     border="black", 
     col="black",
     las=1, 
     breaks=300)
View(ee)
min(ee)
max(ee)
sum(ee)
library(moments)
skewness(ee)
boxplot(ee)
plot(XX, ee, xlab="SqFt", ylab="Residuals", main="Residuals against SqFt", pch=20)
abline(h=0, col=8, lwd=2)

XXX <- subset(data4, SqFt<25)
filtered_XX <- XXX$SqFt
filtered_YY <- XXX$Rent
summary(filtered_XX)
dim(filtered_XX)
View(filtered_XX)

cor(filtered_XX, filtered_YY)
round(b111 <- cor(filtered_XX,filtered_YY)*sd(filtered_YY)/sd(filtered_XX),4)
round(b000 <- mean(filtered_YY) - (mean(filtered_XX))*b111,4)
par(mfrow=c(1,1))
plot(filtered_XX, filtered_YY, xlab = "SqFt", ylab = "Rent", pch = 20)
xxx <- seq(0, 18, length=4)
lines(xxx, b000 + b111*(xxx), col=2)



