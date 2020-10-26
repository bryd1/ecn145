setwd("c:/Users/btdan/Desktop/ECN 145/hw")

#1
library(readr)
ps1 <- read.csv("ps1_data.csv")
View(ps1)

Zcta5 <- c(ps1$V1)
City <- c(ps1$V2)
Bedroom <- c(ps1$V3)
MedianPrice <- c(ps1$V4)
DrivingDistance <- c(ps1$V5)
DrivingTime <- c(ps1$V6)
MathScore <- c(ps1$V7)

#2
str(ps1)
names(ps1)
summary(ps1)

is.na(ps1)
ps1[!complete.cases(ps1),]
getOption("max.print")

ps1 <- na.omit(ps1)
summary(ps1)


#3
plot(x = ps1$DrivingTime, y = ps1$MedianPrice, xlab = "Driving Time", ylab = "Median Price", pch = 19)
abline(lm(ps1$MedianPrice ~ ps1$DrivingTime), col = "red")

#4
fit <- lm(MedianPrice ~ DrivingTime, data = ps1)
summary(fit)

#5
fitbed <- lm(MedianPrice ~ DrivingTime + factor(Bedroom), data=ps1)
summary(fitbed)


#6
plot(x = ps1$DrivingDistance, y = ps1$MathScore, xlab = "Driving Distance", ylab = "Math Score", pch = 19)
abline(lm(ps1$MathScore ~ ps1$DrivingDistance), col = "red")
fitmath <- lm(MathScore ~ DrivingDistance, data = ps1)
summary(ps1)

#7
fitfull <- lm(MedianPrice ~ DrivingTime + factor(Bedroom) + MathScore, data = ps1)
summary(fitfull)

#10
fitsize <- lm(MathScore ~ factor(Bedroom), data = ps1)

summary(fitsize)

