library(Sleuth2)

## Problem 7.24
names(ex0726)
mden <- lm(Denmark~Year, data = ex0726); summary(mden)
summary(mneth <- lm(Netherlands~Year, data = ex0726))
summary(mcan <- lm(Canada~Year, data = ex0726))
summary(mus <- lm(Usa~Year, data = ex0726))

summary(lm(ex0726$Denmark ~ ex0726$Year))

# use of 'with' function
mden = with(ex0726, lm(Denmark~Year)); summary(mden)
# works with functions as boxplot, plot, anova as well!

par(mfrow=c(4,1))
plot(Denmark~Year, data=ex0726, main="Denmark"); abline(mden)
plot(Netherlands~Year, data=ex0726, main="Netherlands"); abline(mneth)
plot(Canada~Year, data=ex0726, main="Canada"); abline(mcan)
plot(Usa~Year, data=ex0726, main="USA"); abline(mus)
par(mfrow=c(1,1))

## Problem 8.23
data = ex0823; names(data)
data$logWine = log(data$Wine)
data$logMortality = log(data$Mortality)
pairs(data[,2:5])
cor(data[,2:5])

## linear models: both unscaled
m1 = with(data, lm(Mortality~Wine)); summary(m1)
par(mfrow=c(2,2)); plot(m1); par(mfrow=c(1,1))
plot(Mortality~Wine, data=data, pch=19,cex=.7,
     main='Wine consumption vs. mortality',
     xlab="wine consumption",ylab="mortality")
abline(m1,lwd=2,col='red')

# y scaled
m2 = with(data, lm(logMortality~Wine)); summary(m2)
plot(logMortality~Wine, data=data, pch=19,cex=.7,
     main='Wine consumption vs. mortality',
     xlab="wine consumption",ylab="log of mortality")
abline(m2,lwd=2,col='red')
par(mfrow=c(2,2)); plot(m2); par(mfrow=c(1,1))

# both scaled
m3 = with(data, lm(logMortality~logWine)); summary(m3)
plot(logMortality~logWine, data=data, pch=19,cex=.7,
     main='Wine consumption vs. mortality',xlab="log of wine consumption",ylab="log of mortality")
abline(m3,lwd=2,col='red')
par(mfrow=c(2,2)); plot(m3); par(mfrow=c(1,1))

# outlier detection
newdata = data[-c(17,18),2:5]
pairs(newdata)
m11 = with(newdata, lm(Mortality~Wine)); summary(m1)
par(mfrow=c(2,2)); plot(m11); par(mfrow=c(1,1))

# y scaled
m21 = with(newdata, lm(logMortality~Wine)); summary(m2)
par(mfrow=c(2,2)); plot(m21); par(mfrow=c(1,1))

# both scaled
m31 = with(newdata, lm(logMortality~logWine)); summary(m3)
par(mfrow=c(2,2)); plot(m31); par(mfrow=c(1,1))

## data example: bush-buchanan-palm beach example
library(Sleuth2)
plot(case0801, pch=20, cex=2)

library(alr3)
data <- subset(florida, Buchanan < 2500)
m1 <- lm(Buchanan ~ Bush, florida)
m2 <- lm(Buchanan ~ Bush, data)
plot(florida$Buchanan ~ florida$Bush, pch=20, xlab="Bush", ylab="Buchanan")
abline(m1, col="red", lwd=2)
abline(m2, lty=2, lwd=2)
legend(0, 3500, c("All points", "Omit outlier"),
       col=c("red", "black"), lty=c(1,2), lwd=c(2,2))

plot(case0802$Time ~ case0802$Voltage, pch=19, cex=0.7, ylim=c(-100, 2500),
     xlab="Voltage", ylab="Time", main="Is a transformation necessary?")
m <- lm(Time ~ Voltage, case0802)
abline(m, col="red")
summary(m)
