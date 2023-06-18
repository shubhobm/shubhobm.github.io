## Homework 2
## Ex 2.23
library(Sleuth2)
names(ex0223)
summary(ex0223)
attach(ex0223)
boxplot(FatalitiesChange~Increase, main="Side by side boxplots of fatality change",
        ylab="Change in fatalities",xlab="Increased speed limits in 1996")

## tapply function
(n = tapply(FatalitiesChange, Increase, length))
(Ave = tapply(FatalitiesChange, Increase, mean))
(Med = tapply(FatalitiesChange, Increase, median))
(Stdev = tapply(FatalitiesChange, Increase, sd))

print(cbind(n,Ave,Med,Stdev))
print(cbind(n,Ave,Med,Stdev), digits=3) # digits specifies accuracy of output

## t-test
t.test(FatalitiesChange~Increase, alternative="less", var.equal=T)
t.test(FatalitiesChange~Increase, alternative="two.sided", var.equal=T)

## Ex 3.33
names(ex0333)
View(ex0333)
summary(ex0333)
attach(ex0333)

## side-by-side boxplots
boxplot(Brainsize~Littersize,main="Small and Large Brains",
xlab='Litter size',ylab="Brain size")

## side-by-side histograms
par(mfrow=c(1,2))
hist(Brainsize[Littersize=="Small"],main="small",xlab="small")
hist(Brainsize[Littersize=="Large"],main="large",xlab="large")
par(mfrow=c(1,1))

## histogram options
hist(Brainsize,main="Histogram of brain sizes of mammals",xlab="Brain size")
hist(Brainsize,main="Histogram of brain sizes of mammals",xlab="Brain size",
     breaks = 15)

hist(Brainsize,main="Histogram of brain sizes of mammals",xlab="Brain size",
     breaks = 15, freq=F)
lines(density(Brainsize))
lines(density(Brainsize), lwd=2, col='blue')

## analyzing log of data
logBrain = log(Brainsize)

# normal qq plots
par(mfrow=c(1,2))
qqnorm(Brainsize, xlab="Actual data"); qqline(Brainsize)
qqnorm(logBrain, xlab="Log-transformed data"); qqline(logBrain)
par(mfrow=c(1,1))

hist(logBrain)

## t-test on log-transformed data
t.test(logBrain~Littersize,var.equal=T)
(CI = c(-0.796046082,  0.002109577))
exp(CI)
exp(median(logBrain))
median(Brainsize)
     
## the apply function
View(trees)

# Three ways to find
c(mean(trees$Girth), mean(trees$Height), mean(trees$Volume))
apply(trees, 2, mean)
colMeans(trees) # can also do rowMeans(trees)

## boxplot using apply
par(mfrow=c(1,3))
apply(trees, 2, boxplot)
par(mfrow=c(1,1))

