bac <- read.table("http://users.stat.umn.edu/~yuan0076/3022/data/bac.txt", header=T)
summary(bac)


(r <- cor(bac$BAC, bac$Beers))
r^2

cor(bac$Beers, bac$BAC)*(sd(bac$BAC)/sd(bac$Beers))

## Linear regresion, lm = linear models
m <- lm(BAC ~ Beers, data=bac)
m.1 <- lm(bac$BAC ~ bac$Beers)
summary(m.1)

plot(bac$BAC ~ bac$Beers, pch=19, cex=0.7, xlab='# of Beers Consumed', 
     ylab='Blood Alcohol Content', main='Scatterplot of BAC vs. Beers')
## abline only works when you have an existing plot
abline(m, col="red", lwd = 2)

newdata <- data.frame(Beers=1:4)
predict(m, newdata, interval="confidence")
predict(m, newdata, interval="confidence", level = 0.95)

newdata <- data.frame(Beers=4)
predict(m, newdata, interval="predict")
## Notice that for the same probability level, confidence interval (CI)
## is narrower than prediction interval (PI)

par(mfrow=c(1,2))
plot(bac$BAC ~ bac$Beers, pch=19, cex=0.7, xlab='# of Beers Consumed', 
     ylab='Blood Alcohol Content', main='Confidence Interval Band',
     ylim=c(-0.05, 0.2))
abline(m, col="red", lwd=2)
newdata <- data.frame(Beers=seq(1, 9, by=0.01))
ciband <- predict(m, newdata, interval="confidence")
lines(ciband[, 2] ~ newdata$Beers, lty=2)
lines(ciband[, 3] ~ newdata$Beers, lty=2)

plot(bac$BAC ~ bac$Beers, pch=19, cex=0.7, xlab='# of Beers Consumed', 
     ylab='Blood Alcohol Content', main='Prediction Interval Band',
     ylim=c(-0.05, 0.2))
abline(m, col="red", lwd=2)
piband <- predict(m, newdata, interval="predict")
lines(piband[, 2] ~ newdata$Beers, lty=2)
lines(piband[, 3] ~ newdata$Beers, lty=2)

par(mfrow=c(1,2))
x <- runif(500)
y <- 5 * x + rnorm(500)
plot(y ~ x, pch=20, cex=0.3)
mtext(expression(sigma(x) * " = " * sigma))

y2 <- 5 * x + rnorm(500) * x
plot(y2 ~ x, pch=20, cex=0.3, ylab="y")
mtext(expression(sigma(x) * " is not constant"))

## Changing conf and pred intervals
x <- runif(1000) ## gives random points from uniform distribution
y <- 5 * x + rnorm(1000) ## rnorm adds N(0,1) errors
m1 = lm(y~x)

par(mfrow=c(1,2))
plot(y ~ x, pch=19, cex=0.5, col=gray(.6), main='Confidence Interval Band')
abline(m1, col="red", lwd=3)
newdata <- data.frame(x=seq(0, 1, 0.01))
ciband <- predict(m1, newdata, interval="confidence")
lines(ciband[, 2] ~ newdata$x, lwd=2, lty=2, col="blue")
lines(ciband[, 3] ~ newdata$x, lwd=2, lty=2, col = "blue")

plot(y ~ x, pch=19, cex=0.5, col=gray(.6), main='Prediction Interval Band')
abline(m1, col="red", lwd=3)
piband <- predict(m1, newdata, interval="prediction")
lines(piband[, 2] ~ newdata$x, lwd=2, lty=2, col="blue")
lines(piband[, 3] ~ newdata$x, lwd=2, lty=2, col = "blue")
par(mfrow=c(1,1))