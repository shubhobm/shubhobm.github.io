## Stat 8051 hw 1
library(alr4)
## 2.1
plot(wt ~ ht, Htwt)
abline(lm(wt ~ ht, Htwt))

n <- dim(Htwt)[1]
(ave <- colMeans(Htwt))
apply(Htwt, 2,mean)
xbar <- ave[1]
ybar <- ave[2]
print(crossprod <- (n-1) * cov(Htwt), digits=5)

SXX <- crossprod[1, 1]
SYY <- crossprod[2, 2]
SXY <- crossprod[1, 2]
(coefs <- c(Intercept=ybar - (SXY/SXX) * xbar, Slope=SXY/SXX))
(s2 <- (SYY - SXY^2/SXX)/(n - 2))
(secoefs <- c(Intercept=sqrt(s2 * (1/n + xbar^2/SXX)),
              Slope=sqrt(s2 * (1/SXX))))
(cov12 <- - s2 * xbar/SXX)
(tvals <- coefs/secoefs)

## 2.13
colMeans(Heights)
var(Heights)
m1 <- lm(dheight ~ mheight, data=Heights)
summary(m1)

confint(m1, level=0.99)
predict(m1, newdata=data.frame(mheight=64),
        interval="prediction",
        level=.99)

## 2.17
m0 <- lm(Y ~ X - 1, data=snake)
summary(m0)
confint(m0)
tval <- (coef(m0)[1] - 0.49)/ sqrt(vcov(m0)[1,1])
df <- dim(snake)[1] - 1
data.frame(tval = tval, df=df, pval = 1 - pt(abs(tval), df))

par(mfrow=c(1,2))
plot(Y ~ X, snake)
m1 <- lm(Y ~ X, snake)
abline(m0, lwd=2)
abline(m1, lty=2, lwd=2)
legend("topleft", c("No intercept", "Intercept"),
       lty=1:2, inset=0.02, lwd=2)
plot(residuals(m0) ~ X, snake)
abline(h=0)
par(mfrow=c(1,1))
