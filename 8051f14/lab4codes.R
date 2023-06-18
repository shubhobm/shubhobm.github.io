## Stat 8051 hw 2
library(alr4)
## 3.2
# Part 1
logppgdp = log(UN11$ppgdp)
data32 = data.frame(with(UN11,
                         cbind(fertility, logppgdp, pctUrban)))
pairs(data32)
# Part 2
m1 = lm(fertility~logppgdp, data=data32)
m2 = lm(fertility~pctUrban, data=data32)
summary(m1)
summary(m2)

# Part 3
mab = lm(logppgdp~pctUrban, data=data32)
mba = lm(pctUrban~logppgdp, data=data32)
par(mfrow=c(1,2))
plot(residuals(m2)~residuals(mab), pch=19, cex=.8) # avp for fertility~logppgdp, adj for pctUrban
plot(residuals(m1)~residuals(mba), pch=19, cex=.8) # avp for fertility~pctUrban, adj for fertility
par(mfrow=c(1,1))
m.full = lm(fertility~logppgdp+pctUrban, data=data32)
summary(m.full)

# Part 4
m.res2ba = lm(residuals(m2)~residuals(mab))
m.res2ba$coef

# Part 5
two.resids = cbind(residuals(m.full), residuals(m.res2ba))
head(two.resids)

# Part 6
summary(m.full)
summary(m.res2ba)

## 4.2
names(Transact)
Transact = within(Transact, {a=(t1+t2)/2
                             d=t1-t2})
M1 = lm(time~t1+t2, Transact)
M2 = lm(time~a+d, Transact)
M3 = lm(time~t2+d, Transact)
M4 = lm(time~t1+t2+a+d, Transact)
# Part 1
coef(M4)

# Part 2 and 3
coef(M1)
coef(M2)
coef(M3)
coef(M4)

# 4.12
randplot = function(n, sigma, X, e){
  Y = 2+3*X+sigma*e
  mod = lm(Y~X)
  plot(Y~X)
  abline(mod)
  abline(2,3, col="red")
}

# Part 1
randplot(300, 1, rnorm(300), rnorm(300))

# Part 2
par(mfrow=c(1,2))
randplot(300, 3, rnorm(300), rnorm(300))
randplot(300, 6, rnorm(300), rnorm(300))
par(mfrow=c(1,1))

# Part 3
randplot(300, 1, rnorm(300), rnorm(300)/rnorm(300))

## 5.8
# Part 1
head(cakes)
m1 = lm(Y ~ X1+X2+I(X1^2)+I(X2^2)+X1:X2, data=cakes)
m1 = lm(Y ~ X1+X2+I(X1^2)+I(X2^2)+X1*X2, data=cakes)
m1 = lm(Y ~ (X1+X2)^2+I(X1^2)+I(X2^2), data=cakes)
summary(m1)

# Part 2
m2 = update(m1, ~.*block)
m2 = update(m1, ~.+block+X1*block+X2*block)
summary(m2)

## 5.14
names(BGSall)
# Part 1
plot(HT18~HT9, data=BGSall)
plot(HT18~HT9,
     pch=ifelse(Sex==0,1,19),
     data=BGSall)
legend("topleft",c("Male","Female"), pch=c(1,19), title="Sex")

# Part 2
# additive model or parallel model
m.add = lm(HT18 ~ HT9+Sex, BGSall)
coefs = coef(m.add)
abline(coefs[1],coefs[2], lty=2, lwd=2)
abline(coefs[1]+coefs[3],coefs[2], lwd=2)
# interactive model
m.int = update(m.add, ~.^2)
anova(m.add, m.int  )

# Part 3
confint(m.add, "Sex")
confint(m.add)
