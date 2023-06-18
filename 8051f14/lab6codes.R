## Stat 8051 hw 4
library(alr4)
## 7.7
# Part 1
plot(Progeny ~ Parent, galtonpeas)
m.weighted <- lm(Progeny ~ Parent,
                 data=galtonpeas, weights= 1/SD^2)
abline(m.weighted, lwd=2)

abline(m.unweighted <- lm(Progeny ~ Parent,
                          data=galtonpeas), lty=2, lwd=2)
legend("topleft", c("WLS", "OLS"), lty=1:2 , lwd=2,
       cex=.8, inset=.02)

# Part 2
compareCoefs(m.weighted, m.unweighted)

## 7.8
# Part 1
par(mfrow=c(1,2))
plot(Weight~Age, jevons)
plot(SD~Age, jevons)
par(mfrow=c(1,1))

# Part 2
mod2 = lm(Weight~Age, data=jevons, weights=SD^2/n)
(z = summary(mod2))

# Part 3
tstat = (z$coef[1,1]-7.9876)/z$coef[1,2]
2*(1-pt(abs(tstat), 3))

# Part 4
c0 = mod2$coef[1]
c1 = mod2$coef[2]
mean.vec = c0+(1:5)*c1
se.vec = with(jevons, sqrt(SD^2 + SD^2/n^2))
pnorm(7.9379, mean=mean.vec, sd=se.vec)

# Part 5
(age.at.min = (7.9379-c0)/c1)
grad = c(-1/c1, -(7.9379-c0)/c1^2)
(se.age.at.min = sqrt(t(grad)%*%vcov(mod2)%*%grad))

# using inbuilt function
deltaMethod(mod2, "(7.9379-Intercept)/Age")

## 7.10
data(fuel2001)
fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Income <- fuel2001$Income/1000
fuel2001$logMiles <- log(fuel2001$Miles,2)

m0 = lm(Fuel~Tax+Dlic+Income+logMiles, data=fuel2001)

# bootstrap function
bootcoefs = function(nsamp){
  n = nrow(fuel2001)
  coef.mat = matrix(0, nrow=nsamp, ncol=4)
  for(i in 1:nsamp){
    isamp = sample(1:n, n, replace=T)
    imod = update(m0, data=fuel2001[isamp,])
    coef.mat[i,] = coef(imod)[-1]
  }
  return(coef.mat)
}

set.seed(805104)
beta.matrix = bootcoefs(1e4)
par(mfrow=c(2,2))
apply(beta.matrix, 2, hist)
par(mfrow=c(1,1))

# compare with actual CI
# actual
confint(m0)[-1,]

# bootstrap approx
mean.vec = apply(beta.matrix, 2, mean)
sd.vec = apply(beta.matrix, 2, sd)
cbind(mean.vec-1.96*sd.vec, mean.vec+1.96*sd.vec)

# bootstrap actual
qfun = function(x) quantile(x, c(.025,.975))
t(apply(beta.matrix, 2, qfun))

## 10.2
Highway$sigs1 = with(Highway, (sigs*len+1)/len)
# Part 1
f = log(rate)~log(adt)+log(trks)+lane+acpt+log(sigs1)+itg+slim+log(len)+lwid+shld+htype
mfull = lm(f, Highway)
# backward selection
mbk <- step(mfull, scope=c(lower=.~log(len)),
                            direction="backward", trace=F)
mbk$call
# forward selection
mfw <- step(lm(log(rate)~log(len), Highway),
             scope=f, direction="forward", trace=F)
mfw$call

# Part 1
# FS
f1 = log(rate*len)~log(adt)+log(trks)+lane+acpt+log(sigs1)+itg+slim+lwid+shld+htype
m2fw <- step(lm(log(rate*len)~lwid, Highway), scope=f1, direction="forward")

# BE
m2be <- step(lm(f1, Highway), scope=c(lower=.~lwid), direction="backward")
