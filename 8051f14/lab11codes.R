## Stat 8051 hw 8
library(faraway)
## Faraway Ch3 Prob 1
plot.ts(discoveries)

n = as.numeric(discoveries)
discoveries1 = data.frame(n)
discoveries1$year = 1860:1959

m1.pois = glm(n~poly(year,2), family=poisson, data=discoveries1)
summary(m1.pois)

# dispersion
(dp <- sum(residuals(m1.pois,type="pearson")^2)/m1.pois$df.res)

# test for outliers
par(mfrow=c(2,2))
plot(m1.pois)
par(mfrow=c(1,1))

require(car)
halfnorm(residuals(m1.pois))
outlierTest(m1.pois)

m11.pois = update(m1.pois, subset=-26)
summary(m11.pois)

## Faraway Ch3 Prob 2
head(salmonella)
m2.pois = glm(colonies~dose, family=poisson, data=salmonella)
summary(m2.pois)

# different ways of checking for overdispersion
# manually
(dp <- sum(residuals(m2.pois,type="pearson")^2)/m2.pois$df.res)
summary(m2.pois, dispersion=dp)

# R function
require(AER)
dispersiontest(m2.pois, alternative="greater")

# another R function
require(MASS)
m2.nb = glm.nb(colonies~dose, data=salmonella)
summary(m2.nb)

require(pscl)
odTest(m2.nb)

# cubic fits
m21.pois = glm(colonies~poly(dose,3), family=poisson, data=salmonella)
summary(m21.pois)
dispersiontest(m21.pois, alternative="greater")

m21.nb = glm.nb(colonies~poly(dose,3), data=salmonella)
summary(m21.nb)

## Faraway Ch3 Prob 3
head(esdcomp)

# Rate model
(m3.pois = glm(complaints~visits+
                residency+gender+revenue+hours,
              family=poisson,
              data=esdcomp))

m31.pois = update(m3.pois, ~.-visits+log(visits))
summary(m31.pois)

Anova(m3.pois)
dispersiontest(m31.pois)

m32.pois = update(m3.pois, ~.-visits+offset(log(visits)))
summary(m32.pois)

Anova(m32.pois)
dispersiontest(m32.pois)

1-pchisq(deviance(m32.pois), df.residual(m32.pois))

# negative binomial
m3.nb = glm.nb(complaints~log(visits)+
                 (residency+gender+revenue+hours),
            ,data=esdcomp)
1-pchisq(deviance(m3.nb), df.residual(m3.nb))
summary(m3.nb)
Anova(m3.nb)
