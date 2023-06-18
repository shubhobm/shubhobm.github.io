## Stat 8051 hw 3
library(alr4)
## 6.7
data(fuel2001)
fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Income <- fuel2001$Income/1000
fuel2001$logMiles <- log(fuel2001$Miles,2)

m0 = lm(Fuel~Tax+Dlic+Income+logMiles, data=fuel2001)
m1 = lm(Fuel~logMiles+Income+Dlic+Tax, data=fuel2001)

# Part 1
anova(m0)
anova(m1)

# Part 2
Anova(m0, type=2)
Anova(m1, type=2)

## 6.9
m1 <- lm(Y ~ X1 + I(X1^2) + X2 + I(X2^2) + X1:X2, cakes)
m1 = lm(Y~(X1+X2)^2 + I(X1^2) + I(X2^2), cakes)
m2 <- update(m1, ~ . - X1:X2)
m3 <- update(m1, ~ . - I(X1^2))
m4 <- update(m1, ~ . - X1 - I(X1^2) - X1:X2)

anova(m2, m1)
anova(m3, m1)
anova(m4, m1)

## 6.14
# Part 1
A = lm(log(acrePrice)~year, data=MinnLand)
summary(A)

# Part 2
MinnLand$fyear = factor(paste(MinnLand$year))
B = lm(log(acrePrice)~fyear, data=MinnLand)
summary(B)

# Part 4
anova(A,B)
# Alternate way
library(alr3)
pureErrorAnova(A)

## 8.2
# Part 1
library(MASS)
z = boxcox(lm(Distance~Speed, data=stopping))
z$x[which.max(z$y)]

invResPlot(lm(Distance~Speed, data=stopping))

# Part 2
par(mfrow=c(1,3))
plot(Distance~1/Speed, stopping)
plot(Distance~log(Speed), stopping)
plot(Distance~Speed, stopping)
par(mfrow=c(1,1))

# Part 3
plot(Distance~Speed^2, stopping)

# Part 4
reg1 = lm(Distance~Speed+I(Speed^2),
          weight=1/Speed^2, data=stopping)
# Check
reg2 = lm(Distance^.5~Speed, data=stopping)

plot(Distance~Speed, data=stopping)
lines(fitted.values(reg1)~stopping$Speed,
      type = "l",col = "blue")
lines((fitted.values(reg2))^2~stopping$Speed,
      type = "l",col = "red")

## 9.11
Fuel = c(514.279, 374.164, 426.349, 842.792, 317.492)
ehat = c(-163.145, -137.599, -102.409, -183.499, -49.452)
h = c(.256, .162, .206, .084, .415)
sighat = 64.891
n = 51; p1 = 5
r = ehat/(sighat*sqrt(1-h))
t = r*sqrt((n-p1-1)/(n-p1-r^2))
D = r^2*h/(p1*(1-h))
d = (data.frame(cbind(r,t,D),
               row.names = c("Alaska", "NY", "Hawaii", "Wyoming", "Dist. Col")))

pmin(n*2*pt(-abs(t),46),1)
