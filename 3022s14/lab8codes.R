require(Sleuth2)
head(ex0915)
plot(ex0915)
pairs(ex0915)

## Part a
m = lm(Yield~Rainfall, data=ex0915); summary(m)

## Part b
m1 = lm(Yield~Rainfall+I(Rainfall^2), data=ex0915); summary(m1)

# use of 'update' command
m11 = update(m, ~.+I(Rainfall^2)); summary(m11)
# dot '.' adds a term to existing model
m11 = update(m, ~ +I(Rainfall^2)); summary(m11)

## Part c
plot(residuals(m1)~ex0915$Year)
plot(m1$resid ~ ex0915$Year) # gives the same thing

## Plot of rainfall curve on yield
plot(Yield~Rainfall, data=ex0915)

# create a range of values from min to max of Rainfall
xx = seq(min(ex0915$Rainfall), max(ex0915$Rainfall), 1/1000)

# Fit the curve in m1 for those values
yy = m1$coef %*% rbind(1, xx, xx^2)

# plot the curve on the scatterplot
lines(xx,yy)

## Part d
m2 = lm(Yield~Rainfall+I(Rainfall^2)+Year, data=ex0915); summary(m2)

## Part e
m3 = lm(Yield~Rainfall+I(Rainfall^2)+Year+Year*Rainfall, data=ex0915)
summary(m3)

## doing everything using 'update'
m = lm(Yield~Rainfall, data=ex0915); summary(m)

m11 = update(m, ~.+I(Rainfall^2)); summary(m11)

m21 = update(m11, ~.+Year); summary(m21)
m21 = update(m, ~.+I(Rainfall^2) + Year); summary(m21) # same thing

m31 = update(m21, ~.+Year*Rainfall); summary(m31)

# Delete variables from model using update
m22 = update(m31, ~.-I(Rainfall^2)); summary(m22)

## The 'anova' command
# compares two nested models
anova(m,m2)
anova(m1,m2)