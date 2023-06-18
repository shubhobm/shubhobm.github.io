library(Sleuth3)
View(case0902)
names(case0902)

plot(case0902[,2:5])

## fit regression model
m1 = lm(Brain ~ Body+Gestation+log(Litter), data=case0902)
summary(m1)

anova(m1)

## does diagnostic plots
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

# plot leverages etc
leverages = hat(model.matrix(m1)); plot(leverages)
stud.res = rstudent(m1); plot(stud.res); abline(h=0)
CooksD = cooks.distance(m1); plot(CooksD)

## work without observation 3
(outlier.pt = case0902[3,])
new.data = case0902[-3,]

m2 = update(m1, data=new.data)
summary(m2)

par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

# eliminating more influential points does not improve fit
(more.outliers = case0902[c(25,48,53),])
new.data1 = case0902[-c(3,25,48,53),]

m3 = update(m2, data=new.data1)

par(mfrow=c(2,2))
plot(m3)
par(mfrow=c(1,1))

## log transformation on all variables
m = lm(log(Brain) ~ log(Body)+log(Gestation)+log(Litter), data=case0902)
summary(m)

par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))