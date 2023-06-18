library(Sleuth3) # not in Sleuth2
View(ex1031)
names(ex1031)

## initial plots
plot(BodyMass ~ ClutchVolume, data=ex1031)
plot(log(BodyMass) ~ log(ClutchVolume), data=ex1031, pch=19, cex=0.7)
plot(log(BodyMass)~log(ClutchVolume), data=ex1031, col=as.numeric(Group),
     pch=19, cex=0.7) # colored according to groups

## creating new variables
data = ex1031
data$lBM = log(ex1031$BodyMass)
data$lCV = log(ex1031$ClutchVolume)

## Fit linear models
m1 = lm(lCV ~ lBM+Group, data=data)
m1 = lm(log(ClutchVolume) ~ log(BodyMass)+Group, data=data)

# perform anova
anova(m1)

# F-test for overall regression
m0 = update(m1, ~1)
m0 = lm(lCV ~ 1, data=data) # same as last one
anova(m0,m1)

# F-test for interaction
m.int = lm(lCV ~ lBM+Group+lBM*Group, data=data)
anova(m1,m.int)

# significantly different groups
summary(m1)
names = levels(data$Group)

# change baseline group to Mani
names.minus.mani = names[-3]
names.new = c("Mani", names.minus.mani)
Group1 = factor(data$Group, names.new)

# refit linear model changes baseline
m1new = lm(lCV ~ lBM+Group1, data=data); summary(m1new)
# look for the level with smallest absolute t-value or largest p-value