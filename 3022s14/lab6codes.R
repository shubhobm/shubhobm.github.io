library(Sleuth2)
ex916 = ex0328
# ex0327 for Sleuth3

## A bunch of scatterplots
# original scale
plot(Removed~Duration, data = ex916,
     col = as.numeric(Bee), pch = as.numeric(Bee),
     xlab="Duration of visit", ylab="Proportion of pollen removed",
     main="Coded scatterplot")
legend('topleft', col=c(1,2), pch=c(1,2), legend=c('Queen','Worker'))

# logit proportion  vs.duration
ex916$lremoved = with(ex916, log(Removed/(1-Removed)))
plot(lremoved~Duration, data = ex916,
     col = as.numeric(Bee), pch = as.numeric(Bee),
     xlab="Duration of visit", ylab="logit Proportion of pollen removed", main="Coded scatterplot")
legend('topleft', col=c(1,2), pch=c(1,2), legend=c('Queen','Worker'))

# logit proportion vs. log duration
ex916$lduration = with(ex916, log(Duration))
plot(lremoved~lduration, data = ex916,
     col = as.numeric(Bee), pch = as.numeric(Bee),
     xlab="log Duration of visit", ylab="logit Proportion of pollen removed", main="Coded scatterplot")
legend('topleft', col=c(1,2), pch=c(1,2), legend=c('Queen','Worker'))

## Linear models
# log vs. log, interactive model with bees
mbee = lm(lremoved~lduration*Bee, data = ex916)

mbee = lm(lremoved~(lduration+Bee)^2, data = ex916)

mbee = lm(lremoved~lduration+Bee+lduration*Bee, data = ex916)
summary(mbee)

# log vs. log, additive model with bees
mbee2 = lm(lremoved~lduration+Bee, data = ex916)
summary(mbee2)
# 