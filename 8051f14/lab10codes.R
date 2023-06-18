## Stat 8051 hw 7
library(faraway)
## Faraway Ch2 Prob 2
names(wbca)
# Part a
lmod1 = glm(Class~., data=wbca, family=binomial)
summary(lmod1)

(X2 = sum(residuals(lmod1, type="pearson")^2))
1-pchisq(X2, df=lmod1$df.residual)

# test for significance
with(lmod.summary,
     1-pchisq(null.deviance-deviance,
              df=df.null-df.residual))

# checks for individual significances
require(car)
Anova(lmod1)

# pseudo R^2
with(lmod.summary, 1-deviance/null.deviance)

# Part b
(lmod2 = step(lmod1, trace=F))

# Part c
# make new data frame
newdata = c(1,1,3,2,1,1,4,1,1)
newdata = data.frame(t(newdata))
colnames(newdata) = colnames(wbca)[-1]

p = predict(lmod2, newdata=newdata, se.fit=TRUE)
(CI = with(p, c(fit-1.96*se.fit, fit, fit+1.96*se.fit)))
## IMPORTANT: this is CI for the log-odds
## http://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression
# CI in original scale
lmod2$family$linkinv(CI)

# Part d
fullpred = predict(lmod2, newdata=wbca,
                   type="response")
# or use fullpred = lmod2$fitted
pred5 = ifelse(fullpred>.5, 1, 0)

sum(pred5!=wbca$Class)
table(pred5, wbca$Class)

# Part e
pred9 = ifelse(fullpred>.9, 1, 0)

sum(pred9!=wbca$Class)
table(pred9, wbca$Class)

# Part f
test = seq(3, nrow(wbca), by=3)
lmod21 = update(lmod2, subset = -test)

fullpred1 = predict(lmod21, newdata=wbca[test,],
                    type="response")

pred51 = ifelse(fullpred1>.5, 1, 0)
sum(pred51!=wbca$Class[test])
table(pred51, wbca$Class[test])

pred91 = ifelse(fullpred1>.9, 1, 0)
sum(pred91!=wbca$Class[test])
table(pred91, wbca$Class[test])

## Faraway Ch2 Prob 3
names(pima)

# Part a
summary(pima)
plot(pima[,-ncol(pima)])

# drop insulin=0 or triceps=0 cases
ind = with(pima, which(insulin==0 | triceps==0 |
                         glucose==0 | diastolic==0 | bmi==0))

# Part b
lmod.pima = glm(test~pregnant+glucose+diastolic+triceps+insulin+bmi+diabetes+age,
                data=pima, subset=-ind,
                family=binomial)
summary(lmod.pima)

(X2 = sum(residuals(lmod.pima, type="pearson")^2))
1-pchisq(X2, df=lmod.pima$df.residual)

# Part c
(diff.bmi = with(pima,
                 quantile(bmi, .75) - quantile(bmi, .25)))
(diff.logodd = 0.087*diff.bmi)
(se.logodd = 0.015*diff.bmi)
(CI.logodd = c(diff.logodd-1.96*se.logodd, diff.logodd, diff.logodd+1.96*se.logodd))
(CI.odd = exp(CI.logodd))

# Part d
with(pima[-which(pima$diastolic==0),], t.test(diastolic~test))

# Part e
par(mfrow=c(2,2))
plot(lmod.pima)
par(mfrow=c(1,1))

# Part f
newpima = data.frame(t(c(1, 99, 64, 22, 76, 27, .25, 25)))
colnames(newpima) = colnames(pima)[-ncol(pima)]

p.pima = predict(lmod.pima, newdata=newpima, se.fit=TRUE)
(CI.pima = with(p.pima, c(fit-1.96*se.fit, fit, fit+1.96*se.fit)))
lmod.pima$family$linkinv(CI.pima)
