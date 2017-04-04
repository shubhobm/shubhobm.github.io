library(Sleuth3)
head(case1202)
pairs(case1202[,c(1,4,5,6,7)])

# Full regression model
m.full = lm(log(Bsal) ~ Senior+Age+Educ+Exper, data=case1202)
summary(m.full)

# stepwise backward selection
step(m.full, direction="backward")

# doesn't print output
step(m.full, direction="backward", trace=F)

# forward selection
m0 = lm(log(Bsal) ~ 1, data=case1202)
step(m0, scope = ~Senior+Age+Educ+Exper, direction="forward")

# doesn't print output
step(m0, scope = ~Senior+Age+Educ+Exper, direction="forward", trace=F)

# storing selected models
m.back = step(m.full, direction="backward", trace=F)
m.for = step(m0, scope = ~Senior+Age+Educ+Exper, direction="forward", trace=F)

# introducing 'sex' as new variable
m4.full = lm(log(Bsal) ~ Senior+Age+Educ+Exper+Sex, data=case1202)
summary(m4.full)
step(m4.full, direction="backward", trace=F)

step(m0, scope = ~Senior+Age+Educ+Exper+Sex, direction="forward", trace=F)

# interactions
m2.full = lm(log(Bsal) ~ (Senior+Age+Educ+Exper)^2, data=case1202)
summary(m2.full)
m2.back = step(m2.full, direction="backward", trace=F); summary(m2.back)

m2.for = step(m0, scope = ~(Senior+Age+Educ+Exper)^2, direction="forward", trace=F)
summary(m2.for)

# new data
BreastTissue <- read.csv("C:/Study/UMN files/3022/BreastTissue.csv")
pairs(BreastTissue[, 3:11])
library(copula)
X = pobs(BreastTissue[, 3:11]); colnames(X) = colnames(BreastTissue)[3:11]
pairs(X)

m3.full = lm(P ~ I0+PA500+HFS+DA+Area+A.DA+Max.IP+DR, data=data.frame(X))
summary(m3.full)
# backward selection
step(m3.full, direction="backward", trace=F)

# forward selection.. gives a different set of variables
m30 = lm(P~1, data=data.frame(X))
step(m30, scope=~I0+PA500+HFS+DA+Area+A.DA+Max.IP+DR,
     direction="forward", trace=F)
# if there is a lot of correlation among predictors, forward/backward selection
# might not be reliable

# use BIC as selection criterion
library(Rcmdr)
stepwise(m.full, direction="backward", criterion="BIC", trace=F)
step(m.full, direction="backward", trace=F) # uses AIC

