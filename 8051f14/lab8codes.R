## Stat 8051 hw 5
library(alr4)
## 11.3
names(walleye)

Linf = max(walleye$length)+1
walleye$var = log(1-walleye$length/Linf)
lmod.coef = coef(lm(var~age, walleye))
K = -lmod.coef[2]; t0 = -lmod.coef[1]/lmod.coef[2]

# no period effect
nlmod1 = nls(length~Linf*(1-exp(-K*(age-t0))),
             start=list(Linf=Linf, K=K, t0=t0), data=walleye)

# most specific model
nlmod2 = nls(length ~ (period==1)*Linf1*(1-exp(-K1*(age-t01)))+
               (period==2)*Linf2*(1-exp(-K2*(age-t02)))+
               (period==3)*Linf3*(1-exp(-K3*(age-t03))),
             start=list(Linf1=Linf, Linf2=Linf, Linf3=Linf,
                        K1=K, K2=K, K3=K,
                        t01=t0, t02=t0, t03=t0), data=walleye)

# same Linf
nlmod3 = nls(length~(period==1)*Linf*(1-exp(-K1*(age-t01)))+
               (period==2)*Linf*(1-exp(-K2*(age-t02)))+
               (period==3)*Linf*(1-exp(-K3*(age-t03))),
             start=list(Linf=Linf,
                        K1=K, K2=K, K3=K,
                        t01=t0, t02=t0, t03=t0), data=walleye)

# same K
nlmod4 = nls(length~(period==1)*Linf1*(1-exp(-K*(age-t01)))+
               (period==2)*Linf2*(1-exp(-K*(age-t02)))+
               (period==3)*Linf3*(1-exp(-K*(age-t03))),
             start=list(Linf1=Linf, Linf2=Linf, Linf3=Linf,
                        K=K,
                        t01=t0, t02=t0, t03=t0), data=walleye)

# same t0
nlmod5 = nls(length~(period==1)*Linf1*(1-exp(-K1*(age-t0)))+
               (period==2)*Linf2*(1-exp(-K2*(age-t0)))+
               (period==3)*Linf3*(1-exp(-K3*(age-t0))),
             start=list(Linf1=Linf, Linf2=Linf, Linf3=Linf,
                        K1=K, K2=K, K3=K,
                        t0=t0), data=walleye)

# compare nested models
anova(nlmod1, nlmod3, nlmod2)
anova(nlmod1, nlmod4, nlmod2)
anova(nlmod1, nlmod5, nlmod2)

## Problem 2
library(MASS)
head(Boston)

## full model
lmod = lm(medv~., Boston)

## all-subsets based on AIC
n = nrow(Boston); p = ncol(Boston)-1
require(leaps)
subsetObj = summary(regsubsets(medv~., Boston, nvmax=p))
k = 1:p
bicvals = subsetObj$bic
aicvals = bicvals - k*log(n) + 2*k
which.min(bicvals)
which.min(aicvals) # both are same model

# plot them
plot(aicvals, type="l", xlab="no. of variables", ylab="value")
lines(bicvals, type="l", col="red")
legend("topright",c("AIC","BIC"), col=c("black","red"), lty=1)

# list of variable indices in best model
best.ind = which(subsetObj$which[which.min(bicvals),-1])
BostonBest = Boston[,c(best.ind,14)]

lmod.as = update(lmod, data=BostonBest)
anova(lmod.as, lmod) # compare with larger model

## LASSO
require(glmnet)
X = as.matrix(Boston[,-14])
y = as.matrix(Boston[,14])
cv.lasso = cv.glmnet(X, y, alpha=1)
lmod.lasso = glmnet(X, y, alpha=1)
coef(lmod.lasso, s=cv.lasso$lambda.min)

## Ridge
cv.ridge = cv.glmnet(X, y, alpha=0)
lmod.ridge = glmnet(X, y, alpha=0)
coef(lmod.ridge, s=cv.ridge$lambda.min)

## Comparing model performances
geterr = function(data, model, nrep){
  n = nrow(data); p = ncol(data)
  mspe.vec = rep(0, nrep)
  
  for(i in 1:nrep){
    train = sample(1:n, floor(n/2), replace=F)
    halfmodel = update(model, data=data[train,])
    preds = predict(halfmodel, newdata=data[-train,])
    err = data[-train,p] - preds
    mspe.vec[i] = mean(err^2)
  }
  mean(mspe.vec)
}

geterr.glmnet = function(data, alpha, nrep){
  n = nrow(data); p = ncol(data)
  mspe.vec = rep(0, nrep)
  
  for(i in 1:nrep){
    train = sample(1:n, floor(n/2), replace=F)
    halfmodel = cv.glmnet(x=data[train,-p], y=data[train,p], alpha=alpha)
    preds = predict(halfmodel, newx=data[-train,-p], s="lambda.min")
    err = data[-train,p] - preds
    mspe.vec[i] = mean(err^2)
  }
  mean(mspe.vec)
}

set.seed(10222014)
nrep=100
outmat = matrix(c("Linear","All subsets linear","LASSO","Ridge",
         geterr(Boston, lmod, nrep=nrep),
         geterr(Boston, lmod.as, nrep=nrep),
         geterr.glmnet(as.matrix(Boston), alpha=1, nrep=nrep),
         geterr.glmnet(as.matrix(Boston), alpha=0, nrep=nrep)),
       ncol=2, byrow=F)
outmat[,2] = format(as.numeric(outmat[,2]), digits=4)
outmat
