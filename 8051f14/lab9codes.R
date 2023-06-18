## Stat 8051 hw 6
library(alr4)
## 12.5
names(Donner)

# Part 1
t = with(Donner, table(y,sex)); t
print(fracs <- c(t[2,1]/sum(t[,1]),t[2,2]/sum(t[,2])))
chisq.test(t,correct=F)

# Part 2
summary(reg <- glm(y~age, family=binomial(),
                   data=Donner))
exp(coef(reg))

# Part 3
residualPlot(reg)

# Part 4
regnew = update(reg, ~.+I(age^2)+sex+status)
summary(regnew)

residualPlot(regnew)
plot(Effect("age", regnew),
     grid=TRUE, rescale.axis=FALSE)

Donner$y1 = ifelse(Donner$y=="died",0,1)
plot(y1~age, Donner)
d = 1:70
lines(predict(reg, data.frame(age=d),
              type="response")~d)

lines(predict(glm(y~age+I(age^2),
                  family=binomial(),
                  data=Donner),
              data.frame(age=d),type="response")~d, lty=3)
points(predict(regnew, Donner ,
               type="response")~Donner$age, pch=19)

## 12.6
names(Challeng)
summary(reg <- 
          glm(cbind(fail, n-fail)~temp,
              family=binomial(),
              data=Challeng))

summary(reg1 <- glm(cbind(fail, n-fail)~temp+pres,
                    family=binomial(),
                    data=Challeng))

temp1 = 20:85
failprop = with(Challeng, fail/n)
p = predict(reg, data.frame(temp=temp1), type="response")
plot(p~temp1, type="l")
points(failprop~Challeng$temp)

predict(reg, data.frame(temp=31), type="response")



## 12.7
summary(m1 <- glm(cbind(surv, m - surv) ~ 
                    class + age + sex,
                  binomial, data=Whitestar))
Anova(m1)

m2 <- update(m1, ~(class + age + sex)^2)
Anova(m2)

m3 <- update(m2, ~ . - age:sex)
plot(allEffects(m3),
     rescale.axis=FALSE, grid=TRUE,
     multiline=TRUE, ci.style="bars")
