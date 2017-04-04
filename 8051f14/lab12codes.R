## Stat 8051 week 12
library(faraway)
## Faraway Ch4 Prob 10
summary(UCBAdmissions)

chisq.test(UCBAdmissions[,,6])

## create new data-frame
nums = as.numeric(UCBAdmissions)
Sex = rep(c("M","M","F","F"), 6)
Admitted = rep(c("Y","N"), 12)
Dept = rep(c("A","B","C","D","E","F"), rep(4,6))

# model of dependence
mod10.pois1 = glm(nums~Sex*Dept*Admitted, family=poisson)

require(car)
Anova(mod10.pois1)

mod10.pois2 = glm(nums~Admitted*Sex + Dept*Sex, family=poisson)
deviance(mod10.pois2)

# binomial model
nums1 = matrix(nums, ncol=2, byrow=T)
Sex1 = rep(c("M","F"), 6)
Dept1 = rep(c("A","B","C","D","E","F"), rep(2,6))

mod10.bin = glm(nums1~Sex1, family=binomial)
summary(mod10.bin)

mod10.bin1 = glm(nums1~Sex1*Dept1, family=binomial)
summary(mod10.bin1)

require(car)
Anova(mod10.bin1)
