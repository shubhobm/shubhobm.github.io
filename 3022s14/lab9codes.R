head(InsectSprays)
## The data contains effect of 6 different insect sprays.
## count is the count of insects killed by the spray
boxplot(count~spray, data=InsectSprays)
## clearly sprays C, D, E have very different effects than other 3

## doing an ANOVA reveals significant effect of spray
mod = lm(count~spray, data=InsectSprays); summary(mod)
anova(mod)

require(plyr)
InsectSprays$spray1 = revalue(InsectSprays$spray,
                              c("A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6))
View(InsectSprays)
## But doing a linear regression, considering sprays 1 to 6 as quantitative variables
## reveals no effects of sprays
mod2 = mod = lm(count~as.numeric(spray1), data=InsectSprays); summary(mod2)

