require(alr3)
data = read.table("http://users.stat.umn.edu/~qiuxx008/teaching/stat8051/fall12/exams/spartina.txt", header = T)
attach(data)
pairs(data)
summary(data)

## Scatterplot matrix and transformations
summary(powerTransform(as.matrix(data[,c(2,3,4,5,6)])))

reg1 = lm(Biomass~log(K)+log(Na)+log(pH)+log(Salinity)+Zn+factor(Location)+factor(Type))
ncvTest(reg1)
require(MASS); boxcox(reg1)
invResPlot(reg1)
ncvTest(lm(log(Biomass)~log(K)+log(Na)+log(pH)+log(Salinity)+Zn+factor(Location)+factor(Type)))
ncvTest(lm(sqrt(Biomass)~log(K)+log(Na)+log(pH)+log(Salinity)+Zn+factor(Location)+factor(Type)))

## Residual analysis
reg2 = lm(log(Biomass)~log(K)+log(Na)+log(pH)+log(Salinity)+Zn+factor(Location)+factor(Type))
summary(reg2)
anova(reg2)
plot(reg2, which=1)
residualPlots(reg2, quadratic = F)
mmps(reg2, sd = T)

## Outliers and influential points
outlierTest(reg2)
#qqplot(reg2)
influenceIndexPlot(reg2, id.n=5)
par(mfrow=c(1,1))
influencePlot(reg2, id.n=4)

## Variable selection
reg0 = lm(log(Biomass)~1)
step(reg0, scope=~log(K)+log(Na)+log(pH)+log(Salinity)+Zn+factor(Location)+factor(Type), direction="forward")
step(reg2, scope=~1, direction="backward")

## Final model
regfinal = lm(log(Biomass)~log(K)+log(pH)+Zn+factor(Location)+factor(Type))
summary(regfinal)
anova(regfinal)
pureErrorAnova(regfinal)
