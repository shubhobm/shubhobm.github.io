require(Sleuth2) # can also use Sleuth3
data(ex0221)
names(ex0221) ## Gives variable names

## 2 ways of doing boxplot
boxplot(ex0221$Weight ~ ex0221$Status)
boxplot(Weight~Status, data = ex0221)

attach(ex0221)
boxplot(Weight~Status)
detach(ex0221)

## Other plots
dead = ex0221$Weight[which(ex0221$Status=='perished')]
alive = ex0221$Weight[which(ex0221$Status=='survived')]

par(mfrow=c(1,2))
# histogram
hist(dead); hist(alive)
# density plot
plot(density(dead)); plot(density(alive))

# awesome plots!
par(mfrow=c(1,1))
plot(density(alive))
plot(density(alive), type='p')
plot(density(alive), lwd=2)
plot(density(alive), lwd=2, lty = 2)
plot(density(alive), lwd=2, col='blue', xlim = c(21,33), ylim=c(0,.4))

plot(density(alive), lwd=2, col='blue', xlim = c(21,33), ylim=c(0,.4),
     main='Density plots of \n perished and survived patients', xlab='Weight',
     ylab = 'Probability')
lines(density(dead), lwd=2, col='red')
legend("topright", c("Survived","Perished"), lwd=2, col = c("Blue", "red"))


## 2 ways of doing t-test

t.test(dead,alive)
t.test(dead,alive, var.equal=T)
t.test(dead,alive, var.equal=T, conf.level = 0.99)
t.test(dead,alive, var.equal=T, mu = 1)
t.test(dead,alive, var.equal=T, mu = 1, alternative='two.sided')

t.test(Weight ~ Status, data = ex0221)
t.test(Weight ~ Status, data = ex0221, var.equal=T)
t.test(ex0221$Weight ~ ex0221$Status)

attach(ex0221)
t.test(Weight~Status)
detach(ex0221)