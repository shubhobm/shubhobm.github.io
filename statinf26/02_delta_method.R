# Session 2 — Sampling Distributions
# 02. DELTA METHOD
#
# If sqrt(n)(Y_n-theta) -> N(0,sigma^2), then
# sqrt(n)[g(Y_n)-g(theta)]
#     -> N(0, sigma^2 [g'(theta)]^2)

set.seed(123)

# ------------------------------------------------------------
# Example 1: g(x) = exp(x)
# X_i ~ N(mu,sigma^2), Y_n = Xbar
# mu=1, sigma=2
# Predicted asymptotic SD = sigma*exp(mu)
# ------------------------------------------------------------

B <- 50000
n <- 100
mu <- 1
sigma <- 2

xbar <- replicate(B, mean(rnorm(n, mu, sigma)))
delta_stat <- sqrt(n)*(exp(xbar)-exp(mu))

hist(delta_stat, breaks=80, probability=TRUE,
     main="Delta method: exp(x)", xlab="Delta statistic")
curve(dnorm(x, 0, sigma*exp(mu)), add=TRUE, lwd=2)

cat("Empirical mean: ",mean(delta_stat),"\n")
cat("Empirical SD:   ",sd(delta_stat),"\n")
cat("Predicted SD:   ",sigma*exp(mu),"\n")

# Convergence with n
par(mfrow=c(2,3))
for (n in c(5,10,30,100,500)) {
  xbar <- replicate(B, mean(rnorm(n,mu,sigma)))
  z <- sqrt(n)*(exp(xbar)-exp(mu))
  hist(z, breaks=60, probability=TRUE,
       main=paste("n =",n), xlab="Delta statistic")
  curve(dnorm(x,0,sigma*exp(mu)), add=TRUE, lwd=2)
}
par(mfrow=c(1,1))

# ------------------------------------------------------------
# Example 2: g(x) = log(x)
# mu=4, sigma=1
# g'(mu)=1/mu
# Predicted asymptotic SD = sigma/mu = 0.25
# ------------------------------------------------------------

B <- 50000
n <- 100
mu <- 4
sigma <- 1

xbar <- replicate(B, mean(rnorm(n,mu,sigma)))
z <- sqrt(n)*(log(xbar)-log(mu))

hist(z, breaks=80, probability=TRUE,
     main="Delta method: log(x)", xlab="Delta statistic")
curve(dnorm(x,0,sigma/mu), add=TRUE, lwd=2)

cat("Empirical mean: ",mean(z),"\n")
cat("Empirical SD:   ",sd(z),"\n")
cat("Predicted SD:   ",sigma/mu,"\n")

# ------------------------------------------------------------
# Taylor approximation:
# g(Xbar) ≈ g(mu) + g'(mu)(Xbar-mu)
# ------------------------------------------------------------

B <- 20000
for (n in c(5,10,30,100,500)) {
  xbar <- replicate(B, mean(rnorm(n,mu,sigma)))
  actual <- log(xbar)
  linear <- log(mu)+(xbar-mu)/mu
  error <- actual-linear
  cat("n =",n," SD of Taylor error =",sd(error),"\n")
}

# Visualize error
n <- 30
xbar <- replicate(B, mean(rnorm(n,mu,sigma)))
error <- log(xbar)-(log(mu)+(xbar-mu)/mu)

hist(error, breaks=60,
     main="Taylor approximation error",
     xlab="log(Xbar) - linear approximation")

# ------------------------------------------------------------
# Example 3: g(x)=x^2
# mu=2, sigma=1
# g'(mu)=4, predicted SD = 4
# ------------------------------------------------------------

B <- 50000
n <- 100
mu <- 2
sigma <- 1

xbar <- replicate(B, mean(rnorm(n,mu,sigma)))
z2 <- sqrt(n)*(xbar^2-mu^2)

hist(z2, breaks=80, probability=TRUE,
     main="Delta method: x^2", xlab="Delta statistic")
curve(dnorm(x,0,2*abs(mu)*sigma), add=TRUE, lwd=2)

cat("Empirical SD: ",sd(z2),"\n")
cat("Predicted SD: ",2*abs(mu)*sigma,"\n")

# Hands-on:
# Try g(x)=sqrt(x), g(x)=1/x, or g(x)=log(x).
# Try smaller n and inspect when the approximation is poor.
# Try g'(theta)=0 and discuss why the first-order delta method
# then gives a degenerate limit.
