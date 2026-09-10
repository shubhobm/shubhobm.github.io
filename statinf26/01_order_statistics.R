# Session 2 — Sampling Distributions
# 01. ORDER STATISTICS
#
# For a continuous population with pdf f and cdf F:
# f_X(j)(x) = n! / [(j-1)!(n-j)!] *
#             f(x) F(x)^(j-1) [1-F(x)]^(n-j)

set.seed(123)

n <- 10
j <- 3
B <- 100000

# B independent samples, each of size n
X <- matrix(rnorm(B * n), nrow = B)
Xj <- apply(X, 1, sort)[j, ]

# Empirical sampling distribution + theoretical density
hist(Xj, breaks=80, probability=TRUE,
     main=expression(paste("Sampling distribution of ", X[(3)])),
     xlab=expression(X[(3)]))

curve(
  factorial(n) / (factorial(j-1) * factorial(n-j)) *
    dnorm(x) * pnorm(x)^(j-1) * (1-pnorm(x))^(n-j),
  add=TRUE, lwd=2
)

# Empirical vs theoretical moments
f_order <- function(x, n, j) {
  factorial(n) / (factorial(j-1) * factorial(n-j)) *
    dnorm(x) * pnorm(x)^(j-1) * (1-pnorm(x))^(n-j)
}

theory_mean <- integrate(function(x) x*f_order(x,n,j), -Inf, Inf)$value
theory_second <- integrate(function(x) x^2*f_order(x,n,j), -Inf, Inf)$value

cat("Empirical mean:   ", mean(Xj), "\n")
cat("Theoretical mean: ", theory_mean, "\n")
cat("Empirical SD:     ", sd(Xj), "\n")
cat("Theoretical SD:   ", sqrt(theory_second-theory_mean^2), "\n")

# See how the distribution changes with j
par(mfrow=c(2,2))
for (j in c(1,3,5,8)) {
  Xj <- apply(X, 1, sort)[j,]
  hist(Xj, breaks=60, probability=TRUE,
       main=paste("j =",j), xlab="Order statistic")
  curve(
    factorial(n) / (factorial(j-1)*factorial(n-j)) *
      dnorm(x)*pnorm(x)^(j-1)*(1-pnorm(x))^(n-j),
    add=TRUE, lwd=2
  )
}
par(mfrow=c(1,1))

# Maximum from Uniform(0,1)
# X_(n) ~ Beta(n,1), f(x)=n*x^(n-1)
n <- 10
U <- matrix(runif(B*n), nrow=B)
Umax <- apply(U,1,max)

hist(Umax, breaks=50, probability=TRUE,
     main="Maximum from Uniform(0,1)", xlab=expression(X[(n)]))
curve(n*x^(n-1), from=0, to=1, add=TRUE, lwd=2)

# Hands-on: change n and j and repeat.
