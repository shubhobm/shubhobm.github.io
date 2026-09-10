# Session 2 — Sampling Distributions
# 03. BOX-MULLER ALGORITHM
#
# Generate U1,U2 iid Uniform(0,1)
# R = sqrt(-2 log U1)
# theta = 2*pi*U2
# X = R cos(theta), Y = R sin(theta)
#
# Then X and Y are independent N(0,1).

set.seed(123)

B <- 100000

U1 <- runif(B)
U2 <- runif(B)

R <- sqrt(-2*log(U1))
theta <- 2*pi*U2

X_box <- R*cos(theta)
Y_box <- R*sin(theta)

# ------------------------------------------------------------
# 1. Basic moment checks
# ------------------------------------------------------------

cat("Box-Muller X: mean =",mean(X_box)," SD =",sd(X_box),"\n")
cat("Box-Muller Y: mean =",mean(Y_box)," SD =",sd(Y_box),"\n")

# ------------------------------------------------------------
# 2. Compare with theoretical N(0,1)
# ------------------------------------------------------------

par(mfrow=c(1,2))

hist(X_box, breaks=60, probability=TRUE,
     main="Box-Muller X", xlab="X")
curve(dnorm(x), add=TRUE, lwd=2)

hist(Y_box, breaks=60, probability=TRUE,
     main="Box-Muller Y", xlab="Y")
curve(dnorm(x), add=TRUE, lwd=2)

par(mfrow=c(1,1))

# ------------------------------------------------------------
# 3. Generate normals using rnorm()
# ------------------------------------------------------------

X_rnorm <- rnorm(B)

hist(X_rnorm, breaks=60, probability=TRUE,
     main="rnorm()", xlab="X")
curve(dnorm(x), add=TRUE, lwd=2)

# ------------------------------------------------------------
# 4. Q-Q comparisons
# ------------------------------------------------------------

par(mfrow=c(1,2))

qqnorm(X_box, main="Box-Muller vs N(0,1)")
qqline(X_box, lwd=2)

qqplot(X_rnorm, X_box,
       main="Box-Muller vs rnorm()",
       xlab="rnorm() quantiles",
       ylab="Box-Muller quantiles")
abline(0,1,lwd=2)

par(mfrow=c(1,1))

# ------------------------------------------------------------
# 5. Compare moments and quantiles
# ------------------------------------------------------------

cat("\nMoment comparison\n")
print(c(mean=mean(X_box), SD=sd(X_box), variance=var(X_box)))
print(c(mean=mean(X_rnorm), SD=sd(X_rnorm), variance=var(X_rnorm)))
print(c(mean=0, SD=1, variance=1))

probs <- c(.001,.01,.05,.25,.50,.75,.95,.99,.999)

comparison <- rbind(
  Box_Muller=quantile(X_box,probs),
  rnorm=quantile(X_rnorm,probs),
  theoretical=qnorm(probs)
)

print(comparison)

# ------------------------------------------------------------
# 6. Compare statistically
#
# With B=100,000, formal tests can detect tiny numerical
# differences. Use them as demonstrations, not as proof.
# ------------------------------------------------------------

cat("\nOne-sample tests for Box-Muller X\n")
print(t.test(X_box, mu=0))

# KS test against N(0,1)
print(ks.test(X_box, "pnorm"))

# Direct two-sample comparison
cat("\nTwo-sample KS comparison: Box-Muller vs rnorm()\n")
print(ks.test(X_box, X_rnorm))

# ------------------------------------------------------------
# 7. Independence of X and Y
# ------------------------------------------------------------

cat("\nCorrelation X,Y:",cor(X_box,Y_box),"\n")

plot(X_box,Y_box,
     pch=16, cex=.3,
     main="Box-Muller: X versus Y",
     xlab="X", ylab="Y")

# ------------------------------------------------------------
# 8. Visualize the transformation
# ------------------------------------------------------------

par(mfrow=c(1,2))

plot(U1,U2,pch=16,cex=.3,
     main="Original uniforms",
     xlab="U1", ylab="U2")

plot(X_box,Y_box,pch=16,cex=.3,
     main="After Box-Muller",
     xlab="X", ylab="Y")

par(mfrow=c(1,1))

# Hands-on:
# 1. Change B.
# 2. Change the random seed.
# 3. Compare selected quantiles.
# 4. Explain the circular shape of (X,Y).
# 5. Explain why two uniforms produce two normals.
