###############################################################################
# ONE-SAMPLE TESTS: what breaks when an assumption is violated?
#
# Annotated, base-R classroom simulation. Each experiment starts with a true
# null, then changes one assumption used to calibrate the test. The output is
# an empirical rejection rate across repeated samples. A correctly calibrated
# level-.05 test should reject about 5% of the time (allowing for simulation
# error).
###############################################################################

set.seed(20261007)
alpha <- 0.05

###############################################################################
# Utilities
###############################################################################

# Approximate Monte Carlo standard error of an observed rejection rate.
summarize_rate <- function(rate, R) {
  data.frame(rate = rate,
             MCSE = sqrt(rate * (1 - rate) / R),
             nominal = alpha,
             simulations = R)
}

# Two-sided equal-tail Monte Carlo p-value for a null distribution centered at
# zero. The +1 correction prevents a reported p-value of exactly zero.
two_sided_mc_p <- function(observed, null_statistics) {
  (1 + sum(abs(null_statistics) >= abs(observed))) /
    (length(null_statistics) + 1)
}

###############################################################################
# DEMO 1: one-sample mean tests
###############################################################################
# Scientific null in every row below: H0: E[X] = 0.
#
# Compare:
#   1. Student t test: exact for Normal data; asymptotic under iid sampling
#      with finite variance, but not exact at small n under skewness.
#   2. Sign-flip test for the mean: exact if the null distribution is symmetric
#      about zero. Mean zero alone is NOT enough.
#   3. Null-centered bootstrap t test: approximate; resamples centered data so
#      the bootstrap null has mean zero.
#
# The two populations have the same mean and variance:
#   Normal(0,1), and Exponential(1)-1.
# The second has mean 0 but is strongly right-skewed.

n_mean <- 12
R_mean <- 1000
B_boot <- 399

# All 2^n sign vectors. Enumeration makes the sign-flip p-value exact under
# symmetry (conditional on the observed magnitudes) for this small n.
all_signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), n_mean)))

signflip_p_exact <- function(x) {
  t_obs <- abs(mean(x))
  t_null <- drop(all_signs %*% abs(x)) / length(x)
  mean(abs(t_null) >= t_obs - 1e-12)
}

bootstrap_mean_p <- function(x, B = B_boot) {
  n <- length(x)
  # Impose H0 by centering the empirical distribution at zero.
  x0 <- x - mean(x)
  t_obs <- mean(x) / (sd(x) / sqrt(n))
  
  t_star <- replicate(B, {
    xb <- sample(x0, size = n, replace = TRUE)
    mean(xb) / (sd(xb) / sqrt(n))
  })
  two_sided_mc_p(t_obs, t_star)
}

run_mean_simulation <- function(generator, R = R_mean) {
  rejected <- matrix(FALSE, nrow = R, ncol = 3,
                     dimnames = list(NULL,
                                     c("Student t", "Sign flip", "Null bootstrap t")))
  for (r in seq_len(R)) {
    x <- generator(n_mean)
    rejected[r, "Student t"] <-
      t.test(x, mu = 0)$p.value <= alpha
    rejected[r, "Sign flip"] <-
      signflip_p_exact(x) <= alpha
    rejected[r, "Null bootstrap t"] <-
      bootstrap_mean_p(x) <= alpha
  }
  colMeans(rejected)
}

normal_mean <- function(n) rnorm(n, mean = 0, sd = 1)
skewed_mean_zero <- function(n) rexp(n, rate = 1) - 1

rates_normal <- run_mean_simulation(normal_mean)
rates_skewed <- run_mean_simulation(skewed_mean_zero)

mean_results <- rbind(
  data.frame(scenario = "Normal(0,1)",
             method = names(rates_normal),
             rejection_rate = unname(rates_normal)),
  data.frame(scenario = "Exponential(1)-1",
             method = names(rates_skewed),
             rejection_rate = unname(rates_skewed))
)
mean_results$MCSE <- sqrt(mean_results$rejection_rate *
                            (1 - mean_results$rejection_rate) / R_mean)

cat("\nDEMO 1: H0 is mean = 0; sample size n =", n_mean, "\n")
print(mean_results, row.names = FALSE, digits = 3)

# Unambiguous interpretation:
#   Under Normal(0,1), both t and sign-flip tests should be near .05.
#   Under Exponential(1)-1, the mean is still zero, but symmetry is false.
#   The sign-flip test is no longer calibrated for the mean-zero null.
#   The t test is no longer an exact t test, though it can improve as n grows.
#   The bootstrap is also approximate; it is not guaranteed to repair a small-n
#   or poorly represented tail problem.

###############################################################################
# DEMO 2: the t test's CLT approximation under skewness as n grows
###############################################################################
# Same mean-zero skewed population as above. This isolates sample size: the
# t reference distribution is not exact here, but the studentized CLT predicts
# improving calibration as n increases (finite variance is present).

R_size <- 2000
n_grid <- c(8, 20, 50, 200)
t_rates <- numeric(length(n_grid))

for (j in seq_along(n_grid)) {
  n <- n_grid[j]
  reject <- replicate(R_size, {
    x <- skewed_mean_zero(n)
    t.test(x, mu = 0)$p.value <= alpha
  })
  t_rates[j] <- mean(reject)
}

t_size_results <- data.frame(
  n = n_grid,
  rejection_rate = t_rates,
  MCSE = sqrt(t_rates * (1 - t_rates) / R_size),
  nominal = alpha
)
cat("\nDEMO 2: Student t test under skewness, true mean = 0\n")
print(t_size_results, row.names = FALSE, digits = 3)

###############################################################################
# DEMO 3: exact chi-square test for a variance
###############################################################################
# Null in both cases: H0: Var(X) = 1.
# The chi-square reference law is exact for normal samples. It does NOT follow
# merely from having a variance of one. Compare Normal(0,1) with t(5) scaled
# to variance one. The latter has heavy tails and a much larger fourth moment.

chisq_variance_p <- function(x, sigma0_sq = 1) {
  n <- length(x)
  q <- (n - 1) * var(x) / sigma0_sq
  lower <- pchisq(q, df = n - 1)
  upper <- pchisq(q, df = n - 1, lower.tail = FALSE)
  min(1, 2 * min(lower, upper))  # equal-tail two-sided p-value
}

# Null-centered residual bootstrap for the variance. Rescaling makes the
# empirical null distribution have sample variance sigma0_sq. This is an
# approximate bootstrap test, not an exact distribution-free variance test.
bootstrap_variance_p <- function(x, sigma0_sq = 1, B = B_boot) {
  n <- length(x)
  residuals <- x - mean(x)
  z0 <- sqrt(sigma0_sq) * residuals / sd(x)
  observed <- var(x) - sigma0_sq
  null_statistics <- replicate(B, {
    xb <- sample(z0, size = n, replace = TRUE)
    var(xb) - sigma0_sq
  })
  
  p_lower <- (1 + sum(null_statistics <= observed)) / (B + 1)
  p_upper <- (1 + sum(null_statistics >= observed)) / (B + 1)
  min(1, 2 * min(p_lower, p_upper))
}

n_var <- 20
R_var <- 600
run_variance_simulation <- function(generator) {
  reject_chisq <- reject_boot <- logical(R_var)
  for (r in seq_len(R_var)) {
    x <- generator(n_var)
    reject_chisq[r] <- chisq_variance_p(x) <= alpha
    reject_boot[r] <- bootstrap_variance_p(x) <= alpha
  }
  c("Chi-square exact-normal" = mean(reject_chisq),
    "Null variance bootstrap" = mean(reject_boot))
}

normal_var_rates <- run_variance_simulation(
  function(n) rnorm(n, mean = 0, sd = 1)
)
heavy_tail_var_rates <- run_variance_simulation(
  function(n) rt(n, df = 5) * sqrt(3 / 5)
)

variance_results <- rbind(
  data.frame(scenario = "Normal(0,1)", method = names(normal_var_rates),
             rejection_rate = unname(normal_var_rates)),
  data.frame(scenario = "Scaled t(5), variance 1",
             method = names(heavy_tail_var_rates),
             rejection_rate = unname(heavy_tail_var_rates))
)
variance_results$MCSE <- sqrt(variance_results$rejection_rate *
                                (1 - variance_results$rejection_rate) / R_var)
cat("\nDEMO 3: H0 is variance = 1; sample size n =", n_var, "\n")
print(variance_results, row.names = FALSE, digits = 3)

# Discuss:
#   * Chi-square is exact in the normal row.
#   * Under scaled t(5), the variance null is still true but normality fails.
#   * The bootstrap preserves an estimated residual shape and is approximate;
#     its performance with n=20 is an empirical question, not a guarantee.

###############################################################################
# DEMO 4: exact binomial test versus normal score approximation
###############################################################################
# Null: response probability p = .05, with only n = 20 patients. The null
# expected number of responses is one, so a normal approximation is doubtful.
# The exact binomial test is valid but discrete and can be conservative.

score_proportion_p <- function(k, n, p0) {
  z <- (k / n - p0) / sqrt(p0 * (1 - p0) / n)
  2 * pnorm(-abs(z))
}

n_prop <- 20
p0 <- 0.05
R_prop <- 5000
reject_exact <- reject_score <- logical(R_prop)

for (r in seq_len(R_prop)) {
  k <- rbinom(1, size = n_prop, prob = p0)
  reject_exact[r] <- binom.test(k, n_prop, p = p0)$p.value <= alpha
  reject_score[r] <- score_proportion_p(k, n_prop, p0) <= alpha
}

prop_results <- data.frame(
  method = c("Exact binomial", "Normal score approximation"),
  rejection_rate = c(mean(reject_exact), mean(reject_score)),
  MCSE = c(sqrt(mean(reject_exact) * (1 - mean(reject_exact)) / R_prop),
           sqrt(mean(reject_score) * (1 - mean(reject_score)) / R_prop)),
  nominal = alpha
)
cat("\nDEMO 4: H0 is p = .05; n = 20; expected responses = 1\n")
print(prop_results, row.names = FALSE, digits = 3)

# Discuss:
#   A conservative exact test and a poorly calibrated approximation are not
#   the same issue. Discreteness may reduce the exact test's size; a normal
#   approximation can misstate tail probabilities when expected counts are
#   tiny. Also compare the score statistic with the Wald statistic that plugs
#   in p-hat; the latter can behave especially badly near zero or one.

###############################################################################
# Optional class display: empirical rejection rates against the nominal level
###############################################################################
# Run after discussing each table. This plot summarizes calibration only; it
# does not show power, effect size, or whether two tests target the same null.

barplot(mean_results$rejection_rate,
        names.arg = paste(mean_results$scenario, mean_results$method,
                          sep = "\n"),
        las = 2, cex.names = 0.7,
        ylab = "Empirical rejection rate",
        main = "Mean-zero null: exactness depends on symmetry",
        ylim = c(0, max(0.25, mean_results$rejection_rate)))
abline(h = alpha, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Nominal 5%", col = "red", lwd = 2, lty = 2,
       bty = "n")

###############################################################################
# End-of-demo questions
# 1. Which null is being tested in each row?
# 2. Which assumption generated the reference distribution?
# 3. Is a rejection-rate departure a calibration failure, conservatism, or a
#    test of a different estimand?
# 4. Which procedures become more accurate as n grows, and why?
###############################################################################
