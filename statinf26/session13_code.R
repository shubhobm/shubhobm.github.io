# Categorical data: sequential base R procedures for both sessions.
# Each table uses rows and columns in the order stated in the notes.
# Run blocks in order. No additional R packages are needed.

# SESSION 1: a known recovery benchmark. Null probabilities are fixed.
observed <- c(Full = 110, Partial = 50, None = 40)
benchmark <- c(0.5, 0.3, 0.2)
gof <- chisq.test(observed, p = benchmark)
gof$expected
gof                     # X-squared = 2.6667, df = 2, p = 0.2636

# The likelihood-ratio deviance tests the same specified null.
expected <- sum(observed) * benchmark
positive <- observed > 0
deviance <- 2 * sum(observed[positive] *
                   log(observed[positive] / expected[positive]))
c(G2 = deviance, p_value = pchisq(deviance, 2, lower.tail = FALSE))

# Independence (one cross-classified sample) and homogeneity (separate
# independent groups) share this computation but have different designs.
site_table <- matrix(c(30, 20, 10, 20, 30, 40),
                     nrow = 2, byrow = TRUE,
                     dimnames = list(Site = c("A", "B"),
                                     Recovery = c("Full", "Partial", "None")))
site_test <- chisq.test(site_table, correct = FALSE)
site_test$expected
site_test               # X-squared = 16.6667, df = 2, p about 0.000240

# Fitted milestone model: estimate theta from the SAME category counts.
# chisq.test() does not know that one parameter was estimated. Therefore
# calculate the statistic and use df = 3 - 1 - 1 = 1 explicitly.
n <- sum(observed)
theta_hat <- (2 * observed[1] + observed[2]) / (2 * n)
fitted_prob <- c(theta_hat^2, 2 * theta_hat * (1 - theta_hat),
                 (1 - theta_hat)^2)
fitted_expected <- n * fitted_prob
fitted_X2 <- sum((observed - fitted_expected)^2 / fitted_expected)
fitted_expected        # 91.125, 87.750, 21.125
c(theta = theta_hat, X2 = fitted_X2,
  p_value = pchisq(fitted_X2, df = 1, lower.tail = FALSE))

# SESSION 2: Fisher's exact test for two INDEPENDENT groups.
# Success is the FIRST column. With this orientation, "less" means
# recovery odds under pathway A are lower than under pathway B.
pilot <- matrix(c(1, 9, 8, 2), nrow = 2, byrow = TRUE,
                dimnames = list(Pathway = c("A", "B"),
                                Recovery = c("Yes", "No")))
fisher.test(pilot, alternative = "two.sided")  # p = 0.00547749
# Use this direction only if it was specified before seeing the data.
fisher.test(pilot, alternative = "less")       # p = 0.00273875

# Enumerate the null tables. Given both margins, one cell determines all.
# dhyper(x, m, n, k): x successes in k draws from m successes + n failures.
n_A <- sum(pilot[1, ])
n_B <- sum(pilot[2, ])
total_success <- sum(pilot[, 1])
N <- sum(pilot)
x <- seq.int(max(0, total_success - n_B), min(n_A, total_success))
null_prob <- dhyper(x, m = total_success, n = N - total_success, k = n_A)
observed_a <- pilot[1, 1]
observed_prob <- dhyper(observed_a, total_success, N - total_success, n_A)
data.frame(A_success = x, A_failure = n_A - x,
           B_success = total_success - x,
           B_failure = n_B - total_success + x, null_prob = null_prob)
# Include probability ties; the tolerance guards against roundoff.
p_two <- sum(null_prob[null_prob <= observed_prob * (1 + 1e-7)])
c(lower_tail = sum(null_prob[x <= observed_a]), two_sided = p_two)
# The sample odds ratio is NOT generally fisher.test()$estimate:
# that function reports a conditional MLE of the odds ratio.
sample_OR <- pilot[1, 1] * pilot[2, 2] / (pilot[1, 2] * pilot[2, 1])
sample_OR             # 1/36; also report observed risks 0.1 versus 0.8

# McNemar: rows are BEFORE and columns are AFTER for the SAME patients.
# b = loss of control; c = gain of control. Only b and c enter the test.
paired <- matrix(c(40, 18, 6, 36), nrow = 2, byrow = TRUE,
                 dimnames = list(Before = c("Yes", "No"),
                                 After = c("Yes", "No")))
b <- paired[1, 2]
c_gain <- paired[2, 1]
discordant <- b + c_gain
# mcnemar.test() is asymptotic, even when correct = TRUE.
# If there are no discordances, the exact p-value is 1; skip 0/0 ratios.
if (discordant > 0) {
  mcnemar.test(paired, correct = FALSE)   # X-squared = 6, p = 0.01431
  mcnemar.test(paired, correct = TRUE)    # p = 0.02474
  exact_change <- binom.test(b, discordant, p = 0.5)
  exact_change$p.value                   # 0.02265584
  # Improvement means gains exceed losses, hence a SMALL loss count b.
  # Select this alternative in advance if improvement is the question.
  binom.test(b, discordant, p = 0.5, alternative = "less")$p.value
} else {
  exact_change_p <- 1
  exact_change_p
}
# binom.test()'s interval concerns the probability of LOSS conditional
# on discordance; it is not a CI for the marginal before-after change.
n_paired <- sum(paired)
before_rate <- sum(paired[1, ]) / n_paired
after_rate <- sum(paired[, 1]) / n_paired
change <- (c_gain - b) / n_paired
se_change <- sqrt((discordant / n_paired - change^2) / n_paired)
c(before = before_rate, after = after_rate, change = change)
change + c(-1, 1) * qnorm(0.975) * se_change
# This normal interval is approximate and is not useful with sparse
# discordance or at boundaries. Fisher on 'paired' tests a DIFFERENT null.

# Simpson's paradox: a separate illustrative observational cohort.
# Use the SAME baseline-severity categories for both pathways.
severity <- c("Mild", "Severe")
success_A <- c(81, 192)
total_A <- c(87, 263)
success_B <- c(234, 55)
total_B <- c(270, 80)
risk_A <- success_A / total_A
risk_B <- success_B / total_B
weight_A <- total_A / sum(total_A)
weight_B <- total_B / sum(total_B)
data.frame(severity, risk_A, risk_B, difference = risk_A - risk_B,
           weight_A, weight_B)
# Within BOTH strata A has higher recovery. Overall, A has lower recovery.
pooled_A <- sum(success_A) / sum(total_A)
pooled_B <- sum(success_B) / sum(total_B)
c(A = pooled_A, B = pooled_B, difference = pooled_A - pooled_B)

# Equal weights describe a hypothetical target population with 50% in
# each severity stratum. They are an explicit choice, not uniquely correct.
target_weights <- c(0.5, 0.5)
standard_A <- sum(target_weights * risk_A)
standard_B <- sum(target_weights * risk_B)
c(A = standard_A, B = standard_B, difference = standard_A - standard_B)
# Common weights give A = 0.83054, B = 0.77708. Changing the weights changes
# the target population. Standardization alone does not establish causality.
barplot(rbind(A = risk_A, B = risk_B), beside = TRUE,
        names.arg = severity, ylim = c(0, 1),
        col = c("steelblue", "tan3"), ylab = "Observed recovery proportion",
        legend.text = c("Pathway A", "Pathway B"),
        args.legend = list(x = "bottomleft", bty = "n"))
