# CATEGORICAL DATA ANALYSIS: SESSION 1
# Statistical Inference 2026-27
# Three classroom demos using a hypothetical hospital recovery study.
# Base R only: no packages or downloads required.
#
# HOW TO USE
# Explanations are inline comments only; they are not printed to the console.
# Numeric results and plots are retained for the classroom demonstrations.
# <- assigns a value; c() combines values; rbind()/cbind() combine rows/columns.
# set.seed() makes simulations repeatable; B is the number of simulated samples.
# A null model is the benchmark assumption used to generate expected counts.
# source("categorical_session1_demo.R") runs all demos; plots appear separately.
# Run from top to bottom, selecting successive lines or blocks in RStudio.
# All objects stay in your workspace; later sections reuse earlier results.
# Run each for-loop as a complete block.
# In RStudio, use the Plots history to revisit plots.
# All datasets are teaching examples, not clinical evidence.
# Simulations assume independent patients with common probabilities within
# each population/group. They do not model hospital clustering or missingness.

# ============================================================================
# 1. MULTINOMIAL COUNTS AND A SPECIFIED RECOVERY BENCHMARK (10-15 minutes)
# ============================================================================
set.seed(20261028)
n <- 200L
B <- 10000L                       # Number of simulated cohorts
p0 <- c(Full = 0.50, Partial = 0.30, None = 0.20)
observed <- c(Full = 110, Partial = 50, None = 40)

# First create individual patient outcomes, then count categories.
# Fixing factor levels keeps a category even if it happens to have count 0.
patient_outcomes <- factor(
  sample(names(p0), size = n, replace = TRUE, prob = p0),
  levels = names(p0)
)
# These are the recovery labels for just the first 12 of 200 simulated patients. Full,
# Partial and None are categories, not numbers. The Levels line lists all possible
# categories; it is not an extra patient.
print(head(patient_outcomes, 12))
# This table counts how many of the 200 simulated patients fall in each category. The
# three counts must add to 200. They need not equal 100, 60 and 40 because this is one
# random cohort.
print(table(patient_outcomes))
# These are the same counts divided by 200. Read 0.50 as 50% of patients. The proportions
# sum to 1. This simulated cohort is separate from the supplied observed counts used
# later.
print(prop.table(table(patient_outcomes)))
# This random cohort is distinct from the supplied observed cohort below.

# rmultinom() generates counts directly; each COLUMN is one cohort.
counts <- rmultinom(B, size = n, prob = p0)
rownames(counts) <- names(p0)
expected <- n * p0
theoretical_cov <- n * (diag(as.numeric(p0)) - outer(p0, p0))
dimnames(theoretical_cov) <- list(names(p0), names(p0))
empirical_cov <- cov(t(counts))   # cov() treats columns as variables

# We now simulate 10,000 cohorts, each with 200 patients. Theoretical is n times the
# benchmark probability: 100, 60 and 40. Simulated is the average count across cohorts,
# not the count from one cohort. The two columns should be close.
print(round(cbind(Theoretical = expected, Simulated = rowMeans(counts)), 3))
# This matrix describes how category COUNTS vary across repeated cohorts. Diagonal entries
# are variances: n*p*(1-p). Off-diagonal entries are covariances: -n*p_i*p_j. Negative
# covariance occurs because all counts must share a fixed total of 200; it does not mean
# the patients are dependent.
print(theoretical_cov)
# This estimates the same covariance matrix using the 10,000 simulated cohorts. Compare
# each entry with the theoretical matrix above. Small differences are simulation noise.
# t(counts) makes cohorts the rows and categories the columns, as cov() requires.
print(round(empirical_cov, 2))
# colSums(counts) finds the total patient count in each simulated cohort. Every total is
# 200, so the variance of those totals is exactly zero even though the individual category
# counts vary.
print(var(colSums(counts)))
# ASK: Independent patients, but negative count covariances -- why?
# ANSWER: A patient contributes to exactly one category; totals stay at n.

# Calculate Pearson manually, retaining each category's contribution.
contributions <- (observed - expected)^2 / expected
X2 <- sum(contributions)
df <- length(p0) - 1L
p_asym <- pchisq(X2, df = df, lower.tail = FALSE)
# We now use the supplied observed cohort: 110 Full, 50 Partial and 40 None. Expected
# means what the benchmark predicts on average: 100, 60 and 40. Each Pearson contribution
# is (Observed-Expected)^2/Expected. These contributions measure discrepancies and add to
# the test statistic.
print(cbind(Observed = observed, Expected = expected,
            Pearson_contribution = contributions))
# X2 is the total Pearson discrepancy, about 2.667. There are 2 degrees of freedom because
# three counts have one fixed-total constraint and no benchmark probabilities were
# estimated. The p-value, about 0.264, is the null-model probability of a statistic at
# least this large (using a chi-square approximation). At a 5% threshold we do not reject
# the benchmark; this does not establish that it is true.
print(c(X2 = X2, df = df, asymptotic_p = p_asym))
# Verify only AFTER showing the manual calculation.
reference <- chisq.test(observed, p = p0)
# This is the built-in R check of the manual Pearson calculation above. X-squared is X2,
# df is degrees of freedom, and p-value is the upper-tail probability. The numbers should
# match. The following stopifnot() is silent if the check passes and stops the script if
# it fails.
print(reference)
stopifnot(isTRUE(all.equal(X2, unname(reference$statistic))))

# The same fixed null probabilities are used for every simulated cohort.
differences <- sweep(counts, 1, expected, '-')
X2_sim <- colSums(sweep(differences^2, 1, expected, '/'))
# Include ties. The +1 construction gives a valid Monte Carlo test under
# this fully specified null, rather than reporting a spurious zero p-value.
p_mc <- (1 + sum(X2_sim >= X2 - 1e-12)) / (B + 1)
# Asymptotic uses the theoretical chi-square curve. Monte_Carlo estimates the null tail
# probability from simulated cohorts, counting statistics at least as large as the
# observed one. Approx_MC_SE measures simulation uncertainty in that estimate, not
# uncertainty in the recovery probabilities. Neither p-value is the probability that the
# null hypothesis is true.
print(c(Asymptotic = p_asym, Monte_Carlo = p_mc,
        Approx_MC_SE = sqrt(p_mc * (1 - p_mc) / B)))

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1))
# PLOT 1 (left): Blue bars are the observed proportions 0.55, 0.25, 0.20; grey bars are
# the benchmark 0.50, 0.30, 0.20. Compare bar heights within each recovery category.
barplot(rbind(Observed = observed / n, Benchmark = p0), beside = TRUE,
        col = c('steelblue', 'grey75'), ylim = c(0, 0.65),
        ylab = 'Proportion', main = 'Recovery: observed and benchmark',
        legend.text = TRUE, args.legend = list(bty = 'n', cex = 0.8))
# PLOT 2 (right): Grey bars show Pearson statistics from repeated null samples. The red
# curve is the chi-square approximation; the dashed blue line is the observed statistic.
# The null probability to the right of that line is the p-value. The vertical axis is
# density, so area, not bar height, represents probability.
hist(X2_sim, breaks = 45, probability = TRUE, col = 'grey85',
     border = 'white', main = 'Pearson statistic under the null',
     xlab = expression(X^2))
curve(dchisq(x, df = df), add = TRUE, col = 'firebrick', lwd = 2)
abline(v = X2, col = 'steelblue', lwd = 2, lty = 2)
legend('topright', c('Chi-square, df = 2', 'Observed statistic'),
       col = c('firebrick', 'steelblue'), lty = c(1, 2), lwd = 2, bty = 'n')

# ASK: Why 2 df rather than 3? The three proportions sum to 1.
# CONCLUSION: p is about 0.264; failure to reject does not prove equivalence.
par(oldpar)  # Restore graphics settings after this pair of plots.

# ============================================================================
# 2. INDEPENDENCE: ONE NETWORK SAMPLE, TWO CLASSIFICATIONS (15 minutes)
# ============================================================================
observed <- rbind(A = c(Full = 30, Partial = 20, None = 10),
                  B = c(Full = 20, Partial = 30, None = 40))
n <- sum(observed)
nr <- nrow(observed)
nc <- ncol(observed)
# This is ONE sample of 150 patients, classified by site (rows) and recovery (columns).
# The Sum row and column are totals, not extra observations. Site A has 60 patients and
# site B has 90, but these site sizes were not fixed in advance in this sampling design.
print(addmargins(observed))
# DESIGN: Sample 150 patients once; record their site and recovery.
# Site sample sizes were NOT fixed in advance.
# p_ij is a JOINT probability; all six probabilities sum to 1.
empirical_joint <- observed / n
row_prob <- rowSums(observed) / n
col_prob <- colSums(observed) / n
fitted_joint <- outer(row_prob, col_prob)
dimnames(fitted_joint) <- dimnames(observed)
expected <- n * fitted_joint
# Each cell is its observed count divided by the GRAND total, 150. For example, A/Full is
# 30/150 = 0.20: the fraction of all patients who are both at site A and fully recovered.
# All six cells together sum to 1; individual rows do not.
print(round(empirical_joint, 4))
# Under independence, P(site AND recovery) = P(site)*P(recovery). The site proportions are
# 0.4 and 0.6; all three recovery proportions are 1/3. Multiplying these margins gives the
# fitted joint probabilities below. These probabilities are estimated from this sample.
print(round(fitted_joint, 4))
# Expected counts are the grand total times the fitted independent probabilities,
# equivalently row total times column total divided by grand total. Each cell in row A has
# expectation 20; each in row B has expectation 30. These are model averages, not
# additional data.
print(expected)

contributions <- (observed - expected)^2 / expected
X2 <- sum(contributions)
# Avoid 0 * log(0): a zero observed count contributes zero to G2.
positive <- observed > 0
G2 <- 2 * sum(observed[positive] * log(observed[positive] / expected[positive]))
df <- (nr - 1L) * (nc - 1L)
# Each cell shows (Observed-Expected)^2/Expected. Large entries identify cells
# contributing most to the overall disagreement with independence. Squaring removes the
# direction: a shortage and an excess can contribute equally.
print(contributions)
# A Pearson residual is (Observed-Expected)/sqrt(Expected). Positive means more patients
# than independence predicts; negative means fewer. Squaring a residual gives its Pearson
# contribution above. A/Full is positive and A/None is negative; site B shows the reverse
# pattern. These residuals are descriptive and are not separate independent hypothesis
# tests.
print(round((observed - expected) / sqrt(expected), 3))
results <- data.frame(Statistic = c('Pearson', 'Deviance'),
                      Value = c(X2, G2), df = df,
                      p_value = pchisq(c(X2, G2), df, lower.tail = FALSE))
# Pearson uses squared discrepancies; Deviance uses the likelihood-ratio statistic G2 =
# 2*sum(O*log(O/E)). Both are compared with a chi-square distribution with (2-1)*(3-1) = 2
# degrees of freedom. Pearson is about 16.667 with p about 0.00024: strong evidence
# against independence under this model. Association alone does not show that site causes
# recovery differences.
print(results)
reference <- chisq.test(observed, correct = FALSE)
# The built-in chisq.test() repeats the Pearson independence test. Its statistic, df and
# p-value should match the Pearson row above, not the Deviance row. The assertion
# immediately after it checks the statistic and produces no output when successful.
print(reference)
stopifnot(isTRUE(all.equal(X2, unname(reference$statistic))))
# correct = FALSE makes the requested uncorrected calculation explicit.
# A continuity correction is not applied by chisq.test to this 2 x 3 table.

# NULL SIMULATION: one multinomial over all SIX cells, total fixed at 150.
# as.vector() and matrix() both use column-major ordering in R.
draws <- rmultinom(B, size = n, prob = as.vector(fitted_joint))
X2_sim <- numeric(B)
row_totals <- matrix(NA_real_, B, nr, dimnames = list(NULL, rownames(observed)))
col_totals <- matrix(NA_real_, B, nc, dimnames = list(NULL, colnames(observed)))
zero_margin <- logical(B)
for (b in seq_len(B)) {
  tab <- matrix(draws[, b], nrow = nr, ncol = nc)
  rt <- rowSums(tab); ct <- colSums(tab)
  row_totals[b, ] <- rt; col_totals[b, ] <- ct
  # Re-estimate BOTH margins in this simulated dataset.
  E_b <- outer(rt, ct) / n
  # A zero fitted cell from an empty margin also has observed count zero.
  # Use its zero contribution for computation and flag the event. Such
  # boundary tables need care; no such events are expected in this demo.
  zero_margin[b] <- any(E_b == 0)
  keep <- E_b > 0
  X2_sim[b] <- sum((tab[keep] - E_b[keep])^2 / E_b[keep])
}
# We simulated 10,000 new tables under the fitted independence model. Grand_total_variance
# must be zero because every table contains 150 patients. Zero_margin_tables counts tables
# with an empty row or column; expect zero here. This is a diagnostic count, not a
# p-value.
print(c(Grand_total_variance = var(colSums(draws)),
        Zero_margin_tables = sum(zero_margin)))
# These variances measure how much the site sample sizes change across simulated samples.
# Both should be near 150*0.4*0.6 = 36. The site sizes are random, even though their sum
# is always 150.
print(apply(row_totals, 2, var))
# These variances measure how much the recovery-category totals change across samples.
# Each should be near 150*(1/3)*(2/3) = 33.33. Both row and column margins are random in
# this design.
print(apply(col_totals, 2, var))
# A 95% quantile is a cutoff below which about 95% of null statistics fall. Compare the
# simulated cutoffs with the chi-square cutoffs in each column; close values support the
# approximation. These are statistic cutoffs, not p-values. Every simulated table has its
# margins re-estimated before calculating its statistic.
print(rbind(Simulated = quantile(X2_sim, c(.5, .9, .95, .99)),
            Chi_square = qchisq(c(.5, .9, .95, .99), df)))
# This is an unconditional parametric bootstrap from a FITTED null.
# It is not a finite-sample exact test. With only 10000 draws, the observed
# p-value near 0.00024 has few exceedances, so use the simulation to show
# the distribution/design rather than to claim a precise extreme-tail p.
# chisq.test(..., simulate.p.value = TRUE) uses FIXED margins for a table:
# that is a different conditional simulation, not the one implemented here.

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1))
# PLOT 1 (left): Each bar is a within-site recovery proportion, dividing A counts by 60
# and B counts by 90. Compare the blue and orange bars within each outcome. This compares
# recovery patterns fairly despite unequal site sizes.
barplot(prop.table(observed, margin = 1), beside = TRUE,
        col = c('steelblue', 'orange3'), ylim = c(0, .6),
        main = 'Within-site recovery proportions', ylab = 'Proportion',
        legend.text = c('Site A', 'Site B'), args.legend = list(bty = 'n'))
# PLOT 2 (right): Grey bars show statistics under fitted independence; the red curve is
# chi-square with 2 df. The observed statistic (blue dashed line) is far in the right
# tail. The simulation illustrates the null distribution; 10,000 draws are too few to
# estimate this very small tail probability precisely.
hist(X2_sim, breaks = 45, probability = TRUE, col = 'grey85',
     border = 'white', xlim = c(0, max(X2_sim, X2) * 1.05),
     main = 'Independence: fitted-null simulation', xlab = expression(X^2))
curve(dchisq(x, df), add = TRUE, col = 'firebrick', lwd = 2)
abline(v = X2, col = 'steelblue', lty = 2, lwd = 2)
legend('topright', c('Chi-square, df = 2', 'Observed statistic'),
       col = c('firebrick', 'steelblue'), lty = c(1, 2), lwd = 2, bty = 'n')
# ASK: Which cells explain the discrepancy? Does association imply causation?
par(oldpar)  # Restore graphics settings after this pair of plots.

# ============================================================================
# 3. HOMOGENEITY: SEPARATE GROUPS WITH FIXED SIZES (15 minutes)
# ============================================================================
# Continue with the same observed table, n, nr, nc and B from Section 2.
# Change the sampling design: group sizes are now fixed in advance.
group_sizes <- c(A = 60L, B = 90L)
stopifnot(all(rowSums(observed) == group_sizes))
# DESIGN: The numbers 60 and 90 were fixed before collecting outcomes.
# Here p_ij is a WITHIN-GROUP probability: EACH row sums to 1.
group_prob <- prop.table(observed, margin = 1)
pooled_prob <- colSums(observed) / n
expected <- outer(group_sizes, pooled_prob)
dimnames(expected) <- dimnames(observed)
# Now imagine TWO separately sampled groups whose sizes were fixed at 60 and 90. Each cell
# is its count divided by its own GROUP size. Each row therefore sums to 1. For example,
# Full is 30/60 = 0.50 in A and 20/90 = 0.2222 in B. These are within-group probabilities,
# unlike the joint probabilities in Demo 2.
print(round(group_prob, 4))
# The homogeneity null says both groups share one recovery distribution. Estimate that
# distribution by combining their counts and dividing by 150: 50/150 = 1/3 for each
# outcome. This weights group distributions by their sizes, 60/150 and 90/150; it is not
# an equal-weight average of the two rows.
print(pooled_prob)
# Pooling is sample-size weighted, not an unweighted average of rows.
weighted_pool <- colSums(sweep(group_prob, 1, group_sizes / n, '*'))
stopifnot(isTRUE(all.equal(as.numeric(pooled_prob), as.numeric(weighted_pool))))
# Multiply each fixed group size by the pooled recovery probabilities. Expected counts are
# 20 in each cell of A and 30 in each cell of B. These are numerically identical to Demo
# 2, although the sampling design and interpretation of probabilities differ.
print(expected)
X2 <- sum((observed - expected)^2 / expected)
positive <- observed > 0
G2 <- 2 * sum(observed[positive] * log(observed[positive] / expected[positive]))
df <- nr * (nc - 1L) - (nc - 1L)
results <- data.frame(Statistic = c('Pearson', 'Deviance'), Value = c(X2, G2),
                      df = df, p_value = pchisq(c(X2, G2), df, lower.tail = FALSE))
# Pearson and Deviance test whether the two groups share one recovery distribution.
# Separate group distributions require 2*(3-1) = 4 free parameters; one shared
# distribution requires 3-1 = 2. Their difference gives df = 2. The statistics and
# p-values match Demo 2, providing strong evidence against a common distribution under the
# assumed sampling model.
print(results)
# This built-in result checks the Pearson row above. R may label it a test of independence
# because it sees a table, not the sampling design. With fixed group sizes, we interpret
# the same calculation as a test of homogeneity.
print(chisq.test(observed, correct = FALSE))
# Same numbers as Demo 2, although the likelihood and design differ.

# HOMOGENEITY NULL: two independent multinomials with fixed group sizes.
# Each object below has 3 rows (categories) and B columns (replicates).
counts_A <- rmultinom(B, size = group_sizes[1], prob = pooled_prob)
counts_B <- rmultinom(B, size = group_sizes[2], prob = pooled_prob)
hom_X2 <- numeric(B)
hom_rows <- cbind(A = colSums(counts_A), B = colSums(counts_B))
hom_cols <- t(counts_A + counts_B)
colnames(hom_cols) <- colnames(observed)
empty_category <- logical(B)
for (b in seq_len(B)) {
  tab <- rbind(counts_A[, b], counts_B[, b])
  p_b <- colSums(tab) / n            # Refit common distribution each time
  E_b <- outer(group_sizes, p_b)
  empty_category[b] <- any(E_b == 0)
  keep <- E_b > 0
  hom_X2[b] <- sum((tab[keep] - E_b[keep])^2 / E_b[keep])
}

# Compare with the independence simulations already generated in Section 2.
design_check <- rbind(
  Independence = c(Grand = var(rowSums(row_totals)),
                   setNames(apply(row_totals, 2, var), c('Row_A', 'Row_B')),
                   setNames(apply(col_totals, 2, var), c('Col_F', 'Col_P', 'Col_N'))),
  Homogeneity = c(Grand = var(rowSums(hom_rows)),
                  setNames(apply(hom_rows, 2, var), c('Row_A', 'Row_B')),
                  setNames(apply(hom_cols, 2, var), c('Col_F', 'Col_P', 'Col_N')))
)
# Each entry is a variance across repeated samples, not a count from the observed table.
# Grand is zero in both designs. Row_A and Row_B vary under Independence but are exactly
# zero under Homogeneity because group sizes stay 60 and 90. Col_F, Col_P and Col_N vary
# under both designs. This is the key difference between the experiments.
print(round(design_check, 2))
stopifnot(all(hom_rows[, 1] == 60), all(hom_rows[, 2] == 90),
          all(colSums(draws) == 150))
# Compare each column across the two simulated designs and the chi-square reference. The
# 50%, 90%, 95% and 99% entries are null-statistic cutoffs. Similar values show that
# different sampling experiments can have the same approximate chi-square law; they do not
# make the experiments identical.
print(rbind(Independence = quantile(X2_sim, c(.5, .9, .95, .99)),
            Homogeneity = quantile(hom_X2, c(.5, .9, .95, .99)),
            Chi_square = qchisq(c(.5, .9, .95, .99), df)))

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1))
# PLOT 1 (left): The grey histogram shows how the number of A patients varies under
# independence sampling. The blue dashed line marks the fixed value 60 under homogeneity
# sampling. Both experiments have 150 total patients, but only homogeneity fixes the A
# group size.
hist(row_totals[, 1], breaks = seq(min(row_totals[, 1]) - .5,
                               max(row_totals[, 1]) + .5, by = 1),
     col = 'grey80', border = 'white', main = 'Group A size across samples',
     xlab = 'Number of patients in group A')
abline(v = 60, col = 'steelblue', lwd = 3, lty = 2)
legend('topright', c('Independence: random', 'Homogeneity: always 60'),
       col = c('grey60', 'steelblue'), lwd = c(6, 3), lty = c(1, 2),
       bty = 'n', cex = .8)
# ECDFs avoid smoothing artifacts near the lower boundary zero.
# PLOT 2 (right): An ECDF gives the fraction of simulated statistics at or below each
# horizontal-axis value. Blue is independence, orange is homogeneity, and the black dotted
# curve is the chi-square CDF. Close curves show similar null distributions. The display
# ends at 12, so it does not show the entire extreme tail.
plot(ecdf(X2_sim), do.points = FALSE, verticals = TRUE,
     col = 'steelblue', lwd = 2, xlim = c(0, 12),
     main = 'Null distributions of Pearson statistic',
     xlab = expression(X^2), ylab = 'Cumulative probability')
lines(ecdf(hom_X2), do.points = FALSE, verticals = TRUE,
      col = 'orange3', lwd = 2, lty = 2)
curve(pchisq(x, df), add = TRUE, col = 'black', lwd = 2, lty = 3)
legend('bottomright', c('Independence simulation', 'Homogeneity simulation',
                        'Chi-square, df = 2'),
       col = c('steelblue', 'orange3', 'black'), lty = 1:3, lwd = 2,
       bty = 'n', cex = .8)

# Both simulations use estimated null probabilities: neither is claimed
# to be a finite-sample exact unconditional test. Their sampling designs
# differ, but under regularity both fitted statistics converge to chi-square.
# ASK: Can the observed table alone tell you which experiment occurred?
# ANSWER: No. You must read the sampling description.
# ASK: Why does fitting a common distribution cost c-1 parameters here?
# Unrestricted: r(c-1); null: c-1; difference: (r-1)(c-1).
par(oldpar)  # Restore graphics settings after this pair of plots.
