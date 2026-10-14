# ANOVA CLASS DEMO: ONE-WAY AND TWO-WAY ANOVA
# Statistical Inference | Suggested duration: 30-40 minutes
# Run section by section in RStudio (Ctrl+Enter), pausing at the questions.
# Uses only base R; no installation or internet connection is needed.
# To run everything: source("anova_class_demo.R", echo = TRUE)
# Plots appear in the Plots pane; use its back arrow to revisit earlier plots.
#
# Learning goals:
# 1. Build a one-way ANOVA F statistic from sums of squares.
# 2. Interpret and test a two-factor interaction.
# 3. Distinguish uncertainty in cell means from evidence of interaction.
#
# Sources: ?ToothGrowth, ?anova.lm, ?plot, ?arrows
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.lm.html

section <- function(title) cat("\n\n", title, "\n", strrep("=", 70), "\n", sep = "")

# 0. MEET THE DATA -----------------------------------------------------------
section("0. Vitamin C and tooth growth")
data("ToothGrowth", package = "datasets")
dat <- ToothGrowth
# 60 guinea pigs; response: length of odontoblasts (tooth-growth cells).
# supp: OJ = orange juice; VC = ascorbic acid (Vitamin C).
# dose: 0.5, 1, or 2 mg/day. There are 10 animals per combination.
# Each row is a different animal, NOT a repeated measurement.
# Treat dose as a factor: three unrestricted means, not a straight-line trend.
dat$dose <- factor(dat$dose, levels = c(0.5, 1, 2))
dat$supp <- factor(dat$supp, levels = c("OJ", "VC"))
print(head(dat))
print(with(dat, table(dose, supp)))
print(aggregate(len ~ dose + supp, data = dat, FUN = mean))
# ASK: How would lm(len ~ dose) change if dose were left numeric?
# ANSWER: A numeric dose uses one slope (1 df); a factor uses 2 df.

# 1. ONE-WAY ANOVA -----------------------------------------------------------
section("1. Does mean tooth growth vary by dose within VC?")
# Hold supplement fixed so this is a clean one-factor experiment.
# Pooling both supplements here would hide the second experimental factor.
vc <- subset(dat, supp == "VC")
boxplot(len ~ dose, data = vc, col = "lightblue",
        xlab = "Vitamin C dose (mg/day)", ylab = "Odontoblast length",
        main = "One-way ANOVA: VC animals only")
stripchart(len ~ dose, data = vc, vertical = TRUE, method = "stack",
           add = TRUE, pch = 16, col = "steelblue4")

# Model: Y_ij = mu_i + epsilon_ij, epsilon_ij iid N(0, sigma^2).
# H0: mu_0.5 = mu_1 = mu_2. H1: at least one mean differs.
one <- lm(len ~ dose, data = vc)
print(anova(one))

# Construct the table by hand to connect output with the mathematics.
N <- nrow(vc)
k <- nlevels(vc$dose)
means <- with(vc, tapply(len, dose, mean))
counts <- with(vc, table(dose))
grand_mean <- mean(vc$len)
SS_between <- sum(as.numeric(counts) * (means - grand_mean)^2)
SS_within <- sum(residuals(one)^2)
SS_total <- sum((vc$len - grand_mean)^2)
MS_between <- SS_between / (k - 1)
MS_within <- SS_within / (N - k)
F_value <- MS_between / MS_within
p_value <- pf(F_value, df1 = k - 1, df2 = N - k, lower.tail = FALSE)
print(data.frame(Source = c("Dose", "Residual", "Total"),
                 SS = c(SS_between, SS_within, SS_total),
                 df = c(k - 1, N - k, N - 1)))
print(c(F = F_value, p = p_value, pooled_variance = MS_within))
stopifnot(isTRUE(all.equal(SS_total, SS_between + SS_within)),
          isTRUE(all.equal(unname(F_value), anova(one)[1, "F value"])))
# Under H0, F ~ F_(2,27). Rejection alone does not identify which means differ.
# ASK: Why do we divide by df before taking the ratio?
# ANSWER: Under H0, both mean squares estimate the same error variance.

# Optional bridge from the previous class: a single pre-specified contrast.
# Compare 2 mg/day against 0.5 mg/day: coefficients (-1, 0, 1), sum = 0.
a <- c(-1, 0, 1)
L_hat <- sum(a * means)
SE_L <- sqrt(MS_within * sum(a^2 / as.numeric(counts)))
t_value <- L_hat / SE_L
CI <- L_hat + c(-1, 1) * qt(0.975, N - k) * SE_L
print(c(contrast = L_hat, SE = SE_L, t = t_value,
        p = 2 * pt(-abs(t_value), df = N - k),
        CI_low = CI[1], CI_high = CI[2]))
# Under H0: L = 0, L_hat / SE_L ~ t_(N-k).
# L_hat / sqrt(MS_within) is a SCALED t, with multiplier sqrt(sum(a_i^2/n_i)).
# This is a single planned contrast, not simultaneous inference for all pairs.

# 2. TWO-WAY ANOVA AND INTERACTION -------------------------------------------
section("2. Does the supplement difference depend on dose?")
# Now use all 60 observations: a balanced 3 x 2 factorial design.
# Additive model: E(Y_ijr) = mu + alpha_i + beta_j.
# Interaction model: E(Y_ijr) = mu + alpha_i + beta_j + (alpha beta)_ij.
# R syntax: dose * supp expands to dose + supp + dose:supp.
additive <- lm(len ~ dose + supp, data = dat)
full <- lm(len ~ dose * supp, data = dat)

# Compute the six cell means and their model-based standard errors.
# Under the common-variance ANOVA model:
# SE(mean_ij) = sqrt(MSE_full / n_ij), with MSE_full = RSS_full / 54.
# Pooling uses all six cells to estimate the common error variance.
# Since every cell has n = 10, all six SE bars have the same width.
cell_means <- with(dat, tapply(len, list(dose = dose, supp = supp), mean))
cell_n <- with(dat, table(dose, supp))
MSE_full <- deviance(full) / df.residual(full)
cell_se <- sqrt(MSE_full / cell_n)
print(cell_se)

# Draw the interaction plot explicitly so we can add standard error bars.
# Small horizontal offsets keep the two bars at dose 2 from obscuring each
# other. Tick labels show the true factor levels; offsets have no data meaning.
x <- seq_len(nlevels(dat$dose))
offsets <- c(-0.04, 0.04)
cols <- c("darkorange3", "steelblue4")
symbols <- c(16, 17)
plot(range(x) + c(-0.2, 0.2),
     range(cell_means - cell_se, cell_means + cell_se) + c(-1, 1),
     type = "n", xaxt = "n",
     xlab = "Vitamin C dose (mg/day)", ylab = "Mean odontoblast length",
     main = "Interaction: is the OJ - VC gap constant?",
     sub = "Bars: mean +/- 1 pooled SE (not 95% confidence intervals)")
axis(1, at = x, labels = levels(dat$dose))
for (j in seq_along(levels(dat$supp))) {
  xpos <- x + offsets[j]
  lines(xpos, cell_means[, j], type = "b", pch = symbols[j],
        col = cols[j], lwd = 2)
  arrows(x0 = xpos, y0 = cell_means[, j] - cell_se[, j],
         x1 = xpos, y1 = cell_means[, j] + cell_se[, j],
         angle = 90, code = 3, length = 0.06, col = cols[j], lwd = 1.5)
}
legend("topleft", legend = levels(dat$supp), title = "Supplement",
       col = cols, pch = symbols, lty = 1, lwd = 2, bty = "n")
# These bars describe uncertainty in each MEAN, not the spread of individual
# observations. A 95% pointwise CI would use qt(0.975, 54) * SE instead.
# Overlap/non-overlap of these bars is NOT a formal test of interaction.
# Interaction concerns differences between gaps; use the F test below.
# ASK BEFORE TESTING: Would parallel lines support an additive mean structure?
# ANSWER: Yes. Nonparallel sample lines suggest interaction, but sampling noise
# also makes lines nonparallel. Crossing is NOT required for interaction.
# Connecting lines guide the eye; dose is still a categorical predictor.
cell_means <- with(dat, tapply(len, list(dose = dose, supp = supp), mean))
print(cell_means)
print(data.frame(dose = rownames(cell_means),
                 OJ_minus_VC = cell_means[, "OJ"] - cell_means[, "VC"]))
# Expected gaps: 5.25, 5.93, -0.08. The OJ advantage changes with dose.

# H0 for interaction: the population OJ - VC gap is identical at all doses.
# Equivalently, both independent difference-in-differences are zero.
# Additive model has 4 parameters; full model has 6 cell-mean parameters.
# F = [(RSS_add - RSS_full)/2] / [RSS_full/54], under H0: F_(2,54).
print(anova(additive, full))
print(anova(full))
# Expected interaction p-value approximately 0.022.
# The interaction row in the full table is the same nested-model test.
# With interaction, explain supplement differences AT EACH DOSE; a single
# average supplement effect does not describe the full pattern.
# Do not infer equality at 2 mg/day merely because its sample gap is near zero.

# Optional: pointwise confidence intervals for the three simple effects,
# using the full model's common error variance. These are NOT simultaneous CIs.
cell_n <- with(dat, table(dose, supp))
MSE_full <- deviance(full) / df.residual(full)
gap <- cell_means[, "OJ"] - cell_means[, "VC"]
gap_se <- sqrt(MSE_full * (1 / cell_n[, "OJ"] + 1 / cell_n[, "VC"]))
critical <- qt(0.975, df.residual(full))
print(data.frame(dose = rownames(cell_means), gap = gap, SE = gap_se,
                 lower = gap - critical * gap_se,
                 upper = gap + critical * gap_se))

# 3. ASSUMPTIONS AND CLASS WRAP-UP -------------------------------------------
section("3. Assumptions and class wrap-up")
# The exact F tests assume independent normal errors with a common variance.
# Independence comes from the experimental design.
# We will discuss how to assess model assumptions in a later class.
## EXIT QUESTIONS (with instructor answers):
# 1. How many residual df in the full 3 x 2 model? 60 - 6 = 54.
# 2. Must lines cross for interaction? No; nonconstant gaps are the key.
# 3. Are the plotted bars 95% confidence intervals? No, they are +/- 1 SE.
# 4. Does bar overlap test interaction? No; compare additive and full models.
# 5. Is the additive model adequate here? The interaction test provides
#    evidence against it; discuss supplement differences separately by dose.
# 6. Why are all SE bars equal? Equal cell sizes and a pooled variance estimate.
#
# Instructor note: leave Type I/II sums of squares and unbalanced designs
# for a later experimental-design class. Here the balanced factorial design
# permits a clean introduction to main effects and interaction.
cat("\nDemo complete. Revisit the interaction plot and dose-specific gaps.\n")

