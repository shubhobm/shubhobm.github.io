# ============================================================================
# SUFFICIENT STATISTICS: AN ANNOTATED R SIMULATION
# ============================================================================
#
# THE BIG IDEA
# ------------
# We observe a complete Bernoulli sample
#
#                 X = (X_1, ..., X_n),   X_i = 0 or 1,
#
# where P_theta(X_i=1)=theta.  The unknown parameter is theta.
#
# We will investigate the statistic
#
#                 T(X) = X_1 + ... + X_n,
#
# the total number of successes.  T is sufficient for theta if, after we are
# told T, the remaining uncertainty about the ORDER of the zeros and ones no
# longer depends on theta.
#
# With n=5 and T=2, for example, there are choose(5,2)=10 possible samples:
# 11000, 10100, ..., 00011.  Conditional on T=2, each has probability 1/10,
# whatever the value of theta.  That disappearance of theta is sufficiency.
#
# WHAT THIS FILE DOES
# -------------------
# 1. Simulates Bernoulli samples at theta = 0.20, 0.50, and 0.80.
# 2. Keeps only samples for which T=2.
# 3. Shows that the ten possible orderings occur with probability about 0.10
#    for all three values of theta.
# 4. Contrasts T with the non-sufficient statistic S(X)=X_1.
# 5. Illustrates the two-experimenter randomization argument in the text.
#
# HOW TO RUN IT
# -------------
# Open this file in RStudio and click Source.  The left plot shows sufficiency;
# the right plot shows what failure of sufficiency looks like. Numerical results
# and teaching questions are printed in the Console. No add-on packages are
# required.
# ============================================================================

# Fixing the random-number seed makes the simulated results reproducible.
# Everyone who runs the file obtains the same small Monte Carlo fluctuations.
set.seed(6202)

# ----- 1. Choose the model and simulation settings --------------------------
n <- 5                       # Number of Bernoulli observations in each sample.
t_fixed <- 2                 # We condition on the event T(X)=2.
theta_grid <- c(0.20, 0.50, 0.80) # Three possible values of the unknown theta.
B <- 200000                  # Number of complete samples drawn at each theta.

# B is deliberately large because conditioning discards every simulated sample
# whose sum is not t_fixed.  Reducing B makes the simulation faster but noisier.

# T must be a possible number of successes: 0, 1, ..., n.
stopifnot(t_fixed >= 0, t_fixed <= n)

# ----- 2. Construct the sample space and the fiber A_t ----------------------
#
# The full sample space contains 2^n rows.  For n=5, expand.grid constructs
# all 32 binary vectors from 00000 through 11111.
all_x <- as.matrix(expand.grid(rep(list(0:1), n)))
colnames(all_x) <- paste0("X", seq_len(n))

# The "fiber" A_t is the collection of complete samples that produce the same
# statistic t:
#
#                 A_t = {x : T(x)=t}.
#
# Here we select the rows containing exactly t_fixed ones.  Although every row
# in this collection has the same T, the rows are different complete datasets.
fiber <- all_x[rowSums(all_x) == t_fixed, , drop = FALSE]

# Labels such as "11000" make the complete datasets readable on the plot.
fiber_labels <- apply(fiber, 1, paste0, collapse = "")

# There are m=choose(n,t) ways to choose the t positions containing ones.
# With the default settings, m=choose(5,2)=10.
m <- nrow(fiber)

# This helper converts a matrix row such as c(1,1,0,0,0) to "11000".
state_label <- function(z) apply(z, 1, paste0, collapse = "")

# ----- 3. Estimate the conditional distribution of X given T=t -------------
#
# For one selected theta, this function performs literal conditioning:
#
#   a. Generate B complete samples from the Bernoulli(theta) model.
#   b. Keep only rows whose sum is t_fixed.
#   c. Among the retained rows, calculate the relative frequency of every
#      possible x in A_t.
#
# Those relative frequencies estimate P_theta(X=x | T=t).
conditional_frequencies <- function(theta, B, n, t_fixed, labels) {
  # rbinom generates B*n zeros and ones; matrix arranges them into B samples.
  sim <- matrix(rbinom(B * n, size = 1, prob = theta), ncol = n)
  
  # TRUE marks the rows satisfying the conditioning event T=t_fixed.
  keep <- rowSums(sim) == t_fixed
  
  # Count each retained ordering. "levels=labels" also includes an ordering
  # if its simulated count happens to be zero.
  counts <- table(factor(state_label(sim[keep, , drop = FALSE]),
                         levels = labels))
  
  # Return both the conditional proportions and the number of retained rows.
  list(prob = as.numeric(prop.table(counts)), retained = sum(keep))
}

# Run the preceding experiment separately at every theta in theta_grid.
conditional_results <- lapply(
  theta_grid, conditional_frequencies,
  B = B, n = n, t_fixed = t_fixed, labels = fiber_labels
)
# Pull the estimated probability vectors into one table.  A column corresponds
# to a value of theta; a row corresponds to a complete sample x in A_t.
conditional_prob <- do.call(cbind, lapply(conditional_results, `[[`, "prob"))
colnames(conditional_prob) <- paste0("theta = ", theta_grid)
rownames(conditional_prob) <- fiber_labels

# WHY SHOULD ALL THE COLUMNS BE THE SAME?
#
# For a particular x with t ones, its joint Bernoulli probability is
# theta^t(1-theta)^(n-t).  The probability that T=t is the binomial probability
# choose(n,t) theta^t(1-theta)^(n-t).  Therefore Definition 6.2.1 gives
#
#   P_theta(X=x | T=t)
#       = theta^t (1-theta)^(n-t) /
#         [choose(n,t) theta^t (1-theta)^(n-t)]
#       = 1/choose(n,t),
#
# The theta terms cancel.  The answer 1/choose(n,t) contains no theta.  This is
# precisely the statement that the conditional distribution does not depend on
# theta.  With n=5 and t=2, the exact value is 1/10=0.10.
exact_conditional <- rep(1 / choose(n, t_fixed), m)

# ----- 4. Construct a statistic that is NOT sufficient ----------------------
#
# Let S(X)=X_1, so we retain only the first observation.  Knowing X_1 tells us
# nothing about the independent observation X_2.  Consequently,
#
#               P_theta(X_2=1 | X_1=0) = theta.
#
# Theta remains in the conditional distribution, so S cannot be sufficient.
# The following simulation estimates that conditional probability.
insufficient_estimates <- vapply(theta_grid, function(theta) {
  sim <- matrix(rbinom(B * n, 1, theta), ncol = n)
  # From rows satisfying X_1=0, find the proportion for which X_2=1.
  mean(sim[sim[, 1] == 0, 2])
}, numeric(1))

# ----- 5. Draw the two-panel lecture figure ---------------------------------
#
# The left panel shows that T is sufficient.  The right panel shows what a
# failure of sufficiency looks like.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(5, 4.4, 3.2, 1),
    oma = c(0, 0, 2, 0), las = 1)

cols <- c("#0072B2", "#D55E00", "#009E73")

# PANEL 1 (left): estimated P_theta(X=x | T=2).
# Each colored curve comes from a different theta.  All three should sit near
# the dashed exact value 0.10.  Their small differences are simulation error,
# not parameter information.
matplot(seq_len(m), conditional_prob, type = "b", pch = 19,
        lty = 1, col = cols, xaxt = "n", ylim = c(0, max(conditional_prob) * 1.18),
        xlab = expression(paste("sample point  ", x, " in ", A[t])),
        ylab = expression(hat(P)[theta](X == x~"|"~T == t)),
        main = "Condition on the sufficient statistic")
axis(1, at = seq_len(m), labels = fiber_labels, las = 2, cex.axis = 0.72)
abline(h = 1 / m, lty = 2, lwd = 2, col = "gray30")
legend("topright", legend = c(colnames(conditional_prob), "exact: 1/choose(n,t)"),
       col = c(cols, "gray30"), pch = c(19, 19, 19, NA),
       lty = c(1, 1, 1, 2), bty = "n", cex = 0.76)

# PANEL 2 (right): what failure looks like for S=X_1.
# Unlike Panel 1, this curve changes with theta. Conditioning on S did
# not remove theta, so information about theta remains in the rest of X.
plot(theta_grid, insufficient_estimates, type = "b", pch = 19, lwd = 2,
     col = "#D55E00", xlim = c(0, 1), ylim = c(0, 1),
     xlab = expression(theta),
     ylab = expression(hat(P)[theta](X[2] == 1~"|"~X[1] == 0)),
     main = expression(paste("Counterexample:  S(X)=", X[1])))
abline(a = 0, b = 1, lty = 2, col = "gray30")
legend("topleft", legend = c("simulation", "exact value = theta"),
       col = c("#D55E00", "gray30"), pch = c(19, NA),
       lty = c(1, 2), bty = "n")

mtext("Sufficiency means: after conditioning on T, the parameter disappears",
      outer = TRUE, cex = 1.15, font = 2)
par(old_par)

# ----- 6. Print the numerical version of Panel 1 ----------------------------
#
# Each row is one complete dataset x with exactly t_fixed successes.  If T is
# sufficient, every entry should be close to 1/m and the three columns should
# be nearly equal.
cat("\nSUFFICIENT-STATISTIC SIMULATION\n")
cat("Model: ", n, " iid Bernoulli(theta) observations\n", sep = "")
cat("Conditioning event: T=sum(X_i)=", t_fixed, "\n", sep = "")
cat("The fiber contains choose(n,t)=", m, " sample points.\n\n", sep = "")

print(round(conditional_prob, 4))
cat("\nExact conditional probability of every row: ",
    round(1 / m, 4), "\n", sep = "")
cat("Samples retained at each theta: ",
    paste(vapply(conditional_results, `[[`, numeric(1), "retained"),
          collapse = ", "), "\n", sep = "")

# ----- 7. Reproduce the text's two-experimenter argument --------------------
#
# Experimenter 1 sees the full sample X.  Experimenter 2 is told only T(X)=t.
# Experimenter 2 generates Y by choosing uniformly from A_t.  This rule needs
# t but does NOT need theta.  Y generally differs from X, yet it has the same
# conditional distribution given T.
observed_x <- c(1, 0, 1, 0, 0)
if (length(observed_x) != n || sum(observed_x) != t_fixed) {
  observed_x <- fiber[1, ]
}
# Select one of the m rows in A_t with equal probability 1/m.
reconstructed_y <- fiber[sample(seq_len(m), size = 1), ]

cat("\nRANDOMIZATION EXPERIMENT\n")
cat("Experimenter 1 sees X = ", paste(observed_x, collapse = ""), "\n", sep = "")
cat("Experimenter 2 knows only T = ", t_fixed,
    " and generates Y = ", paste(reconstructed_y, collapse = ""), "\n", sep = "")
cat("Both lie in the same fiber A_t. The reconstruction rule used no theta.\n")

# Now repeat the argument many times.  For each original sample X, randomly
# permuting its entries preserves T and selects uniformly from its fiber.
# Therefore the reconstructed Y should have approximately the same unconditional
# distribution as X.  Total-variation distance is 0 only for identical
# distributions; the empirical value below should be small, but not exactly 0.
B_reconstruct <- 30000
theta_demo <- 0.35
original_samples <- matrix(rbinom(B_reconstruct * n, 1, theta_demo), ncol = n)

# "apply(..., sample)" randomly rearranges the entries within every row.
reconstructed_samples <- t(apply(original_samples, 1, sample))
all_labels <- state_label(all_x)
p_original <- prop.table(table(factor(state_label(original_samples),
                                      levels = all_labels)))
p_reconstructed <- prop.table(table(factor(state_label(reconstructed_samples),
                                           levels = all_labels)))
# For discrete distributions, TV distance is half the sum of absolute
# probability differences across all possible sample points.
tv_distance <- 0.5 * sum(abs(p_original - p_reconstructed))
cat("Across ", B_reconstruct, " repetitions at theta=", theta_demo,
    ",\nempirical total-variation distance between X and reconstructed Y = ",
    round(tv_distance, 4), ".\n", sep = "")

# ----- 8. Suggested pauses for classroom discussion -------------------------
cat("\nQUESTIONS FOR STUDENTS\n")
cat("1. Why are all choose(n,t) sequences equally likely after conditioning?\n")
cat("2. Why do the estimates differ slightly across theta in Panel 1?\n")
cat("3. In Panel 2, what information about theta remains after S=X1 is known?\n")
cat("4. Does sufficiency mean T determines X? Why or why not?\n")
