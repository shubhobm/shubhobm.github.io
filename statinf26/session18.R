# Statistical Inference 2026-27 | Regularized regression | 15-minute demo
# Run blocks sequentially. Flat script: no user-defined fitting functions.
# Uses glmnet >= 5.2 (current control API). Install/update if necessary:
# install.packages("glmnet", repos = "https://cloud.r-project.org")
# Reference: https://glmnet.stanford.edu/articles/glmnet.html
#
# Roadmap: A setup (0-2 min); B ridge (2-5); C lasso (5-8);
# D dimension (8-11); E correlation (11-14); F interpretation (14-15).
# The shared D-E simulation loop is preparation, not a line-by-line lecture.
#
# CONVENTION: pure loss = RSS/(2*n), not RSS or MSE.
# Ridge objective = pure loss + lambda * ||beta||_2^2 / 2.
# Lasso objective = pure loss + lambda * ||beta||_1.
# The intercept is excluded from all norms and penalties.
# "penalty_base" = ||beta||_2^2/2 (ridge) or ||beta||_1 (lasso).
# "penalty_term" = lambda * penalty_base.
#
# Plots appear on screen by default; set save_plots <- TRUE for a 7-page PDF.
# CSV tables are written under regularized_glmnet_output/ in the working folder.

library(glmnet)
stopifnot(packageVersion("glmnet") >= "5.2")
options(width = 125, digits = 4)
dir.create("regularized_glmnet_output", showWarnings = FALSE)
save_plots <- FALSE
if (save_plots) pdf("regularized_glmnet_output/regularization_demo.pdf",
                    width = 11, height = 8.5, onefile = TRUE)
old_par <- par(no.readonly = TRUE)

# A. SYNTHETIC DATA: n=100, p=5, independent features (0-2 min) ------------
set.seed(20261118)
n <- 100
n_test <- 2000
p_max <- 120
beta_signal <- c(2, -1.5, 1)   # only the first three features carry signal
p <- 5
rho <- 0
Sigma <- (1 - rho) * diag(p) + rho * matrix(1, p, p)

# Draw once and reuse: adding features preserves existing columns, y and noise.
# Test observations are independent of training observations.
E <- matrix(rnorm(n * p_max), n, p_max)
E_test <- matrix(rnorm(n_test * p_max), n_test, p_max)
noise <- rnorm(n)
noise_test <- rnorm(n_test)
# Each n-by-p standard-normal matrix is multiplied by a covariance factor.
# R's chol(Sigma) returns R with t(R) %*% R = Sigma, so E %*% R has
# row covariance Sigma. Use the factor on the RIGHT, without transposing it.
X_raw <- E[, 1:p] %*% chol(Sigma)
X_test_raw <- E_test[, 1:p] %*% chol(Sigma)

# Training signal variance is fixed at 3, with model noise variance 1.
# Repeating this normalization later keeps signal strength comparable as
# correlation increases. It rescales the true slopes without changing sparsity.
signal_raw <- drop(X_raw[, 1:3] %*% beta_signal)
signal_center <- mean(signal_raw)
signal_scale <- sqrt(mean((signal_raw - signal_center)^2)) / sqrt(3)
y_raw <- (signal_raw - signal_center) / signal_scale + noise
y_test_raw <- (drop(X_test_raw[, 1:3] %*% beta_signal) - signal_center) /
  signal_scale + noise_test

# Center and scale using TRAINING quantities and divisor n, not n-1.
# Unit RMS for y also makes the ridge formula agree with glmnet's Gaussian
# response scaling. The test sample never enters these transformations.
x_center <- colMeans(X_raw)
X <- sweep(X_raw, 2, x_center, "-")
x_scale <- sqrt(colMeans(X^2))
X <- sweep(X, 2, x_scale, "/")
X_test <- sweep(sweep(X_test_raw, 2, x_center, "-"), 2, x_scale, "/")
y_center <- mean(y_raw)
y_scale <- sqrt(mean((y_raw - y_center)^2))
y <- (y_raw - y_center) / y_scale
y_test <- (y_test_raw - y_center) / y_scale
colnames(X) <- colnames(X_test) <- paste0("x", 1:p)

# Centering makes the fitted intercept zero. standardize=FALSE below prevents
# a second X scaling; intercept=FALSE is intentional for these centered data.
ols <- lm.fit(x = X, y = y)
ols_loss <- mean(ols$residuals^2) / 2
ols_test_loss <- mean((y_test - drop(X_test %*% ols$coefficients))^2) / 2
print(c(n = n, p = p, OLS_loss = ols_loss, OLS_test_loss = ols_test_loss))

# Same descending lambda grid in every comparison. Equal numerical lambdas
# do not mean equal amounts of regularization for ridge and lasso.
lambda_grid <- 10^seq(2, -4, by = -0.1)
lambda_show <- c(100, 1, 0.1, 0.01, 0.0001)
show_rows <- match(round(log10(lambda_show), 8), round(log10(lambda_grid), 8))
old_control <- glmnet.control()
glmnet.control(fdev = 0, devmax = 1)  # retain the full requested path

# B. RIDGE: BASE IMPLEMENTATION, THEN THE PATH (2-5 min) -------------------
# Gradient=0 gives (X'X/n + lambda*I) beta = X'y/n.
# This one matrix calculation connects the lecture derivation to the package.
# solve() solves a linear system rather than explicitly forming an inverse.
lambda0 <- 0.1
beta_ridge_formula <- drop(solve(crossprod(X) / n + lambda0 * diag(p),
                                  crossprod(X, y) / n))

# Basic glmnet implementation: alpha=0 selects ridge. Fit all lambdas at once.
ridge <- glmnet(X, y, family = "gaussian", alpha = 0,
                lambda = lambda_grid, standardize = FALSE, intercept = FALSE,
                control = list(thresh = 1e-12, maxit = 1000000))
stopifnot(length(ridge$lambda) == length(lambda_grid), ridge$jerr == 0)
k0 <- which.min(abs(ridge$lambda - lambda0))
beta_ridge_glmnet <- as.matrix(coef(ridge))[-1, k0]
print(cbind(OLS = ols$coefficients, ridge_formula = beta_ridge_formula,
            ridge_glmnet = beta_ridge_glmnet))
stopifnot(max(abs(beta_ridge_formula - beta_ridge_glmnet)) < 1e-5)

# Use package methods for coefficients and predictions at every fitted lambda.
B_ridge <- as.matrix(coef(ridge))[-1, , drop = FALSE]
pred_ridge <- predict(ridge, newx = X)
pred_ridge_test <- predict(ridge, newx = X_test)
ridge_table <- data.frame(
  lambda = ridge$lambda,
  l2_norm = sqrt(colSums(B_ridge^2)), l1_norm = colSums(abs(B_ridge)),
  nonzero = ridge$df,
  pure_loss = colMeans((y - pred_ridge)^2) / 2,
  penalty_base = colSums(B_ridge^2) / 2
)
ridge_table$penalty_term <- ridge_table$lambda * ridge_table$penalty_base
ridge_table$regularized_loss <- ridge_table$pure_loss + ridge_table$penalty_term
ridge_table$ols_loss <- ols_loss
ridge_table$excess_loss <- ridge_table$pure_loss - ols_loss
ridge_table$test_loss <- colMeans((y_test - pred_ridge_test)^2) / 2
ridge_table$ols_test_loss <- ols_test_loss

# Increasing lambda reduces L2 norm and increases training loss.
# The weighted penalty may rise and then fall: inspect rather than assume.
# OLS minimizes pure training loss; regularization cannot improve that loss.
print(ridge_table[show_rows, c("lambda", "l2_norm", "pure_loss", "penalty_term",
                              "regularized_loss", "ols_loss", "test_loss")],
      row.names = FALSE)

# C. LASSO: THE SAME ACCOUNTING, USING alpha=1 (5-8 min) -------------------
# No general matrix closed form exists. The base implementation is glmnet.
# glmnet uses coordinate descent, NOT the IRLS algorithm derived in the notes.
lasso <- glmnet(X, y, family = "gaussian", alpha = 1,
                lambda = lambda_grid, standardize = FALSE, intercept = FALSE,
                control = list(thresh = 1e-12, maxit = 1000000))
stopifnot(length(lasso$lambda) == length(lambda_grid), lasso$jerr == 0)
print(coef(lasso, s = lambda0))  # dots in sparse output represent exact zeros

B_lasso <- as.matrix(coef(lasso))[-1, , drop = FALSE]
pred_lasso <- predict(lasso, newx = X)
pred_lasso_test <- predict(lasso, newx = X_test)
lasso_table <- data.frame(
  lambda = lasso$lambda,
  l2_norm = sqrt(colSums(B_lasso^2)), l1_norm = colSums(abs(B_lasso)),
  nonzero = lasso$df,
  pure_loss = colMeans((y - pred_lasso)^2) / 2,
  penalty_base = colSums(abs(B_lasso))
)
lasso_table$penalty_term <- lasso_table$lambda * lasso_table$penalty_base
lasso_table$regularized_loss <- lasso_table$pure_loss + lasso_table$penalty_term
lasso_table$ols_loss <- ols_loss
lasso_table$excess_loss <- lasso_table$pure_loss - ols_loss
lasso_table$test_loss <- colMeans((y_test - pred_lasso_test)^2) / 2
lasso_table$ols_test_loss <- ols_test_loss

# L2 is tracked for comparison, but L1 is lasso's actual penalty norm.
# L1 must shrink as lambda increases. L2 and support size need not be monotone
# for a general correlated design; variables can leave and re-enter the model.
print(lasso_table[show_rows, c("lambda", "l2_norm", "l1_norm", "nonzero",
                              "pure_loss", "penalty_term", "regularized_loss",
                              "ols_loss", "test_loss")], row.names = FALSE)

# Native glmnet path plots: larger log(lambda) means stronger regularization.
# sign.lambda=1 makes that direction explicit. label=TRUE labels each curve
# by coefficient index j at its low-lambda end; the legend repeats the mapping.
par(mfrow = c(1, 2), mar = c(4, 4, 6, 1))
plot(ridge, xvar = "lambda", sign.lambda = 1, label = TRUE,
     col = seq_len(p), lwd = 2)
title("Ridge: coefficient paths", line = 4)
legend("bottomright", legend = paste0("Coefficient ", seq_len(p)),
       col = seq_len(p), lty = 1, lwd = 2, cex = 0.8, bty = "n")
plot(lasso, xvar = "lambda", sign.lambda = 1, label = TRUE,
     col = seq_len(p), lwd = 2)
title("Lasso: coefficient paths", line = 4)
legend("bottomright", legend = paste0("Coefficient ", seq_len(p)),
       col = seq_len(p), lty = 1, lwd = 2, cex = 0.8, bty = "n")

# Six panels disentangle the norms, pure losses and objective components.
# Manual code is only bookkeeping/plotting; glmnet performs all penalized fits.
for (method in c("Ridge", "Lasso")) {
  tab <- if (method == "Ridge") ridge_table else lasso_table
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  plot(tab$lambda, tab$l2_norm, type = "l", log = "x", lwd = 2,
       xlab = "lambda", ylab = "L2 norm", main = "Coefficient magnitude")
  plot(tab$lambda, tab$pure_loss, type = "l", log = "x", lwd = 2,
       ylim = range(c(tab$pure_loss, ols_loss)),
       xlab = "lambda", ylab = "RSS / (2n)", main = "Pure training loss")
  abline(h = ols_loss, col = "firebrick", lty = 2, lwd = 2)
  legend("topleft", "OLS", col = "firebrick", lty = 2, bty = "n")
  plot(tab$lambda, tab$penalty_term, type = "l", log = "x", lwd = 2,
       xlab = "lambda", ylab = "lambda x penalty base", main = "Weighted penalty")
  plot(tab$lambda, tab$regularized_loss, type = "l", log = "x", lwd = 2,
       xlab = "lambda", ylab = "Pure loss + weighted penalty",
       main = "Regularized objective")
  plot(tab$lambda, tab$penalty_base, type = "l", log = "x", lwd = 2,
       xlab = "lambda", ylab = if (method == "Ridge") "L2 norm squared / 2" else "L1 norm",
       main = "Unweighted penalty base")
  plot(tab$lambda, tab$test_loss, type = "l", log = "x", lwd = 2,
       ylim = range(c(tab$test_loss, ols_test_loss)),
       xlab = "lambda", ylab = "Test MSE / 2", main = "Independent test loss")
  abline(h = ols_test_loss, col = "firebrick", lty = 2, lwd = 2)
  legend("topleft", "OLS", col = "firebrick", lty = 2, bty = "n")
  mtext(paste(method, "| n=100, p=5, population correlation=0"), outer = TRUE)
}

# D-E. INCREASE DIMENSION, THEN CORRELATION (8-14 min) --------------------
# D: p=5,20,60,120 at rho=0. E: p=60 at rho=0,.5,.9,.99.
# Sigma = (1-rho)*I + rho*11'; multiply standard normals by chol(Sigma).
# Only three slopes remain nonzero. Extra features are independent noise
# variables in D and correlated proxies for the active features in E.
scenarios <- data.frame(
  experiment = rep(c("Dimension", "Correlation"), each = 4),
  p = c(5, 20, 60, 120, 60, 60, 60, 60),
  rho = c(0, 0, 0, 0, 0, 0.5, 0.9, 0.99)
)
path_parts <- list()
design_parts <- list()
for (i in seq_len(nrow(scenarios))) {
  p_i <- scenarios$p[i]
  rho_i <- scenarios$rho[i]
  Sigma_i <- (1 - rho_i) * diag(p_i) + rho_i * matrix(1, p_i, p_i)
  X_raw_i <- E[, 1:p_i] %*% chol(Sigma_i)
  X_test_raw_i <- E_test[, 1:p_i] %*% chol(Sigma_i)
  signal_i <- drop(X_raw_i[, 1:3] %*% beta_signal)
  signal_center_i <- mean(signal_i)
  signal_scale_i <- sqrt(mean((signal_i - signal_center_i)^2)) / sqrt(3)
  y_raw_i <- (signal_i - signal_center_i) / signal_scale_i + noise
  y_test_raw_i <- (drop(X_test_raw_i[, 1:3] %*% beta_signal) - signal_center_i) /
    signal_scale_i + noise_test

  center_i <- colMeans(X_raw_i)
  X_i <- sweep(X_raw_i, 2, center_i, "-")
  scale_i <- sqrt(colMeans(X_i^2))
  X_i <- sweep(X_i, 2, scale_i, "/")
  X_test_i <- sweep(sweep(X_test_raw_i, 2, center_i, "-"), 2, scale_i, "/")
  y_center_i <- mean(y_raw_i)
  y_scale_i <- sqrt(mean((y_raw_i - y_center_i)^2))
  y_i <- (y_raw_i - y_center_i) / y_scale_i
  y_test_i <- (y_test_raw_i - y_center_i) / y_scale_i

  # When p>=n, OLS coefficients are not unique. Use the minimum-L2 solution
  # from base R's SVD as an explicitly defined comparison in EVERY scenario.
  # A glmnet fit at a tiny positive lambda is not an exact OLS implementation.
  sx <- svd(X_i)
  keep <- sx$d > max(dim(X_i)) * max(sx$d) * .Machine$double.eps
  beta_ols_i <- drop(sx$v[, keep, drop = FALSE] %*%
    (crossprod(sx$u[, keep, drop = FALSE], y_i) / sx$d[keep]))
  ols_loss_i <- mean((y_i - drop(X_i %*% beta_ols_i))^2) / 2
  ols_test_i <- mean((y_test_i - drop(X_test_i %*% beta_ols_i))^2) / 2
  corr_i <- cor(X_i)
  design_parts[[i]] <- data.frame(
    scenario = i, experiment = scenarios$experiment[i], p = p_i, rho = rho_i,
    sample_correlation = mean(corr_i[upper.tri(corr_i)]), rank = sum(keep),
    condition_X = if (sum(keep) < p_i) Inf else max(sx$d) / min(sx$d),
    ols_l2 = sqrt(sum(beta_ols_i^2)), ols_loss = ols_loss_i, ols_test_loss = ols_test_i
  )

  for (a in c(0, 1)) {
    fit_i <- glmnet(X_i, y_i, family = "gaussian", alpha = a,
                    lambda = lambda_grid, standardize = FALSE,
                    intercept = FALSE,
                    control = list(thresh = 1e-12, maxit = 1000000))
    stopifnot(fit_i$jerr == 0, length(fit_i$lambda) == length(lambda_grid))
    B_i <- as.matrix(coef(fit_i))[-1, , drop = FALSE]
    fitted_i <- predict(fit_i, newx = X_i)
    test_i <- predict(fit_i, newx = X_test_i)
    tab_i <- data.frame(
      scenario = i, experiment = scenarios$experiment[i],
      method = if (a == 0) "Ridge" else "Lasso", p = p_i, rho = rho_i,
      lambda = fit_i$lambda, l2_norm = sqrt(colSums(B_i^2)),
      l1_norm = colSums(abs(B_i)), nonzero = fit_i$df,
      pure_loss = colMeans((y_i - fitted_i)^2) / 2,
      penalty_base = if (a == 0) colSums(B_i^2) / 2 else colSums(abs(B_i)),
      ols_loss = ols_loss_i, test_loss = colMeans((y_test_i - test_i)^2) / 2,
      ols_test_loss = ols_test_i
    )
    tab_i$penalty_term <- tab_i$lambda * tab_i$penalty_base
    tab_i$regularized_loss <- tab_i$pure_loss + tab_i$penalty_term
    tab_i$excess_loss <- tab_i$pure_loss - tab_i$ols_loss
    path_parts[[length(path_parts) + 1]] <- tab_i

    # Check loss accounting and theoretical monotonicity, within solver error.
    # Rows have DECREASING lambda: pure loss/objective fall; penalty base rises.
    stopifnot(min(tab_i$excess_loss) > -1e-7,
              max(diff(tab_i$pure_loss)) < 1e-7,
              min(diff(tab_i$penalty_base)) > -1e-6,
              max(diff(tab_i$regularized_loss)) < 1e-7,
              max(abs(deviance(fit_i) / (2*n) - tab_i$pure_loss)) < 1e-7)
  }
}
all_paths <- do.call(rbind, path_parts)
design_summary <- do.call(rbind, design_parts)

# D. Fix lambda=.1 to inspect increasing p, then examine all lambda curves.
# With p=120, centered X has rank at most 99. OLS can interpolate centered y,
# achieving essentially zero training loss while test loss remains substantial.
fixed_rows <- abs(log10(all_paths$lambda) - log10(lambda0)) < 1e-8
summary_columns <- c("method", "p", "rho", "lambda", "l2_norm", "l1_norm",
                     "nonzero", "pure_loss", "penalty_term", "regularized_loss",
                     "ols_loss", "test_loss", "ols_test_loss")
dimension_summary <- all_paths[fixed_rows & all_paths$experiment == "Dimension",
                               summary_columns]
print(design_summary[design_summary$experiment == "Dimension", ], row.names = FALSE)
print(dimension_summary, row.names = FALSE)

# E. Fix p=60 and increase correlation. Individual coefficients become harder
# to identify. Ridge distributes weight across correlated predictors; lasso
# may select a subset. Test loss/support need not change monotonically with rho.
# This is one controlled realization, not a Monte Carlo study of average risk.
correlation_summary <- all_paths[fixed_rows & all_paths$experiment == "Correlation",
                                 summary_columns]
print(design_summary[design_summary$experiment == "Correlation", ], row.names = FALSE)
print(correlation_summary, row.names = FALSE)

# Four pages, one for each experiment/method. Dashed lines show the OLS loss
# for the matching scenario, not the p=5 OLS loss reused for all scenarios.
colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
metrics <- c("l2_norm", "pure_loss", "penalty_term", "regularized_loss",
             "penalty_base", "test_loss")
titles <- c("Coefficient L2 norm", "Pure training loss", "Weighted penalty",
            "Regularized objective", "Unweighted penalty base", "Test loss")
for (experiment in c("Dimension", "Correlation")) {
  for (method in c("Ridge", "Lasso")) {
    tab <- all_paths[all_paths$experiment == experiment & all_paths$method == method, ]
    ids <- unique(tab$scenario)
    labels <- if (experiment == "Dimension") paste0("p=", scenarios$p[ids]) else
      paste0("rho=", scenarios$rho[ids])
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
    for (j in seq_along(metrics)) {
      field <- metrics[j]
      y_range <- range(tab[[field]])
      if (field == "pure_loss") y_range <- range(y_range, tab$ols_loss)
      if (field == "test_loss") y_range <- range(y_range, tab$ols_test_loss)
      plot(range(tab$lambda), y_range, type = "n", log = "x",
           xlab = "lambda", ylab = titles[j], main = titles[j])
      for (k in seq_along(ids)) {
        sub <- tab[tab$scenario == ids[k], ]
        lines(sub$lambda, sub[[field]], col = colors[k], lwd = 2)
        if (field == "pure_loss") abline(h = sub$ols_loss[1], col = colors[k], lty = 2)
        if (field == "test_loss") abline(h = sub$ols_test_loss[1], col = colors[k], lty = 2)
      }
      if (j == 1) legend("topright", labels, col = colors, lty = 1, lwd = 2,
                          cex = 0.8, bty = "n")
    }
    mtext(paste(experiment, "experiment |", method, "| n=100"), outer = TRUE, line = 1)
    mtext("Solid: regularized fit; dashed in loss panels: corresponding OLS",
          outer = TRUE, line = 0, cex = 0.8)
  }
}

# F. INTERPRETATION (14-15 min) ------------------------------------------
# 1. Training loss alone always favors unpenalized least squares.
# 2. Increasing lambda trades a smaller penalty norm for worse training fit.
#    Lasso's guaranteed monotone norm is L1, not L2; ridge's is L2.
# 3. The weighted penalty lambda*P(beta_hat) need not be monotone.
# 4. The minimized regularized objective is nondecreasing in lambda, but each
#    lambda defines a DIFFERENT objective. Do not select lambda by minimizing
#    this plotted value across lambda: it simply favors weak regularization.
# 5. Test loss illustrates generalization; it is not used here to choose lambda.
#    In practice use cv.glmnet on training data and evaluate the chosen fit on
#    the reserved test set once. See the optional package-native code below.
# 6. Strong correlation can destabilize coefficients even when predictions
#    remain reasonable. Correlation does not by itself imply poor prediction.
# 7. These are estimation/prediction plots, not confidence intervals after
#    selection. Debiased-lasso inference requires the lecture's assumptions.

write.csv(ridge_table, "regularized_glmnet_output/ridge_low_dimension.csv", row.names = FALSE)
write.csv(lasso_table, "regularized_glmnet_output/lasso_low_dimension.csv", row.names = FALSE)
write.csv(all_paths, "regularized_glmnet_output/all_lambda_paths.csv", row.names = FALSE)
write.csv(design_summary, "regularized_glmnet_output/design_summary.csv", row.names = FALSE)
write.csv(dimension_summary, "regularized_glmnet_output/dimension_at_lambda_0.1.csv", row.names = FALSE)
write.csv(correlation_summary, "regularized_glmnet_output/correlation_at_lambda_0.1.csv", row.names = FALSE)
do.call(glmnet.control, old_control)
par(old_par)
if (save_plots) dev.off()

# OPTIONAL follow-up, outside the 15-minute roadmap: built-in cross-validation.
# Raw training X/y and the default intercept/scaling are used here; its lambda
# scale is not directly interchangeable with the standardized demonstration.
# No test data are used to choose lambda. Both methods use the same folds.
# set.seed(20261119)
# folds <- sample(rep(1:10, length.out = n))
# cv_ridge <- cv.glmnet(X_raw, y_raw, alpha = 0, foldid = folds)
# cv_lasso <- cv.glmnet(X_raw, y_raw, alpha = 1, foldid = folds)
# plot(cv_ridge)
# plot(cv_lasso)
# coef(cv_lasso, s = "lambda.1se")
# predict(cv_lasso, newx = X_test_raw, s = "lambda.1se")
