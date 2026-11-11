## Two-way ANOVA with unbalanced data: Type I, II and III sums of squares
## Companion code to the lecture notes (Statistical Inference 2026-27, IIMB).
## Section numbers refer to the notes.

## ---- Data -------------------------------------------------------------------
d <- data.frame(
  A = factor(c(rep("Placebo", 6), rep("Drug", 4)),
             levels = c("Placebo", "Drug")),
  B = factor(c(rep("Low", 4), rep("High", 2), rep("Low", 2), rep("High", 2)),
             levels = c("Low", "High")),
  y = c(17, 19, 21, 23, 29, 31, 9, 11, 9, 11)
)

with(d, table(A, B))                       # check the design first
with(d, tapply(y, list(A, B), mean))

## ---- RSS of the five models (Section 3) -------------------------------------
fits <- list(null = y ~ 1, A = y ~ A, B = y ~ B, add = y ~ A + B, full = y ~ A * B)
sapply(fits, function(f) deviance(lm(f, data = d)))
#     null        A        B      add     full
# 586.0000 159.3333 559.3333  83.1429  26.0000

## ---- Interaction and Type I (Sections 4-5) ----------------------------------
mfull <- lm(y ~ A * B, data = d)
anova(mfull)                               # Type I, A first: 426.67, 76.19, 57.14
anova(lm(y ~ B * A, data = d))             # Type I, B first: 26.67, 476.19, 57.14
anova(lm(y ~ A + B, data = d), mfull)      # interaction: F = 13.19 on (1, 6)

## ---- Type II (Section 6) ----------------------------------------------------
# Type II numerator with the additive-model denominator: F = 40.1 on (1, 7)
anova(lm(y ~ B, data = d), lm(y ~ A + B, data = d))

if (requireNamespace("car", quietly = TRUE)) {
  # Type II with the full-model denominator: 476.19, 76.19, 57.14
  print(car::Anova(mfull, type = 2))

  ## ---- Type III and coding (Sections 7-8) -----------------------------------
  # Pitfall: Type III with default treatment coding tests Delta_L = 0
  print(car::Anova(mfull, type = 3))       # A row: 133.33

  # Correct: sum coding, set BEFORE fitting
  mfull_s <- lm(y ~ A * B, data = d,
                contrasts = list(A = contr.sum, B = contr.sum))
  print(car::Anova(mfull_s, type = 3))     # 514.29, 57.14, 57.14
  print(coef(mfull_s)["A1"])               # +7.5: contr.sum codes Placebo as +1
} else {
  message("Install the 'car' package for the Type II / III tables.")
}

## ---- Coding-free route: explicit cell-mean contrasts (Table 1) ---------------
d$cell <- factor(paste(d$A, d$B, sep = "_"),
                 levels = c("Placebo_Low", "Placebo_High", "Drug_Low", "Drug_High"))
mc <- lm(y ~ 0 + cell, data = d)

#              PL     PH     DL    DH
L <- rbind(
  Delta_L   = c(-1,     0,     1,    0),
  Delta_H   = c( 0,    -1,     0,    1),
  TypeII_A  = c(-4/7, -3/7,  4/7,  3/7),
  TypeIII_A = c(-1/2, -1/2,  1/2,  1/2),
  Pop_80_20 = c(-0.8, -0.2,  0.8,  0.2),
  Interact  = c( 1,    -1,    -1,    1)
)
est <- drop(L %*% coef(mc))
se  <- sqrt(diag(L %*% vcov(mc) %*% t(L)))
q   <- qt(0.975, df.residual(mc))
cbind(est, se, lower = est - q * se, upper = est + q * se,
      SS = est^2 / (se^2 / sigma(mc)^2))
# est: -10, -20, -14.29, -15, -12, -10   (pointwise, not simultaneous, intervals)

## ---- Interaction plot ---------------------------------------------------------
with(d, interaction.plot(B, A, y, type = "b", pch = c(1, 19),
     xlab = "Baseline severity", ylab = "Mean itching duration (s)",
     trace.label = "Treatment"))
