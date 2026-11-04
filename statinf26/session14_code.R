# Birth-weight lesson: data -> odds -> logistic model -> linear model -> Box-Cox.
# Run sequentially. Requires only the recommended package MASS and base R.
# Figures are written to the current working directory.
library(MASS)
d <- MASS::birthwt
d$smoke <- factor(d$smoke, c(0, 1), c("No", "Yes"))
d$age_c <- d$age - 25
d$lwt10 <- (d$lwt - 130)/10
stopifnot(all(d$bwt > 0))

# 1. Inspect the data BEFORE specifying a model.
# low is binary; bwt is continuous, in grams. lwt is maternal weight in pounds.
# The two responses describe the same births but answer different questions.
print(head(d[,c("low","bwt","smoke","age","lwt")]))
print(with(d,table(smoke,low)))
tab <- with(d,table(smoke,low))
group_risk <- prop.table(tab,1)[,"1"]
group_odds <- tab[,"1"]/tab[,"0"]
print(cbind(risk=group_risk,odds=group_odds))
print(c(odds_ratio=group_odds["Yes"]/group_odds["No"],
        risk_ratio=group_risk["Yes"]/group_risk["No"]))

# 2. Explain low birth weight: a Bernoulli response and a logit link.
# summary() prints Estimate, Std. Error, z value, Pr(>|z|).
# A logistic slope is a change in LOG ODDS, not a change in probability.
logit_fit <- glm(low ~ smoke + age_c + lwt10,binomial,data=d)
print(coef(summary(logit_fit)),digits=6)
# Exponentiate slopes to get conditional odds ratios. The exponentiated
# intercept is baseline ODDS, because it does not compare two profiles.
print(exp(coef(logit_fit)))
nd_logit <- data.frame(smoke=factor(c("No","Yes"),levels=levels(d$smoke)),
                       age_c=0,lwt10=0)
print(data.frame(smoke=nd_logit$smoke,
                 risk=predict(logit_fit,nd_logit,type="response")))

# Keep the SAME predictors while searching over response transformations.
raw_fit <- lm(bwt ~ smoke + age_c + lwt10, data=d, y=TRUE, qr=TRUE)
# 3. Explain actual birth weight: coefficients now represent GRAMS.
# summary() prints Estimate, Std. Error, t value, Pr(>|t|).
# There are n - 4 = 185 residual degrees of freedom.
print(coef(summary(raw_fit)),digits=6)
print(c(residual_df=df.residual(raw_fit),residual_sd=sigma(raw_fit),
        R_squared=summary(raw_fit)$r.squared))

# 4. Would changing the RESPONSE scale improve this working model?
# Do not transform the binary low outcome. Profile powers of positive bwt.
bc <- boxcox(raw_fit, lambda=seq(-2,3,.002), plotit=TRUE, interp=TRUE)
lambda_hat <- bc$x[which.max(bc$y)]
cutoff <- max(bc$y)-qchisq(.95,1)/2
lambda_ci <- range(bc$x[bc$y >= cutoff])
print(c(lambda_hat=lambda_hat, lower=lambda_ci[1], upper=lambda_ci[2]))

# The profile shows how much likelihood is lost by each power.
png("boxcox_profile.png",1300,800,res=160)
par(mar=c(4.3,4.3,2.4,1))
plot(bc$x,bc$y-max(bc$y),type="l",lwd=3,col="#21618C",ylim=c(-18,0),
     xlab=expression(lambda),ylab="Profile log-likelihood minus maximum",
     main="Box-Cox profile for birth weight")
abline(h=-qchisq(.95,1)/2,lty=2,col="grey40")
abline(v=c(0,1),lty=3,col=c("#A04000","grey40"))
points(lambda_hat,0,pch=19,col="#21618C")
segments(lambda_ci[1],-qchisq(.95,1)/2,lambda_ci[2],-qchisq(.95,1)/2,
         lwd=4,col="#21618C")
legend("bottomright",bty="n",legend=c(sprintf("MLE %.3f",lambda_hat),
  sprintf("95%% set [%.3f, %.3f]",lambda_ci[1],lambda_ci[2]),
  "0 = log; 1 = identity up to a shift"))
dev.off()

# Step 1: transform the OBSERVED birth weights using the selected power.
# expm1(a) computes exp(a)-1 accurately. The limiting case is log(y).
if (abs(lambda_hat) < 1e-8) {
  d$z <- log(d$bwt)
} else {
  d$z <- expm1(lambda_hat*log(d$bwt))/lambda_hat
}

# Step 2: regress transformed weight on the predictors.
bc_fit <- lm(z ~ smoke + age_c + lwt10, data=d)
z_hat <- fitted(bc_fit)

# Step 3: solve z_hat = (y_hat^lambda - 1)/lambda for y_hat.
# Check the inverse domain BEFORE evaluating a noninteger power.
# These are inverse-transformed fitted locations, NOT bias-corrected means.
if (abs(lambda_hat) < 1e-8) {
  inverse_fitted <- exp(z_hat)
} else {
  stopifnot(all(1+lambda_hat*z_hat > 0))
  inverse_fitted <- exp(log1p(lambda_hat*z_hat)/lambda_hat)
}
stopifnot(all(is.finite(inverse_fitted)))

# Show how the calculation works at a concrete maternal profile.
nd <- data.frame(smoke=factor(c("No","Yes"),levels=levels(d$smoke)),
                 age_c=0,lwt10=0)
profile_z <- predict(bc_fit,nd)
if (abs(lambda_hat) < 1e-8) {
  profile_inverse <- exp(profile_z)
} else {
  stopifnot(all(1+lambda_hat*profile_z > 0))
  profile_inverse <- exp(log1p(lambda_hat*profile_z)/lambda_hat)
}
worked_output <- data.frame(smoke=nd$smoke,
  fitted_transformed=profile_z, inverse_fitted_g=profile_inverse,
  ordinary_linear_fitted_g=predict(raw_fit,nd))
print(round(worked_output[-1],3))
print(worked_output)

# For a readable original-scale curve, fix age at 25 and vary weight.
# Each curve uses its specified smoking status; both are adjusted curves.
grid <- expand.grid(lwt10=seq(-4.5,7,length.out=200),
                    smoke=c("No","Yes"),age_c=0)
grid$smoke <- factor(grid$smoke,levels=levels(d$smoke))
grid_z <- predict(bc_fit,grid)
if (abs(lambda_hat) < 1e-8) {
  grid$inverse <- exp(grid_z)
} else {
  stopifnot(all(1+lambda_hat*grid_z > 0))
  grid$inverse <- exp(log1p(lambda_hat*grid_z)/lambda_hat)
}
grid$raw <- predict(raw_fit,grid)

# Left: actual outcomes against inverse fitted values, all in GRAMS.
# A point on the diagonal has observed weight equal to fitted location.
# This is an in-sample illustration, not held-out prediction performance.
# Right: inverse fitted curves (solid) versus ordinary linear fits (dashed).
# Do not interpret the solid curves as arithmetic conditional means.
png("boxcox_inverse_fitted.png",1800,850,res=175)
par(mfrow=c(1,2),mar=c(4.6,4.6,3.2,1))
cols <- c("#21618C","#A04000")
lims <- range(c(d$bwt,inverse_fitted))
plot(inverse_fitted,d$bwt,pch=19,col=adjustcolor(cols[as.integer(d$smoke)],.7),
     xlim=lims,ylim=lims,asp=1,
     xlab="Inverse-transformed fitted birth weight (g)",
     ylab="Observed birth weight (g)",main="Return fitted values to grams")
abline(0,1,lty=2,col="grey35",lwd=2)
legend("topleft",c("Nonsmoker","Smoker","Observed = fitted"),
       col=c(cols,"grey35"),pch=c(19,19,NA),lty=c(NA,NA,2),bty="n",cex=.85)
plot(NA,xlim=c(85,200),ylim=range(c(grid$inverse,grid$raw)),
     xlab="Maternal weight at last menstrual period (lb)",
     ylab="Fitted birth weight on original scale (g)",
     main="Adjusted fitted curves: maternal age 25")
for (j in 1:2) {
  g <- grid[grid$smoke == levels(d$smoke)[j],]
  lines(130+10*g$lwt10,g$inverse,col=cols[j],lwd=2.5)
  lines(130+10*g$lwt10,g$raw,col=cols[j],lwd=2,lty=2)
}
legend("topleft",c("No: inverse Box-Cox","No: ordinary linear",
                   "Yes: inverse Box-Cox","Yes: ordinary linear"),
       col=rep(cols,each=2),lty=rep(c(1,2),2),lwd=2,bty="n",cex=.8)
dev.off()

# Numerical checks of the inverse: applying T_lambda again recovers z_hat.
retransformed <- if (abs(lambda_hat)<1e-8) log(inverse_fitted) else
  expm1(lambda_hat*log(inverse_fitted))/lambda_hat
stopifnot(max(abs(retransformed-z_hat)) < 1e-6)
print(c(inverse_fitted_min=min(inverse_fitted),inverse_fitted_max=max(inverse_fitted),
        observed_min=min(d$bwt),observed_max=max(d$bwt)))

# 5. Make the final scale choice AFTER examining the profile and inverse fit.
# The 95% set contains 1 but not 0: keep birth weight in grams here.
# The estimated-power fit above illustrates the transformation/inversion steps.
k <- match(c(0,1),bc$x)
print(data.frame(lambda=bc$x[k],loglik_loss=max(bc$y)-bc$y[k]))
final_fit <- raw_fit
# Interpret the selected model on its original gram scale.
print(predict(final_fit,nd))
