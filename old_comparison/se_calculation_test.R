library(r2sls)

set.seed(123)
p <- generate_param(1,1,1)
d <- generate_data(p, 100)

model <- outlier_detection(data = d$data, formula = y ~ x1 + x2 | x1 + z2, ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iterations = 3, split = 0.5, shuffle_seed = 2)

summary(model$model$m0) # initial step coefficient estimates and standard errors

# how are the standard errors calculated?
model$model$m0$sigma * sqrt(model$model$m0$cov.unscaled) # replicates se

# hence $cov.unscaled is estimated M_xx_tilde_inv divided by n
est <- estimate_param_null(model)
est$params$Mxx_tilde_inv / 100
model$model$m0$cov.unscaled

# this should always only use the observations that were used in the estimation
# i.e. should be the robust estimate M_xx_tilde_inv_hat_(m)
# to check this, look at iteration

# standard errors of second iteration
summary(model$model$m1)
model$model$m1$sigma * sqrt(model$model$m1$cov.unscaled)

# estimate_param_null() uses the full sample to estimate, so always get the same
# to trick it, pretend as if the subsample (1) was the original sample
d2 <- d
d2$data <- d2$data[model$sel$m0, ]
NROW(d2$data) # 95 now, 5 were classified as outliers
model2 <- outlier_detection(data = d2$data, formula = y ~ x1 + x2 | x1 + z2, ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iterations = 3, split = 0.5, shuffle_seed = 2)

model$model$m1
model2$model$m0 # get same coefficients and se, so was calculated on same subsample

est2 <- estimate_param_null(model2)
est2$params$Mxx_tilde_inv / 95
model$model$m1$cov.unscaled # coincide!

# corrected inference
vro <- varrho(sign_level = model$cons$sign_level, ref_dist = model$cons$reference, iteration = 1)

beta_corr <- beta_inf_correction(model, iteration = 1)

# test whether correct se are extracted
se <- beta_inf(model, iteration = 1)
summary(model$model$m1)


