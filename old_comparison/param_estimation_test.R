

p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
d <- generate_data(p, 5000000)

Sigma2 <- p$params$Sigma2_half %*% p$params$Sigma2_half
R2 <- d$data[, c("r4", "r5")]
Sigma2_est <- cov(R2)
is.positive.definite(Sigma2_est) # TRUE

Sigma2_half <- p$params$Sigma2_half
Sigma2_half_est <- sqrtm(Sigma2_est)
is.positive.definite(Sigma2_half_est) # says is not symmetric but is sign loss
# happens in place 16 of decimal
Sigma2_half_est <- round(Sigma2_half_est, digits = 15)
is.positive.definite(Sigma2_half_est) # TRUE

Sigma2_half_inv <- inv(Sigma2_half)
Sigma2_half_inv_est <- inv(Sigma2_half_est)

# now can estimate from the first stage projections instead of true 1st stage?
model <- ivreg(y ~ -1 + x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6, data = d$data, model = TRUE)
summary(model, component = "projected")
terms(model)

model.matrix(model, component = c("projected")) # are those the fitted values from 1st stage?

# yes looks like it...probably should check though
fitx <- model.matrix(model, component = "projected")[1:10, ]
x <- d$data[1:10, c("x1","x2","x3","x4","x5")]

# manual first stage
first_stage_x4 <- lm(x4 ~ -1 + x1 + x2 + x3 + z4 + z5 + z6, data = d$data)
first_stage_x5 <- lm(x5 ~ -1 + x1 + x2 + x3 + z4 + z5 + z6, data = d$data)

fit_x4 <- matrix(predict(first_stage_x4)[1:10], 10, 1)
fit_x5 <- matrix(predict(first_stage_x5)[1:10], 10, 1)

fitx <- cbind(fitx, fit_x4, fit_x5)
# yes they look the same!
names(fitx) <- NULL
identical(fitx[,4], fitx[,6]) # FALSE
identical(fitx[,5], fitx[,7]) # FALSE
compare(fitx[,4], fitx[,6]) # only very tiny differences in digits, usually after 10th place
compare(fitx[,5], fitx[,7])

identical(round(fitx[,4],9), round(fitx[,6],9)) # TRUE
identical(round(fitx[,5],9), round(fitx[,7],9)) # TRUE

# get the first stage errors
res <- d$data[, c("x4","x5")] - model.matrix(model, component = c("projected"))[, c("x4","x5")]
yyy <- cov(res) # is very close to Sigma2_est
is.positive.definite(yyy) # TRUE
sqrtyyy <- sqrtm(yyy)
sqrtyyyinv <- inv(sqrtyyy)









# trying to estimate Omega(2) ####################################

R2hat <- res
uhat <- model$residuals
R2hat * uhat # does it by column, so what we want
ER2u_hat <- colMeans((R2hat * uhat))

ER2u <- p$params$sigma * p$params$Sigma2_half %*% p$params$Omega2

# for first iteration

Omega2_hat <- sqrtyyyinv %*% matrix(ER2u_hat, 2, 1) / model$sigma

# trying to estimate Pi ##############
d$data$z2 <- d$data$x2
first_stage_x2 <- lm(x2 ~ -1 + x1 + z2 + x3 + z4 + z5 + z6, data = d$data)
first_stage_x4 <- lm(x4 ~ -1 + x1 + x2 + x3 + z4 + z5 + z6, data = d$data)
first_stage_x5 <- lm(x5 ~ -1 + x1 + x2 + x3 + z4 + z5 + z6, data = d$data)

as.matrix(first_stage_x4$coefficients, 6, 1)
as.matrix(first_stage_x5$coefficients, 6, 1)

a <- cbind(as.matrix(first_stage_x4$coefficients, 6, 1), as.matrix(first_stage_x5$coefficients, 6, 1))
Pi_hat <- cbind(rbind(diag(3), matrix(0,3,3)), a)
p$params$Pi
rankMatrix(p$params$Pi)
rankMatrix(Pi_hat) # also rank 5 (full)

# trying to estimate Mzz and Mxx ##########
Z <- d$data[, c("x1","x2", "x3", "z4", "z5", "z6")]
ZZ <- t(as.matrix(Z)) %*% as.matrix(Z)
Mzz_est <- ZZ / NROW(d$data)
p$params$Mzz # very similar, seems consistent

M_xx_tilde_est <- t(Pi_hat) %*% Mzz_est %*% Pi_hat
M_xx_tilde_inv_est <- inv(M_xx_tilde_est)
p$params$Mxx_tilde_inv # very close
p$params$Mxx_tilde_inv %*% M_xx_tilde_est # is decently close to identity matrix







# real example as if data existed
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
d <- generate_data(p, 5000000)
model1 <- outlier_detection(d$data, p$setting$formula, "normal", 0.05, "robustified", iterations = 1)
p_est <- estimate_param(model1, 0)


gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p_est, split = 0.5)
# identical bc under normal Omega, Mxx_tilde_inv, and Sigma_half not needed (0)

gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p_est, split = 0.5)
