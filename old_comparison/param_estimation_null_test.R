p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
d <- generate_data(p, 500000)
model1 <- outlier_detection(d$data, p$setting$formula, "normal", 0.05, "robustified", iterations = 1)
p_est <- estimate_param_null(model1)

p$params$Omega
p_est$params$Omega

p$params$Mxx_tilde_inv
p_est$params$Mxx_tilde_inv

p$params$Sigma_half
p_est$params$Sigma_half
