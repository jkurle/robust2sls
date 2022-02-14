# test algorithm when initial = "saturated" and iterations = "convergence"
library(robust2sls)
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                    mean_z = 0, cov_z = matrix(1),
                    Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                    Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
d <- generate_data(parameters = p, n = 1000)$data
model <- outlier_detection(data = d, formula = p$setting$formula,
                           ref_dist = "normal", sign_level = 0.01,
                           initial_est = "saturated", iterations = "convergence",
                           max_iter = 50, shuffle = FALSE, shuffle_seed = NULL,
                           split = 0.5, convergence_criterion = 0)
# should work now
