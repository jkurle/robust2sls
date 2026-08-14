# IMPORTANT: To be used interactively, cannot simply run to re-create the testing data
# create fixed datasets to test functions that are not about MC/generating data
library(robust2sls)
library(testthat)

### artificial data I
p1 <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
d1 <- generate_data(parameters = p1, n = 1000)$data
saveRDS(d1, file = test_path("./testdata/testdata1.rds"))
# used twice in test-beta_inf_test
# used thrice in test-bootstrap
# used twelve times in test-outlier_tests

### artifical data II
p2 <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                     mean_z = 0, cov_z = matrix(1),
                     Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                     Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
d2 <- generate_data(parameters = p2, n = 50)$data
saveRDS(d2, file = test_path("./testdata/testdata2.rds"))
# used once in test-initial_estimators
# used twice in test-iterative_estimators2
# used twice in test-utility

### artificial data III
p3 <- generate_param(dx1 = 1, dx2 = 1, dz2 = 2, intercept = TRUE,
                     beta = c(2, 4), sigma = 1, mean_z = matrix(c(0,0), 2, 1),
                     cov_z = matrix(c(1,0,0,1), 2, 2),
                     Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                     Pi = t(matrix(c(1, 0, 0, 1, 0, 1), nrow = 2)))
d3 <- generate_data(parameters = p3, n = 50)$data
saveRDS(d3, file = test_path("./testdata/testdata3.rds"))
# used once in test-iterative_estimators2

# data in mc_grid is special, created inside
# to save, temporarily add a saveRDS() command inside the mc_grid() function

### d4 new fixture to be used across all tests
# saveRDS(d, paste0("tests/testthat/testdata/mcgrid/d4/n", n, "_m", m, ".rds"))
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
mc_grid(4, n = c(50), seed = 99991, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05), initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5, verbose = TRUE)
mc_grid(4, n = c(100), seed = 2389, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05), initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5, verbose = TRUE)
