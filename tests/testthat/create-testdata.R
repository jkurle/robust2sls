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

# ##### to test generate_data directly
# ### artificial data IV
# p4 <- generate_param(3, 2, 3, seed = 42)
# set.seed(42)
# d4 <- generate_data(p4, n = 30)
# saveRDS(d4, file = test_path("./testdata/testdata4.rds"))
# # used once in test-monte_carlo
#
# ### artificial data V
# p5 <- generate_param(3, 2, 3, seed = 42)
# set.seed(42)
# d5 <- generate_data(p5, n = 500000)
# saveRDS(d5, file = test_path("./testdata/testdata5.rds"))
# # used once in test-monte_carlo

### artificial data VI
p6 <- generate_param(1, 1, 1, seed = 40)
d6 <- generate_data(parameters = p6, n = 1000)$data
saveRDS(d6, file = test_path("./testdata/testdata6.rds"))
# used once in test-outlier_test

# data in mc_grid is special, created inside
# to save, temporarily add a saveRDS() command inside the mc_grid() function

### d1 (line 266)
saveRDS(d, paste0("tests/testthat/testdata/mcgrid/d1/n", n, "_m", m, ".rds"))
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
results <- mc_grid(100, n = c(100, 1000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05), initial_est = c("saturated", "robustified"), iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = c(0.3, 0.4, 0.5))

### d2 (line 290, 310)
saveRDS(d, paste0("tests/testthat/testdata/mcgrid/d2/n", n, "_m", m, ".rds"))
p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)
out <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p, formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iterations = "convergence", convergence_criterion = 0)

### d3 (line 339, 346, 353, 362, 370, 378, 407, 424, 445, 452)
saveRDS(d, paste0("tests/testthat/testdata/mcgrid/d3/n", n, "_m", m, ".rds"))
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
mc_grid(10, n = c(100, 1000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05), initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5, verbose = TRUE)

