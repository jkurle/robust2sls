# setup
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
d <- generate_data(parameters = p, n = 1000)$data
r1 <- outlier_detection(data = d, formula = p$setting$formula,
                       ref_dist = "normal", sign_level = 0.05,
                       initial_est = "robustified", iterations = 3,
                       convergence_criterion = 0.5, split = 0.5)

# check whether non-parallel, it works
set.seed(10)
cr1 <- case_resampling(r1, 10, parallel = FALSE)
set.seed(10)
registerDoFuture()
plan(cluster, workers = 2)
cr1par <- case_resampling(r1, 10, parallel = TRUE)
plan(sequential)
identical(cr1, cr1par) # TRUE
# check saturated 2SLS
r2 <- outlier_detection(data = d, formula = p$setting$formula,
                        ref_dist = "normal", sign_level = 0.05,
                        initial_est = "saturated", iterations = 3, split = 0.5)
set.seed(10)
cr2 <- case_resampling(r2, 10, parallel = FALSE)
set.seed(10)
registerDoFuture()
plan(cluster, workers = 2)
cr2par <- case_resampling(r2, 10, parallel = TRUE)
plan(sequential)
identical(cr2, cr2par) # TRUE, works
# check convergence
r3 <- outlier_detection(data = d, formula = p$setting$formula,
                        ref_dist = "normal", sign_level = 0.05,
                        initial_est = "robustified", iterations = "convergence",
                        convergence_criterion = 0.5, split = 0.5)
set.seed(10)
cr3 <- case_resampling(r3, 10, parallel = FALSE)
set.seed(10)
registerDoFuture()
plan(cluster, workers = 2)
cr3par <- case_resampling(r3, 10, parallel = TRUE)
plan(sequential)
identical(cr3, cr3par) # TRUE
# check convergence AND saturated at the same time
r4 <- outlier_detection(data = d, formula = p$setting$formula,
                        ref_dist = "normal", sign_level = 0.05,
                        initial_est = "saturated", iterations = "convergence",
                        convergence_criterion = 0.5, split = 0.5)
set.seed(10)
cr4 <- case_resampling(r4, 10, parallel = FALSE, m = "convergence")
set.seed(10)
registerDoFuture()
plan(cluster, workers = 2)
cr4par <- case_resampling(r4, 10, m = "convergence", parallel = TRUE)
plan(sequential)
identical(cr4, cr4par) # TRUE


