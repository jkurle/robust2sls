test_that("test_cpv() works correctly", {

  expect_error(test_cpv(c("a", "b", "c"), 1, 0.1), "'dist' must be a numeric vector")
  expect_error(test_cpv(c(0, 1, 2), "a", 0.1), "'teststat' must be a single numeric value")
  expect_error(test_cpv(c(0, 1, 2), c(1.1, 1.2), 0.1), "'teststat' must be a single numeric value")
  expect_error(test_cpv(c(0, 1, 2), 1, c("a", "b")), "'p' must be a numeric vector")
  expect_error(test_cpv(c(0, 1, 2), 1, c(0.1, 0.2, 1.1)), "Elements of 'p' must lie between 0 and 1.")

  set.seed(40)
  d <- rnorm(10000)
  t <- 1.5
  probs <- c(0.05, 0.1, 0.9, 0.95)
  a <- test_cpv(d, t, probs)
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("pval", "critical"))
  expect_length(a$critical, 4)
  expect_named(a$critical, c("5%", "10%", "90%", "95%"))
  expect_snapshot_output(a)

})

test_that("simes() works correctly", {

  expect_error(simes(pvals = c("a", "b"), alpha = 0.05), "'pvals' must be a numeric vector")
  expect_error(simes(pvals = c(0.1, 0.05, 1.1), alpha = 0.05), "'pvals' must lie between 0 and 1")
  expect_error(simes(pvals = c(0.3, 0.4), alpha = "a"), "'alpha' must be a single numeric value")
  expect_error(simes(pvals = c(0.3, 0.4), alpha = c(0.01, 0.05)), "'alpha' must be a single numeric value")
  expect_error(simes(pvals = c(0.3, 0.4), alpha = -0.1), "'alpha' must lie between 0 and 1")
  expect_error(simes(pvals = c(0.3, 0.4), alpha = 2), "'alpha' must lie between 0 and 1")

  p <- c(0.3, 0.2, 0.15)
  a <- 0.1
  s1 <- simes(pvals = p, alpha = a)
  expect_length(s1, 3)
  expect_type(s1, "list")
  expect_named(s1, c("reject", "alpha", "details"))
  expect_type(s1$reject, "logical")
  expect_type(s1$alpha, "double")
  expect_type(s1$details, "list")
  expect_equal(class(s1$details), "data.frame")
  expect_identical(s1$reject, FALSE)
  expect_identical(s1$alpha, a)
  df <- data.frame(pvals = c(0.15, 0.2, 0.3), alpha_adj = c(0.1/3, 0.2/3, 0.1),
                   reject = c(FALSE, FALSE, FALSE))
  expect_equal(s1$details, df)

  p <- c(0.01, 0.2, 0.5)
  a <- 0.1
  s2 <- simes(pvals = p, alpha = a)
  expect_length(s2, 3)
  expect_type(s2, "list")
  expect_named(s2, c("reject", "alpha", "details"))
  expect_type(s2$reject, "logical")
  expect_type(s2$alpha, "double")
  expect_type(s2$details, "list")
  expect_equal(class(s2$details), "data.frame")
  expect_identical(s2$reject, TRUE)
  expect_identical(s2$alpha, a)
  df <- data.frame(pvals = c(0.01, 0.2, 0.5), alpha_adj = c(0.1/3, 0.2/3, 0.1),
                   reject = c(TRUE, FALSE, FALSE))
  expect_equal(s2$details, df)

  expect_snapshot_output(s1)
  expect_snapshot_output(s2)

})

test_that("multi_cutoff() works correctly", {

  library(robust2sls)
  p <- generate_param(1, 1, 1, seed = 40)
  d <- generate_data(parameters = p, n = 1000)$data
  f <- p$setting$formula

  expect_error(multi_cutoff(gamma = c("a", "b"), data = d, formula = f,
                            ref_dist = "normal", initial_est = "robustified",
                            iterations = 1),
               "'gamma' must be a numeric vector")
  expect_error(multi_cutoff(gamma = c(-0.1, 0.2), data = d, formula = f,
                            ref_dist = "normal", initial_est = "robustified",
                            iterations = 1),
               "'gamma' must lie between 0 and 1")
  expect_error(multi_cutoff(gamma = c(3, 0.2), data = d, formula = f,
                            ref_dist = "normal", initial_est = "robustified",
                            iterations = 1),
               "'gamma' must lie between 0 and 1")

  # test two different backends
  gamma1 <- c(0.01, 0.02)
  library(doFuture, quietly = TRUE)
  registerDoFuture()
  plan(cluster, workers = 2)
  a0 <- multi_cutoff(gamma = gamma1, data = d, formula = f, ref_dist = "normal",
                     initial_est = "robustified", iterations = 0)
  plan(sequential)
  b0 <- multi_cutoff(gamma = gamma1, data = d, formula = f, ref_dist = "normal",
                     initial_est = "robustified", iterations = 0)
  # they will differ in their environments, so need to set to 0 manually
  attr(a0$gamma0.01$cons$formula, '.Environment') <- NULL
  attr(b0$gamma0.01$cons$formula, '.Environment') <- NULL
  attr(a0$gamma0.01$model$m0$formula, '.Environment') <- NULL
  attr(b0$gamma0.01$model$m0$formula, '.Environment') <- NULL
  attr(a0$gamma0.01$model$m0$terms$regressors, '.Environment') <- NULL
  attr(b0$gamma0.01$model$m0$terms$regressors, '.Environment') <- NULL
  attr(a0$gamma0.01$model$m0$terms$instruments, '.Environment') <- NULL
  attr(b0$gamma0.01$model$m0$terms$instruments, '.Environment') <- NULL
  attr(a0$gamma0.01$model$m0$terms$full, '.Environment') <- NULL
  attr(b0$gamma0.01$model$m0$terms$full, '.Environment') <- NULL
  attr(attr(a0$gamma0.01$model$m0$model, 'terms'), '.Environment') <- NULL
  attr(attr(b0$gamma0.01$model$m0$model, 'terms'), '.Environment') <- NULL
  attr(a0$gamma0.02$cons$formula, '.Environment') <- NULL
  attr(b0$gamma0.02$cons$formula, '.Environment') <- NULL
  attr(a0$gamma0.02$model$m0$formula, '.Environment') <- NULL
  attr(b0$gamma0.02$model$m0$formula, '.Environment') <- NULL
  attr(a0$gamma0.02$model$m0$terms$regressors, '.Environment') <- NULL
  attr(b0$gamma0.02$model$m0$terms$regressors, '.Environment') <- NULL
  attr(a0$gamma0.02$model$m0$terms$instruments, '.Environment') <- NULL
  attr(b0$gamma0.02$model$m0$terms$instruments, '.Environment') <- NULL
  attr(a0$gamma0.02$model$m0$terms$full, '.Environment') <- NULL
  attr(b0$gamma0.02$model$m0$terms$full, '.Environment') <- NULL
  attr(attr(a0$gamma0.02$model$m0$model, 'terms'), '.Environment') <- NULL
  attr(attr(b0$gamma0.02$model$m0$model, 'terms'), '.Environment') <- NULL
  expect_equal(a0, b0)

  # now do manually
  cpart1 <- outlier_detection(data = d, formula = f, ref_dist = "normal",
                              initial_est = "robustified", iterations = 0,
                              sign_level = 0.01)
  cpart2 <- outlier_detection(data = d, formula = f, ref_dist = "normal",
                              initial_est = "robustified", iterations = 0,
                              sign_level = 0.02)
  c0 <- list(gamma0.01 = cpart1, gamma0.02 = cpart2)
  # environments but also call will be different, so need to remove manually
  attr(c0$gamma0.01$cons$formula, '.Environment') <- NULL
  attr(c0$gamma0.02$cons$formula, '.Environment') <- NULL
  attr(c0$gamma0.01$model$m0$formula, '.Environment') <- NULL
  attr(c0$gamma0.02$model$m0$formula, '.Environment') <- NULL
  attr(c0$gamma0.01$model$m0$terms$regressors, '.Environment') <- NULL
  attr(c0$gamma0.02$model$m0$terms$regressors, '.Environment') <- NULL
  attr(c0$gamma0.01$model$m0$terms$instruments, '.Environment') <- NULL
  attr(c0$gamma0.02$model$m0$terms$instruments, '.Environment') <- NULL
  attr(c0$gamma0.01$model$m0$terms$full, '.Environment') <- NULL
  attr(c0$gamma0.02$model$m0$terms$full, '.Environment') <- NULL
  attr(attr(c0$gamma0.01$model$m0$model, 'terms'), '.Environment') <- NULL
  attr(attr(c0$gamma0.02$model$m0$model, 'terms'), '.Environment') <- NULL
  a0$gamma0.01$cons$call <- NULL
  c0$gamma0.01$cons$call <- NULL
  a0$gamma0.02$cons$call <- NULL
  c0$gamma0.02$cons$call <- NULL
  expect_equal(a0, c0)

  # reset to original
  a0 <- multi_cutoff(gamma = gamma1, data = d, formula = f, ref_dist = "normal",
                     initial_est = "robustified", iterations = 0)
  expect_type(a0, "list")
  expect_length(a0, length(gamma1))
  expect_named(a0, c("gamma0.01", "gamma0.02"))
  expect_equal(class(a0$gamma0.01), "robust2sls")
  expect_equal(class(a0$gamma0.02), "robust2sls")

  expect_snapshot_output(a0)

})

test_that("proptest() works correctly", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.1, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = "convergence", convergence_criterion = 0,
                         max_iter = 200)

  # store this so that notice if output changes, which would affect other tests
  expect_snapshot_output(models)

  # for smaller gammas seems to converge (quicker) but for larger ones not
  # 0.07, 0.08, 0.1 it goes up to 200 iterations
  a <- proptest(models, alpha = 0.05, iteration = 0, one_sided = FALSE)
  b <- proptest(models, alpha = 0.05, iteration = 1, one_sided = FALSE)
  c <- proptest(models, alpha = 0.05, iteration = "convergence",
                one_sided = FALSE)

  expect_equal(class(a), "data.frame")
  expect_equal(class(b), "data.frame")
  expect_equal(class(c), "data.frame")
  expect_equal(NROW(a), length(gammas))
  expect_equal(NROW(b), length(gammas))
  expect_equal(NROW(c), length(gammas))
  expect_equal(NCOL(a), 8)
  expect_equal(NCOL(b), 8)
  expect_equal(NCOL(c), 8)
  expect_equal(a$iter_test, rep(0, 10))
  expect_equal(b$iter_test, rep(1, 10))
  expect_equal(c$iter_test, rep("convergence", 10))
  expect_equal(a$iter_act, rep(0, 10))
  expect_equal(b$iter_act, rep(1, 10))
  expect_equal(c$iter_act, c(4, 3, 3, 3, 6, 7, 200, 200, 6, 200))
  expect_equal(a$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(b$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(c$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(a$type, rep("two-sided", 10))
  expect_equal(b$type, rep("two-sided", 10))
  expect_equal(c$type, rep("two-sided", 10))
  expect_equal(a$alpha, rep(0.05, 10))
  expect_equal(b$alpha, rep(0.05, 10))
  expect_equal(c$alpha, rep(0.05, 10))
  expect_equal(a$reject, c(FALSE, FALSE, FALSE, TRUE, rep(FALSE, 6)))
  expect_equal(b$reject, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                           TRUE, FALSE, FALSE))
  expect_equal(c$reject, rep(FALSE, 10))

  expect_snapshot_output(a)
  expect_snapshot_output(b)
  expect_snapshot_output(c)

  # try different setting
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 10)
  a <- proptest(models, alpha = 0.05, iteration = 3, one_sided = TRUE)
  b <- proptest(models, alpha = 0.01, iteration = 5, one_sided = FALSE)
  expect_equal(class(a), "data.frame")
  expect_equal(class(b), "data.frame")
  expect_equal(NROW(a), length(gammas))
  expect_equal(NROW(b), length(gammas))
  expect_equal(NCOL(a), 8)
  expect_equal(NCOL(b), 8)
  expect_equal(a$alpha, rep(0.05, 10))
  expect_equal(b$alpha, rep(0.01, 10))
  expect_equal(a$iter_test, rep(3, 10))
  expect_equal(b$iter_test, rep(5, 10))
  expect_equal(a$iter_act, rep(3, 10))
  expect_equal(b$iter_act, rep(5, 10))
  expect_equal(a$type, rep("one-sided", 10))
  expect_equal(b$type, rep("two-sided", 10))

  expect_snapshot_output(a)
  expect_snapshot_output(b)

  # try a single robust2sls_object instead of a list
  model <- outlier_detection(data = d, formula = p$setting$formula,
                             initial_est = "saturated", ref_dist = "normal",
                             sign_level = 0.05, iterations = 3, split = 0.5)
  a <- proptest(model, alpha = 0.1, iteration = 1, one_sided = FALSE)
  b <- proptest(model, alpha = 0.1, iteration = 1, one_sided = TRUE)
  expect_equal(NROW(a), 1)
  expect_equal(NROW(b), 1)
  expect_equal(NCOL(a), 8)
  expect_equal(NCOL(b), 8)
  expect_equal(a$iter_test, 1)
  expect_equal(b$iter_test, 1)
  expect_equal(a$iter_act, 1)
  expect_equal(b$iter_act, 1)
  expect_equal(a$gamma, 0.05)
  expect_equal(b$gamma, 0.05)
  expect_equal(a$t, b$t) # should get same t statistic
  expect_equal(a$type, "two-sided")
  expect_equal(b$type, "one-sided")
  expect_equal(a$alpha, 0.1)
  expect_equal(b$alpha, 0.1)
  expect_equal(a$reject, FALSE)
  expect_equal(b$reject, FALSE)
  expect_snapshot_output(a)
  expect_snapshot_output(b)
  # re-build test statistic and p-values manually
  sample_fodr <- outliers_prop(model, iteration = 1)
  expected_fodr <- 0.05
  n <- NROW(d) # there are no missings, so each observation is actually used
  param_est <- estimate_param_null(model)
  avar_est <- gauge_avar(ref_dist = "normal", sign_level = 0.05,
                         initial_est = "saturated", iteration = 1,
                         parameters = param_est, split = 0.5)
  tval <- sqrt(n) * (sample_fodr - expected_fodr) / sqrt(avar_est)
  tval <- c(tval)# need to put in c() to convert 1x1 matrix to scalar
  expect_identical(tval, a$t)

  pval1side <- pnorm(q = tval, mean = 0, sd = 1, lower.tail = FALSE) # reject for large positive values
  pval2side <- 2*pnorm(q = abs(tval), mean = 0, sd = 1, lower.tail = FALSE)
  expect_identical(pval1side, b$pval)
  expect_identical(pval2side, a$pval)

})

test_that("proptest() raises correct errors", {

  expect_error(proptest(robust2sls_object = 1, alpha = 0.05, iteration = 1,
                        one_sided = TRUE),
               "'robust2sls_object' must be of class 'robust2sls' or a list")
  expect_error(proptest(robust2sls_object = list(1, 2), alpha = 0.05,
                        iteration = 1, one_sided = FALSE),
               "is a list but not all elements have class 'robust2sls'")

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.05, 0.1)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 2)

  expect_error(proptest(models, alpha = "a", iteration = 1, one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(proptest(models, alpha = c(1,1), iteration = 1,
                        one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(proptest(models, alpha = 1.2, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(proptest(models, alpha = -4, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(proptest(models, alpha = 0.05, iteration = FALSE,
                        one_sided = FALSE),
               "'iteration' must either be a numeric")
  expect_error(proptest(models, alpha = 0.05, iteration = "abc",
                        one_sided = FALSE),
               "'iteration' must either be a numeric or the string 'convergence'")
  expect_error(proptest(models, alpha = 0.05, iteration = c(1, 2),
                        one_sided = FALSE),
               "'iteration' must be of length one")
  expect_error(proptest(models, alpha = 0.05, iteration = 1.3,
                        one_sided = FALSE),
               "'iteration' must be an integer")
  expect_error(proptest(models, alpha = 0.01, iteration = 1,
                        one_sided = "FALSE"),
               "'one_sided' must be a logical value of length one")
  expect_error(proptest(models, alpha = 0.01, iteration = 1,
                        one_sided = c(TRUE, FALSE)),
               "'one_sided' must be a logical value of length one")

})

test_that("counttest() raises correct errors", {

  expect_error(counttest(robust2sls_object = 1, alpha = 0.05, iteration = 1,
                        one_sided = TRUE),
               "'robust2sls_object' must be of class 'robust2sls' or a list")
  expect_error(counttest(robust2sls_object = list(1, 2), alpha = 0.05,
                        iteration = 1, one_sided = FALSE),
               "is a list but not all elements have class 'robust2sls'")

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.05, 0.1)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 2)

  expect_error(counttest(models, alpha = "a", iteration = 1, one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(counttest(models, alpha = c(1,1), iteration = 1,
                        one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(counttest(models, alpha = 1.2, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(counttest(models, alpha = -4, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(counttest(models, alpha = 0.05, iteration = FALSE,
                        one_sided = FALSE),
               "'iteration' must either be a numeric")
  expect_error(counttest(models, alpha = 0.05, iteration = "abc",
                        one_sided = FALSE),
               "'iteration' must either be a numeric or the string 'convergence'")
  expect_error(counttest(models, alpha = 0.05, iteration = c(1, 2),
                        one_sided = FALSE),
               "'iteration' must be of length one")
  expect_error(counttest(models, alpha = 0.05, iteration = 1.3,
                        one_sided = FALSE),
               "'iteration' must be an integer")
  expect_error(counttest(models, alpha = 0.01, iteration = 1,
                        one_sided = "FALSE"),
               "'one_sided' must be a logical value of length one")
  expect_error(counttest(models, alpha = 0.01, iteration = 1,
                        one_sided = c(TRUE, FALSE)),
               "'one_sided' must be a logical value of length one")

})

test_that("counttest() works correctly", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.1, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = "convergence", convergence_criterion = 0,
                         max_iter = 200)

  # store this so that notice if output changes, which would affect other tests
  expect_snapshot_output(models)

  # for smaller gammas seems to converge (quicker) but for larger ones not
  # 0.07, 0.08, 0.1 it goes up to 200 iterations
  a <- counttest(models, alpha = 0.05, iteration = 0, one_sided = FALSE)
  b <- counttest(models, alpha = 0.05, iteration = 1, one_sided = FALSE)
  c <- counttest(models, alpha = 0.05, iteration = "convergence",
                one_sided = FALSE)

  expect_equal(class(a), "data.frame")
  expect_equal(class(b), "data.frame")
  expect_equal(class(c), "data.frame")
  expect_equal(NROW(a), length(gammas))
  expect_equal(NROW(b), length(gammas))
  expect_equal(NROW(c), length(gammas))
  expect_equal(NCOL(a), 9)
  expect_equal(NCOL(b), 9)
  expect_equal(NCOL(c), 9)
  expect_equal(colnames(a), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(colnames(b), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(colnames(c), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(a$iter_test, rep(0, 10))
  expect_equal(b$iter_test, rep(1, 10))
  expect_equal(c$iter_test, rep("convergence", 10))
  expect_equal(a$iter_act, rep(0, 10))
  expect_equal(b$iter_act, rep(1, 10))
  expect_equal(c$iter_act, c(4, 3, 3, 3, 6, 7, 200, 200, 6, 200))
  expect_equal(a$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(b$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(c$gamma, seq(0.01, 0.1, 0.01))
  expect_equal(a$type, rep("two-sided", 10))
  expect_equal(b$type, rep("two-sided", 10))
  expect_equal(c$type, rep("two-sided", 10))
  expect_equal(a$alpha, rep(0.05, 10))
  expect_equal(b$alpha, rep(0.05, 10))
  expect_equal(c$alpha, rep(0.05, 10))
  expect_equal(a$reject, rep(FALSE, 10))
  expect_equal(b$reject, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                           TRUE, FALSE, FALSE))
  expect_equal(c$reject, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                           FALSE, FALSE))
  expect_snapshot_output(a)
  expect_snapshot_output(b)
  expect_snapshot_output(c)

  # try different setting
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 10)
  a <- counttest(models, alpha = 0.05, iteration = 3, one_sided = TRUE)
  b <- counttest(models, alpha = 0.01, iteration = 5, one_sided = FALSE)
  expect_equal(class(a), "data.frame")
  expect_equal(class(b), "data.frame")
  expect_equal(NROW(a), length(gammas))
  expect_equal(NROW(b), length(gammas))
  expect_equal(NCOL(a), 9)
  expect_equal(NCOL(b), 9)
  expect_equal(colnames(a), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(colnames(b), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(a$alpha, rep(0.05, 10))
  expect_equal(b$alpha, rep(0.01, 10))
  expect_equal(a$iter_test, rep(3, 10))
  expect_equal(b$iter_test, rep(5, 10))
  expect_equal(a$iter_act, rep(3, 10))
  expect_equal(b$iter_act, rep(5, 10))
  expect_equal(a$type, rep("one-sided", 10))
  expect_equal(b$type, rep("two-sided", 10))

  expect_snapshot_output(a)
  expect_snapshot_output(b)

  # try a single robust2sls_object instead of a list
  model <- outlier_detection(data = d, formula = p$setting$formula,
                             initial_est = "saturated", ref_dist = "normal",
                             sign_level = 0.05, iterations = 3, split = 0.5)
  a <- counttest(model, alpha = 0.1, iteration = 1, one_sided = FALSE)
  b <- counttest(model, alpha = 0.1, iteration = 1, one_sided = TRUE)
  expect_equal(NROW(a), 1)
  expect_equal(NROW(b), 1)
  expect_equal(NCOL(a), 9)
  expect_equal(NCOL(b), 9)
  expect_equal(colnames(a), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(colnames(b), c("iter_test", "iter_act", "gamma", "num_act",
                              "num_exp", "type", "pval", "alpha", "reject"))
  expect_equal(a$iter_test, 1)
  expect_equal(b$iter_test, 1)
  expect_equal(a$iter_act, 1)
  expect_equal(b$iter_act, 1)
  expect_equal(a$gamma, 0.05)
  expect_equal(b$gamma, 0.05)
  expect_equal(a$num_act, b$num_act) # should get same
  expect_equal(a$num_exp, b$num_exp) # should get same
  expect_equal(a$type, "two-sided")
  expect_equal(b$type, "one-sided")
  expect_equal(a$alpha, 0.1)
  expect_equal(b$alpha, 0.1)
  expect_equal(a$reject, FALSE)
  expect_equal(b$reject, FALSE)
  expect_snapshot_output(a)
  expect_snapshot_output(b)
  # re-build p-values manually
  num_act <- outliers(model, iteration = 1)
  num_exp <- model$cons$sign_level * NROW(d)
  pval2side <- stats::poisson.test(x = num_act, r = num_exp,
                                   alternative = "two.sided")$p.value
  pval1side <- stats::poisson.test(x = num_act, r = num_exp,
                                   alternative = "greater")$p.value
  expect_identical(a$pval, pval2side)
  expect_identical(b$pval, pval1side)

})

test_that("multi_cutoff_to_fodr_vec() raises correct errors", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.1, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = "convergence", convergence_criterion = 0,
                         max_iter = 20)
  model <- outlier_detection(data = d, formula = p$setting$formula,
                             ref_dist = "normal", iterations = 3,
                             sign_level = 0.01, initial_est = "robustified")

  expect_error(multi_cutoff_to_fodr_vec(c("a", "b"), 0),
               "'robust2sls_object' must be a list of 'robust2sls' objects")
  expect_error(multi_cutoff_to_fodr_vec(list(a = "a", b = "b"), 0),
               "'robust2sls_object' must be a list of 'robust2sls' objects")
  expect_error(multi_cutoff_to_fodr_vec(model, 0),
               "'robust2sls_object' must be a list of 'robust2sls' objects")
  expect_error(multi_cutoff_to_fodr_vec(models, "nope"),
               "'iteration' must be numeric or the string 'convergence'")
  expect_error(multi_cutoff_to_fodr_vec(models, TRUE),
               "'iteration' must be numeric or the string 'convergence'")
  expect_error(multi_cutoff_to_fodr_vec(models, 1.4),
               "'iteration' must be an integer >= 0 if numeric")
  expect_error(multi_cutoff_to_fodr_vec(models, -3),
               "'iteration' must be an integer >= 0 if numeric")

})

test_that("multi_cutoff_to_fodr_vec() works correctly", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.1, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = "convergence", convergence_criterion = 0,
                         max_iter = 20)

  # ensure we notice if the input changes, then may not be the problem of to_vec
  expect_snapshot_output(models)

  # test iteration 0
  a <- multi_cutoff_to_fodr_vec(models, iteration = 0)
  expect_snapshot_output(a)
  expect_length(a, length(models))
  expect_identical(class(a), "numeric")
  expect_named(a, c("gamma0.01", "gamma0.02", "gamma0.03", "gamma0.04",
                    "gamma0.05", "gamma0.06", "gamma0.07", "gamma0.08",
                    "gamma0.09", "gamma0.1"))

  # do some calculations manually
  # gamma 0.01
  share <- sum(models[[1]]$type$m0 == 0) / 1000
  res <- sqrt(1000) * (share - 0.01)
  names(res) <- "gamma0.01"
  expect_identical(a[1], res)
  # gamma 0.1
  share <- sum(models[[10]]$type$m0 == 0) / 1000
  res <- sqrt(1000) * (share - 0.1)
  names(res) <- "gamma0.1"
  expect_equal(a[10], res, tolerance = 0.0000000000001)

  # test iteration convergence
  a <- multi_cutoff_to_fodr_vec(models, iteration = "convergence")
  expect_snapshot_output(a)
  expect_length(a, length(models))
  expect_identical(class(a), "numeric")
  expect_named(a, c("gamma0.01", "gamma0.02", "gamma0.03", "gamma0.04",
                    "gamma0.05", "gamma0.06", "gamma0.07", "gamma0.08",
                    "gamma0.09", "gamma0.1"))

  # check whether correct iteration (final one) was used for convergence
  # gamma 0.01 converged at iteration 4
  share <- sum(models[[1]]$type$m4 == 0) / 1000
  res <- sqrt(1000) * (share - 0.01)
  names(res) <- "gamma0.01"
  expect_identical(a[1], res)
  # gamma 0.02 converged at iteration 3
  share <- sum(models[[2]]$type$m3 == 0) / 1000
  res <- sqrt(1000) * (share - 0.02)
  names(res) <- "gamma0.02"
  expect_identical(a[2], res)
  # gamma 0.1 did not converge, stopped at iteration 20
  share <- sum(models[[10]]$type$m20 == 0) / 1000
  res <- sqrt(1000) * (share - 0.1)
  names(res) <- "gamma0.1"
  expect_equal(a[10], res, tolerance = 0.00000000000001)

})

test_that("sumtest() raises correct errors", {

  expect_error(sumtest(robust2sls_object = 1, alpha = 0.05, iteration = 1,
                        one_sided = TRUE),
               "'robust2sls_object' must be a list of 'robust2sls' objects")
  expect_error(sumtest(robust2sls_object = list(1, 2), alpha = 0.05,
                        iteration = 1, one_sided = FALSE),
               "'robust2sls_object' must be a list of 'robust2sls' objects")

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.05, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 2)

  expect_error(sumtest(models, alpha = "a", iteration = 1, one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(sumtest(models, alpha = c(1,1), iteration = 1,
                        one_sided = TRUE),
               "'alpha' must be a numeric value of length one")
  expect_error(sumtest(models, alpha = 1.2, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(sumtest(models, alpha = -4, iteration = 1, one_sided = FALSE),
               "'alpha' must be between 0 and 1")
  expect_error(sumtest(models, alpha = 0.05, iteration = FALSE,
                        one_sided = FALSE),
               "'iteration' must either be a numeric")
  expect_error(sumtest(models, alpha = 0.05, iteration = "abc",
                        one_sided = FALSE),
               "'iteration' must either be a numeric or the string 'convergence'")
  expect_error(sumtest(models, alpha = 0.05, iteration = c(1, 2),
                        one_sided = FALSE),
               "'iteration' must be of length one")
  expect_error(sumtest(models, alpha = 0.05, iteration = 1.3,
                        one_sided = FALSE),
               "'iteration' must be an integer")
  expect_error(sumtest(models, alpha = 0.01, iteration = 1,
                        one_sided = "FALSE"),
               "'one_sided' must be a logical value of length one")
  expect_error(sumtest(models, alpha = 0.01, iteration = 1,
                        one_sided = c(TRUE, FALSE)),
               "'one_sided' must be a logical value of length one")

})

test_that("sumtest() works correctly", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  gammas <- seq(0.01, 0.05, 0.01)
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = 2)

  a <- sumtest(models, alpha = 0.05, iteration = 0, one_sided = FALSE)
  b <- sumtest(models, alpha = 0.05, iteration = 1, one_sided = TRUE)
  expect_snapshot_output(a)
  expect_snapshot_output(b)

  expect_equal(class(a), "data.frame")
  expect_equal(NROW(a), 1)
  expect_equal(NCOL(a), 6)
  expect_named(a, c("iter_test", "t", "type", "pval", "alpha", "reject"))
  expect_equal(a$iter_test, 0)
  expect_equal(a$type, "two-sided")
  expect_equal(a$alpha, 0.05)
  expect_equal(a$reject, TRUE)
  expect_equal(attr(a, "gammas"), seq(0.01, 0.05, 0.01))
  expect_equal(class(b), "data.frame")
  expect_equal(NROW(b), 1)
  expect_equal(NCOL(b), 6)
  expect_named(b, c("iter_test", "t", "type", "pval", "alpha", "reject"))
  expect_equal(b$iter_test, 1)
  expect_equal(b$type, "one-sided")
  expect_equal(b$alpha, 0.05)
  expect_equal(b$reject, FALSE)
  expect_equal(attr(b, "gammas"), seq(0.01, 0.05, 0.01))

  # test convergence iteration
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "robustified",
                         iterations = "convergence", convergence_criterion = 0,
                         max_iter = 20)
  # so that notice if input changes, might not be change in sumtest
  expect_snapshot_output(models)
  a <- sumtest(models, alpha = 0.05, iteration = "convergence", one_sided = FALSE)
  expect_snapshot_output(a)

  # 0.01 converges at 4, 0.02 at 3, 0.03 at 3, 0.04 at 3, 0.05 at 6
  # check whether value is correct
  pest <- estimate_param_null(models[[1]])
  val <- (0.004 + 0.013 + 0.021 + 0.026 + 0.032 - 0.01 - 0.02 - 0.03 - 0.04 - 0.05) * sqrt(1000)
  varcov <- matrix(NA, 5, 5)
  for (i in 1:5) {
    for (j in 1:5) {
      varcov[i, j] <- gauge_covar("normal", gammas[i], gammas[j], "robustified", "convergence", pest, split = NULL)
    }
  }
  t <- val/sqrt(sum(varcov))
  expect_equal(a$t, t, tolerance = 0.00000000000001)

  # check saturated 2sls as input
  models <- multi_cutoff(gamma = gammas, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "saturated",
                         iterations = 2, split = 0.3)
  expect_snapshot_output(models)
  c <- sumtest(models, alpha = 0.05, iteration = 0, one_sided = FALSE)
  expect_snapshot_output(c)
  expect_equal(class(c), "data.frame")
  expect_equal(NROW(c), 1)
  expect_equal(NCOL(c), 6)
  expect_named(c, c("iter_test", "t", "type", "pval", "alpha", "reject"))
  expect_equal(c$iter_test, 0)
  expect_equal(c$type, "two-sided")
  expect_equal(c$alpha, 0.05)
  expect_equal(c$reject, TRUE)
  expect_equal(attr(c, "gammas"), seq(0.01, 0.05, 0.01))

  # no theory for unequal split and m > 0, so should give error
  expect_error(sumtest(models, alpha = 0.05, iteration = 5))

  # when input a list of length 1, then get error -> doesn't make sense then
  models <- multi_cutoff(gamma = 0.01, data = d, formula = p$setting$formula,
                         ref_dist = "normal", initial_est = "saturated",
                         iterations = 2, split = 0.5)
  expect_error(sumtest(models, alpha = 0.05, iteration = 0, one_sided = FALSE),
               "requires several different cutoffs")

})
