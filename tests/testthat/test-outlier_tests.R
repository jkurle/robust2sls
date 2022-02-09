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
