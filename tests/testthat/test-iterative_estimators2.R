# new test file to find unit tests more easily

# testing iis_init in iterated algorithm

test_that("outlier_detection() with iis_init() works, fixed iterations", {

  skip_on_cran()

  # base setup
  set.seed(10)
  p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                      mean_z = 0, cov_z = matrix(1),
                      Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                      Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
  d <- generate_data(parameters = p, n = 50)$data
  formula <- y ~ -1+x1+x2 | -1+x1+z2
  gamma <- 0.05

  # iis arguments
  arglist1 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = NULL, weak = NULL)
  model1 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = gamma, initial_est = "iis",
                              user_model = NULL, iterations = 0,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)

  # same setup as previous testing (iis_init and selection_iis)
  # should have retained indicator for observation 19 in m = 0
  ## check overall structure
  expect_identical(class(model1), "robust2sls")
  expect_length(model1, 6)
  expect_named(model1, c("cons", "model", "res", "stdres", "sel", "type"))
  expect_type(model1, "list")
  iis1 <- ivgets::ivisat(formula = formula, data = d, iis = TRUE,
                         t.pval = gamma, wald.pval = arglist1$t.pval,
                         do.pet = arglist1$do.pet, overid = arglist1$overid,
                         weak = arglist1$weak, print.searchinfo = FALSE)
  res <- iis1$final$residuals
  stdres <- res / (iis1$final$sigma * sqrt(iis1$final$df.residual / iis1$final$nobs))
  sel <- rep(TRUE, times = NROW(d))
  sel[19] <- FALSE
  type <- rep(1L, times = NROW(d))
  type[19] <- 0L
  fullmodel <- ivreg::ivreg(formula = formula, data = d)
  fullmodel$call <- NULL # won't coincide exactly
  model1$model$m0$call <- NULL
  names(res) <- names(stdres) <- names(sel) <- names(type) <- as.character(1:50)
  expect_identical(model1$res$m0, res)
  expect_identical(model1$stdres$m0, stdres)
  expect_identical(model1$sel$m0, sel)
  expect_identical(model1$type$m0, type)
  expect_identical(model1$model$m0, fullmodel)

  # iterate fixed number of times
  model2 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = 0.05, initial_est = "iis",
                              user_model = NULL, iterations = 1,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  # will have to re-estimate model based on selection (i.e. leaving out obs 19)
  # do manually to check whether correct
  iterd <- d$selection <- model1$sel$m0
  iterm1 <- ivreg::ivreg(formula = formula, data = d, subset = selection)
  expect_identical(iterm1$nobs, 49L) # quick check that only 49 obs used in estim
  # check residuals, whether any outside cutoff
  res <- d[, "y"] - stats::predict(iterm1, newdata = d)
  expect_identical(length(res), 50L) # residual for all, even obs 19 that was not used in estimation
  psi <- 1 - gamma
  cutoff <- stats::qnorm(p = 1-(gamma/2), mean = 0, sd = 1)
  bias_corr <- 1/(((1-gamma)-2*cutoff*stats::dnorm(cutoff,mean=0,sd=1))/
                    (1-gamma))
  sigma <- iterm1$sigma * sqrt(iterm1$df.residual / iterm1$nobs) * sqrt(bias_corr)
  stdres <- res / sigma
  sel <- (abs(stdres) <= cutoff)
  type <- ifelse(sel == TRUE, 1L, 0L)
  names(res) <- names(stdres) <- names(sel) <- names(type) <- as.character(1:50)
  expect_identical(model2$res$m1, res)
  expect_identical(model2$stdres$m1, stdres)
  expect_identical(model2$sel$m1, sel)
  expect_identical(model2$type$m1, type)

  # iterate more often
  model3 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = 0.05, initial_est = "iis",
                              user_model = NULL, iterations = 10,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  expect_length(model3$res, 11)

  # snapshot outputs
  expect_snapshot_output(model1)
  expect_snapshot_output(model2)
  expect_snapshot_output(model3)

})

test_that("outlier_detection() with iis_init() works, convergence", {

  skip_on_cran()

  # base setup
  set.seed(10)
  p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                      mean_z = 0, cov_z = matrix(1),
                      Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                      Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
  d <- generate_data(parameters = p, n = 50)$data
  formula <- y ~ -1+x1+x2 | -1+x1+z2
  gamma <- 0.05

  # iis arguments
  arglist1 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = NULL, weak = NULL)

  # iterate until convergence, convergence_criterion 0
  model1 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = 0.05, initial_est = "iis",
                              user_model = NULL, iterations = "convergence",
                              convergence_criterion = 0, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  # converges at iteration 2 (technically already at iteration 1 but IIS stores full sample model m0)
  expect_length(model1$model, 3)
  expect_length(model1$res, 3)
  expect_length(model1$stdres, 3)
  expect_length(model1$sel, 3)
  expect_length(model1$type, 3)
  expect_identical(model1$cons$convergence$converged, TRUE)
  expect_identical(model1$cons$convergence$criterion, 0)
  expect_identical(model1$cons$convergence$difference, 0)
  expect_identical(model1$cons$iterations$setting, "convergence")
  expect_identical(model1$cons$iterations$actual, 2)

  # iterate until convergence, convergence_criterion 0.5
  model2 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = 0.05, initial_est = "iis",
                              user_model = NULL, iterations = "convergence",
                              convergence_criterion = 0.5, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  # converges at iteration 1 (technically already at iteration 0 but IIS stores full sample model m0)
  expect_length(model2$model, 2)
  expect_length(model2$res, 2)
  expect_length(model2$stdres, 2)
  expect_length(model2$sel, 2)
  expect_length(model2$type, 2)
  expect_identical(model2$cons$convergence$converged, TRUE)
  expect_identical(model2$cons$convergence$criterion, 0.5)
  expect_identical(model2$cons$iterations$setting, "convergence")
  expect_identical(model2$cons$iterations$actual, 1)
  # check whether was correct to stop
  difference <- sum((model2$model$m0$coefficients - model2$model$m1$coefficients)^2)
  expect_identical(model2$cons$convergence$difference, difference)

  # iterate 5 times but with convergence_criterion 0, so should stop at 2
  model3 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = 0.05, initial_est = "iis",
                              user_model = NULL, iterations = 5,
                              convergence_criterion = 0, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  # converges at iteration 2 (technically already at iteration 1 but IIS stores full sample model m0)
  expect_length(model3$model, 3)
  expect_length(model3$res, 3)
  expect_length(model3$stdres, 3)
  expect_length(model3$sel, 3)
  expect_length(model3$type, 3)
  expect_identical(model3$cons$convergence$converged, TRUE)
  expect_identical(model3$cons$convergence$criterion, 0)
  expect_identical(model3$cons$convergence$difference, 0)
  expect_identical(model3$cons$iterations$setting, 5)
  expect_identical(model3$cons$iterations$actual, 2)

})

test_that("outlier_detection() works with iis_init(), tests turned on", {

  skip_on_cran()

  # base setup
  set.seed(50)
  p <- generate_param(dx1 = 1, dx2 = 1, dz2 = 2, intercept = TRUE,
                      beta = c(2, 4), sigma = 1, mean_z = matrix(c(0,0), 2, 1),
                      cov_z = matrix(c(1,0,0,1), 2, 2),
                      Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                      Pi = t(matrix(c(1, 0, 0, 1, 0, 1), nrow = 2)))
  d <- generate_data(parameters = p, n = 50)$data
  formula <- y ~ -1+x1+x2 | -1+x1+z2+z3
  gamma <- 0.05

  # checked by stepping through function call that weak instrument test is actually applied

  # iis arguments, no testing
  arglist1 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = NULL, weak = NULL)
  model1 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = gamma, initial_est = "iis",
                              user_model = NULL, iterations = 5,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist1)
  # outliers: m0 = 5, m1 = 4, m2 = 2, m3 = 2, m4 = 2, m5 = 2
  # add weak instrument test, should not pass
  arglist2 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = NULL, weak = 0.00000000001)
  expect_warning(expect_error(outlier_detection(data = d, formula = formula, ref_dist = "normal",
                                 sign_level = gamma, initial_est = "iis",
                                 user_model = NULL, iterations = 5,
                                 convergence_criterion = NULL, max_iter = NULL,
                                 shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                                 verbose = FALSE, iis_args = arglist2),
               "IIS final model is NULL. See warning."))
  # add overid test, should not pass
  arglist3 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = 0.99, weak = NULL)
  expect_warning(expect_error(outlier_detection(data = d, formula = formula, ref_dist = "normal",
                                 sign_level = gamma, initial_est = "iis",
                                 user_model = NULL, iterations = 5,
                                 convergence_criterion = NULL, max_iter = NULL,
                                 shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                                 verbose = FALSE, iis_args = arglist3),
               "IIS final model is NULL. See warning."))
  # add both tests
  arglist4 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = 0.99, weak = 0.01)
  expect_warning(expect_error(outlier_detection(data = d, formula = formula, ref_dist = "normal",
                                                sign_level = gamma, initial_est = "iis",
                                                user_model = NULL, iterations = 5,
                                                convergence_criterion = NULL, max_iter = NULL,
                                                shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                                                verbose = FALSE, iis_args = arglist4),
                              "IIS final model is NULL. See warning."))

  # try the built-in tests from gets
  # normality test
  arglist5 <- list(t.pval = 0.01, do.pet = FALSE, normality.JarqueB = 0.05,
                   turbo = FALSE, overid = NULL, weak = NULL)
  model5 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = gamma, initial_est = "iis",
                              user_model = NULL, iterations = 5,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist5)
  # differs from model1 b/c of testing
  # one indicator retained in iter 0, and then no further outlier detected in iterations
  # check this manually
  manual5 <- ivgets::ivisat(formula = formula, data = d, iis = TRUE,
                            t.pval = gamma, normality.JarqueB = 0.05,
                            print.searchinfo = FALSE)
  # only second block is selected over, first one probably fails (is issue in gets)
  # one retained in second block (39)
  # check that diagnostics are present, normality test has been used
  expect_identical(rownames(manual5$selection$diagnostics),
                   c("Ljung-Box AR(1)", "Ljung-Box ARCH(1)", "Jarque-Bera"))
  sel5 <- rep(TRUE, times = 50)
  sel5[39] <- FALSE
  names(sel5) <- as.character(1:50)
  expect_identical(model5$sel$m0, sel5)

  # PET test
  arglist6 <- list(t.pval = 0.0000001, do.pet = TRUE, normality.JarqueB = NULL,
                   turbo = FALSE, overid = NULL, weak = NULL)
  model6 <- outlier_detection(data = d, formula = formula, ref_dist = "normal",
                              sign_level = gamma, initial_est = "iis",
                              user_model = NULL, iterations = 5,
                              convergence_criterion = NULL, max_iter = NULL,
                              shuffle = FALSE, shuffle_seed = NULL, split = 0.5,
                              verbose = FALSE, iis_args = arglist6)

  expect_snapshot_output(model1)
  expect_snapshot_output(model5)
  expect_snapshot_output(model6)

})















