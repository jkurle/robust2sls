test_that("generate_param() gives correct error messages", {

  skip_on_cran()

  # dx1
  expect_error(generate_param(dx1 = "1", dx2 = 2, dz2 = 2),
               "'dx1' must be numeric")
  expect_error(generate_param(dx1 = c(1,2), dx2 = 2, dz2 = 2),
               "'dx1' must have length 1")
  expect_error(generate_param(dx1 = -2, dx2 = 2, dz2 = 2),
               "'dx1' must be a single numeric integer value >= 0")
  expect_error(generate_param(dx1 = 1.5, dx2 = 2, dz2 = 2),
               "'dx1' must be a single numeric integer value >= 0")

  # dx2
  expect_error(generate_param(dx1 = 1, dx2 = "2", dz2 = 2),
               "'dx2' must be numeric")
  expect_error(generate_param(dx1 = 1, dx2 = c(2,3), dz2 = 2),
               "'dx2' must have length 1")
  expect_error(generate_param(dx1 = 1, dx2 = -2, dz2 = 2),
               "'dx2' must be a single numeric integer value >= 0")
  expect_error(generate_param(dx1 = 1, dx2 = 2.1, dz2 = 2),
               "'dx2' must be a single numeric integer value >= 0")

  # dz2
  expect_error(generate_param(dx1 = 1, dx2 = 2, dz2 = "2"),
               "'dz2' must be numeric")
  expect_error(generate_param(dx1 = 1, dx2 = 2, dz2 = c(2, 3)),
               "'dz2' must have length 1")
  expect_error(generate_param(dx1 = 1, dx2 = 2, dz2 = -2),
               "'dz2' must be a single numeric integer value >= 0")
  expect_error(generate_param(dx1 = 1, dx2 = 2, dz2 = 2.3),
               "'dz2' must be a single numeric integer value >= 0")

  # order condition
  expect_error(generate_param(dx1 = 3, dx2 = 2, dz2 = 1),
               "(order condition)")

  # intercept
  expect_error(generate_param(dx1 = 3, dx2 = 2, dz2 = 2, intercept = 1),
               "'intercept' must be a single logical value")
  expect_error(generate_param(dx1 = 3, dx2 = 2, dz2 = 2,
                              intercept = c(TRUE, FALSE)),
               "'intercept' must be a single logical value")

  # beta
  beta1 <- c("a", "b", "c")
  beta2 <- c(1, 2, 3, 4)
  expect_error(generate_param(3, 2, 2, beta = beta1),
               "'beta' must be a numeric vector")
  expect_error(generate_param(3, 2, 2, beta = beta2),
               "'beta' must have length dx1 \\+ dx2")

  # sigma
  sigma1 <- -1
  sigma2 <- "3"
  sigma3 <- c(1,2)
  expect_error(generate_param(3, 2, 2, sigma = sigma1),
               "'sigma' must be a numeric value strictly greater than zero")
  expect_error(generate_param(3, 2, 2, sigma = sigma2),
               "'sigma' must be a numeric value strictly greater than zero")
  expect_error(generate_param(3, 2, 2, sigma = sigma3),
               "'sigma' must have length 1")

  # mean_z
  mz1 <- c("a", "abc")
  mz2 <- c(1, 2, 3, 4, 5)
  mz3 <- c(1, 2, 3, 4)
  expect_error(generate_param(3, 2, 2, mean_z = mz1),
               "'mean_z' must either be NULL or a numeric vector")
  expect_error(generate_param(3, 2, 2, intercept = TRUE, mean_z = mz2),
               "must be dx1 \\+ dz2 \\- 1 when an intercept is included")
  expect_error(generate_param(3, 2, 2, intercept = FALSE, mean_z = mz3),
               "must be dx1 \\+ dz2 when no intercept is included")

  # cov_z
  cz1 <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)
  cz2 <- diag(3)
  cz3 <- diag(2)
  cz4 <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
  expect_error(generate_param(3, 2, 2, cov_z = cz1),
               "'cov_z' must be a numeric matrix")
  expect_error(generate_param(2, 1, 1, intercept = TRUE, cov_z = cz2),
               "square matrix with dimensions \\(dx1 \\+ dz2 \\- 1\\) when an intercept is included")
  expect_error(generate_param(2, 1, 1, intercept = FALSE, cov_z = cz3),
               "square matrix with dimensions \\(dx1 \\+ dz2\\) when no intercept is included")
  expect_error(generate_param(2, 1, 1, intercept = TRUE, cov_z = cz4),
               "'cov_z' must be positive definite")

  # Sigma2_half
  S1 <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)
  S2 <- diag(3)
  S3 <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
  expect_error(generate_param(3, 2, 2, Sigma2_half = S1),
               "'Sigma2_half' must be a numeric matrix")
  expect_error(generate_param(3, 2, 2, Sigma2_half = S2),
               "square matrix with dimensions dx2")
  expect_error(generate_param(3, 2, 2, Sigma2_half = S3),
               "'Sigma2_half' must be positive definite")

  # Omega2
  O1 <- c("a", "b", "c")
  O2 <- matrix(c(1, 2, 3), ncol = 1, nrow = 3)
  O3 <- c(1, 2, 3)
  expect_error(generate_param(3, 2, 2, Omega2 = O1),
               "'Omega' must be a numeric vector")
  expect_error(generate_param(3, 2, 2, Omega2 = O2),
               "'Omega' must have dimensions dx2 by 1")
  expect_error(generate_param(3, 2, 2, Omega2 = O3),
               "'Omega' must have dimensions dx2 by 1")

  # Pi
  Pi1 <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)
  Pi2 <- diag(3)
  Pi3 <- diag(2)
  Pi3[2,2] <- 0
  # create a Pi matrix that fulfills all conditions except identity in corner
  # create Pi via generate_param() as square matrix, then transpose it
  # has correct dimensions, is numeric, but not identity matrix in dx1 corner
  p <- generate_param(2, 2, 2)
  Pi4 <- p$params$Pi
  Pi4_1 <- Pi4[1:4, 1:2]
  Pi4_2 <- Pi4[1:4, 3:4]
  Pi4 <- cbind(Pi4_2, Pi4_1)

  expect_error(generate_param(3, 2, 2, Pi = Pi1),
               "'Pi' must be a numeric matrix")
  expect_error(generate_param(1, 1, 1, Pi = Pi2),
               "'Pi' must have dimensions dz by dx")
  expect_error(generate_param(1, 1, 1, Pi = Pi3),
               "'Pi' must have full rank, i\\.e\\. dx1\\+dx2")
  expect_error(generate_param(2, 2, 2, Pi = Pi4),
               "dx1 by dx1 submatrix of Pi in the upper left corner has to be the identity matrix")

})

test_that("generate_param() works correctly", {

  skip_on_cran()

  # expect no error for:
  p <- generate_param(3, 2, 3)
  expect_silent(generate_param(3, 2, 3, beta = p$params$beta))
  expect_silent(generate_param(3, 2, 3, sigma = p$params$sigma))
  expect_silent(generate_param(3, 2, 3, mean_z = p$params$mean_z))
  expect_silent(generate_param(3, 2, 3, cov_z = p$params$cov_z))
  expect_silent(generate_param(3, 2, 3, Sigma2_half = p$params$Sigma2_half))
  expect_silent(generate_param(3, 2, 3, Omega2 = p$params$Omega2))
  expect_silent(generate_param(3, 2, 3, Pi = p$params$Pi))

  # output coincides with the original function param_gen_2SLS()
  environment(p$setting$formula) <- NULL
  expect_snapshot_output(p)

  # expect no error for: (calling no intercept)
  p <- generate_param(3, 2, 3, intercept = FALSE)
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, beta = p$params$beta))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, sigma = p$params$sigma))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, mean_z = p$params$mean_z))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, cov_z = p$params$cov_z))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, Sigma2_half = p$params$Sigma2_half))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, Omega2 = p$params$Omega2))
  expect_silent(generate_param(3, 2, 3, intercept = FALSE, Pi = p$params$Pi))
  environment(p$setting$formula) <- NULL
  expect_snapshot_output(p)

})


test_that("generate_data() works correctly", {

  skip_on_cran()

  # checked that this replicates the output from original data_gen_2SLS() fun
  p1 <- generate_param(3, 2, 3, seed = 42)
  set.seed(42)
  d1 <- generate_data(p1, n = 30)
  expect_snapshot_output(d1)

  # check that in the sample, the 2SLS model assumptions are approx. fulfilled
  p1 <- generate_param(3, 2, 3, seed = 42)
  set.seed(42)
  d1 <- generate_data(p1, n = 500000)

  expect_equal(mean(d1$data[, "u"]), -0.0000894726,
               tolerance = 0.000001) # close to 0
  expect_equal(mean(d1$data[, "r4"]), -0.0021195908) # close to 0
  expect_equal(mean(d1$data[, "r5"]), -0.0000048737986) # close to 0
  expect_equal(sd(d1$data[, "u"]), 0.99979182) # close to 1

  structural_cov <- p1$structural$cov
  dimnames(structural_cov) <- list(
    c("u", "r4", "r5", "x2", "x3", "z4", "z5", "z6"),
    c("u", "r4", "r5", "x2", "x3", "z4", "z5", "z6"))

  expect_equal(var(d1$data[, c("u", "r4", "r5", "x2", "x3", "z4", "z5", "z6")]),
               structural_cov, tolerance = 0.01)

  expect_equal(mean(d1$data[, "u"]), p1$structural$mean[[1]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "r4"]), p1$structural$mean[[2]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "r5"]), p1$structural$mean[[3]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "x2"]), p1$structural$mean[[4]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "x3"]), p1$structural$mean[[5]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "z4"]), p1$structural$mean[[6]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "z5"]), p1$structural$mean[[7]], tolerance = 0.01)
  expect_equal(mean(d1$data[, "z6"]), p1$structural$mean[[8]], tolerance = 0.01)

  # check that ivreg() recovers beta approximately
  model <- ivreg::ivreg(y ~ -1 + x1 + x2 + x3 + x4 + x5 | x1 + x2 + x3 + z4 + z5 + z6, data = d1$data)
  coef <- model$coefficients
  names(coef) <- NULL
  coef <- matrix(coef, 5, 1)
  expect_equal(coef, d1$beta, tolerance = 0.01)

})

test_that("mc_grid() throws correct error", {

  skip_on_cran() # probably too long and might have problems with parallel
  p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)

  # check error from invalid input "iterations"
  expect_error(mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                       formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = "nonexist", convergence_criterion = 0),
               "Argument iterations not correctly specified.")

  # error invalid path
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  future::plan(future::sequential)
  expect_error(mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                       formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = 3, convergence_criterion = 0, path = "test/"),
               "Argument 'path' should not end with a path separator")
  expect_error(mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                       formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = 3, convergence_criterion = 0, path = "test\\"),
               "Argument 'path' should not end with a path separator")
  expect_error(mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                       formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = "convergence", convergence_criterion = 0, path = "test/"),
               "Argument 'path' should not end with a path separator")
  expect_error(mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                       formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = "convergence", convergence_criterion = 0, path = "test\\"),
               "Argument 'path' should not end with a path separator")

})

test_that("mc_grid() works correctly", {

  skip_on_cran() # probably too long and might have problems with parallel
  # skip_on_ci() # causes trouble on Windows server
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  cl <- parallel::makeCluster(ncores)
  parallel::clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
  future::plan(future::cluster, workers = cl)
  results <- mc_grid(100, n = c(100, 1000), seed = 42, parameters = p,
                              formula = p$setting$formula, ref_dist = "normal",
                              sign_level = c(0.01, 0.05),
                              initial_est = c("saturated", "robustified"),
                              iterations = 0, shuffle = FALSE,
                              shuffle_seed = NULL, split = c(0.3, 0.4, 0.5))

  expect_snapshot_output(results) # checked with manual original simulations
  parallel::stopCluster(cl)

})

test_that("mc_grid() works correctly with convergence setting", {

  skip_on_cran() # probably too long and might have problems with parallel
  p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)

  # know the values because tested the settings before in a separate file
  # convergence without max_iter
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  cl <- parallel::makeCluster(ncores)
  parallel::clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
  future::plan(future::cluster, workers = cl)
  out <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                 formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                 sign_level = 0.05, initial_est = "robustified",
                 iterations = "convergence", convergence_criterion = 0)
  parallel::stopCluster(cl)

  outfreq <- list(list("2" = 1L, "3" = 3L, "4" = 3L, "5" = 1L, "8" = 1L, "9" = 1L),
                  list("5" = 4L, "6" = 2L, "7" = 1L, "8" = 1L, "10" = 1L, "15" = 1L))
  class(outfreq) <- "AsIs"
  expect_equal(NROW(out), 2)
  expect_equal(class(out$conv_freq), "AsIs")
  expect_equal(out$conv_freq, outfreq)
  expect_equal(out$max, c("NULL", "NULL"))

  # convergence with max_iter
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  cl <- parallel::makeCluster(ncores)
  parallel::clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
  future::plan(future::cluster, workers = cl)
  out2 <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
                  formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
                  sign_level = 0.05, initial_est = "robustified",
                  iterations = "convergence", convergence_criterion = 0,
                  max_iter = 5)
  parallel::stopCluster(cl)

  outfreq2 <- list(list("2" = 1L, "3" = 3L, "4" = 3L, "5" = 3L),
                   list("5" = 10L))
  class(outfreq2) <- "AsIs"
  expect_equal(NROW(out2), 2)
  expect_equal(class(out2$conv_freq), "AsIs")
  expect_equal(out2$conv_freq, outfreq2)
  expect_equal(out2$max, c(5, 5))

  expect_snapshot_output(out)
  expect_snapshot_output(out2)

})

test_that("mc_grid() prints correct output when verbose = TRUE", {

  skip_on_cran() # probably too long and might have problems with parallel
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  future::plan(future::sequential)

  # iterations fixed setting
  expect_output(mc_grid(10, n = c(100, 1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01, 0.05),
                        initial_est = "robustified",
                        iterations = 0, shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "Total number of Monte Carlo experiments:")
  expect_output(mc_grid(10, n = c(100, 1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01, 0.05),
                        initial_est = "robustified",
                        iterations = 0, shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "Monte Carlo experiment:")
  expect_output(mc_grid(10, n = c(100, 1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01, 0.05),
                        initial_est = "robustified",
                        iterations = 0, shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "user")

  # convergence setting
  expect_output(mc_grid(10, n = c(1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01),
                        initial_est = "robustified",
                        iterations = "convergence", convergence_criterion = 3,
                        shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "Total number of Monte Carlo experiments:")
  expect_output(mc_grid(10, n = c(1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01),
                        initial_est = "robustified",
                        iterations = "convergence", convergence_criterion = 3,
                        shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "Monte Carlo experiment:")
  expect_output(mc_grid(10, n = c(1000), seed = 42, parameters = p,
                        formula = p$setting$formula, ref_dist = "normal",
                        sign_level = c(0.01),
                        initial_est = "robustified",
                        iterations = "convergence", convergence_criterion = 3,
                        shuffle = FALSE,
                        shuffle_seed = NULL, split = 0.5, verbose = TRUE),
                "user")

})

test_that("mc_grid() saves intermediate results correctly", {

  skip_on_ci() # runs locally, sometimes causes problems on GitHub Actions
  skip_on_cran() # probably too long and might have problems with parallel
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  future::plan(future::sequential)

  pth <- NULL
  save_file <- function(code) {
    directory <- tempdir()
    pth <<- directory
    path <- file.path(directory, "M10n1000g0.01irobustifieds0.5.csv")
    code
    return(path)
  }

  expect_snapshot_file(path = save_file(mc_grid(10, n = c(1000), seed = 42, parameters = p,
                                                  formula = p$setting$formula, ref_dist = "normal",
                                                  sign_level = c(0.01), path = pth,
                                                  initial_est = "robustified",
                                                  iterations = 0, shuffle = FALSE,
                                                  shuffle_seed = NULL, split = 0.5)),
                       name = "iter0.csv")

  pth <- NULL
  save_file <- function(code) {
    directory <- tempdir()
    pth <<- directory
    path <- file.path(directory, "M10n1000g0.01irobustifieds0.5.csv")
    code
    return(path)
  }

  expect_snapshot_file(path = save_file(mc_grid(10, n = c(1000), seed = 42, parameters = p,
                                                formula = p$setting$formula, ref_dist = "normal",
                                                sign_level = c(0.01), path = pth,
                                                initial_est = "robustified",
                                                iterations = "convergence", convergence_criterion = 3,
                                                shuffle = FALSE, shuffle_seed = NULL, split = 0.5)),
                       name = "iterconv.csv")

})

test_that("CI::mc_grid() saves intermediate results correctly", {

  # this is to have coverage of the test on CI
  # but this is really tested locally
  # expect_silent to check no error or warnings raised
  skip_on_cran() # probably too long and might have problems with parallel
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  future::plan(future::sequential)

  expect_silent(a <- mc_grid(10, n = c(1000), seed = 42, parameters = p,
                formula = p$setting$formula, ref_dist = "normal",
                sign_level = c(0.01), path = tempdir(),
                initial_est = "robustified",
                iterations = 0, shuffle = FALSE,
                shuffle_seed = NULL, split = 0.5))

  expect_silent(a <- mc_grid(10, n = c(1000), seed = 42, parameters = p,
                formula = p$setting$formula, ref_dist = "normal",
                sign_level = c(0.01), path = tempdir(),
                initial_est = "robustified",
                iterations = "convergence", convergence_criterion = 3,
                shuffle = FALSE, shuffle_seed = NULL, split = 0.5))

})
