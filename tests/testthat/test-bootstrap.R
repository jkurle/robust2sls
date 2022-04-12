# quick, initial unit testing to check broad implementation and detect if code breaks
# bootstrap functions still under development, so likely to change in the future

test_that("nonparametric() works as intended", {

  indices <- 1:100
  R <- 10

  expect_error(nonparametric(indices = indices, R = R, seed = "12"),
               "Argument 'seed' must either be NULL or an integer")
  expect_error(nonparametric(indices = indices, R = R, seed = TRUE),
               "Argument 'seed' must either be NULL or an integer")
  expect_error(nonparametric(indices = indices, R = R, seed = 1.3),
               "Argument 'seed' must either be NULL or an integer")

  set.seed(40)
  n1 <- nonparametric(indices = indices, R = R, replacement = TRUE, seed = NULL)
  n2 <- nonparametric(indices = indices, R = R, replacement = TRUE, seed = 40)
  expect_identical(n1, n2)
  expect_type(n1, "list")
  expect_identical(class(n1), "list")
  expect_identical(length(n1), 10L)
  listlengths <- sapply(X = n1, FUN = length)
  expect_identical(listlengths, rep(100L, times = 10))

  n3 <- nonparametric(indices = indices, R = R, replacement = FALSE, seed = NULL)
  expect_type(n3, "list")
  expect_identical(class(n3), "list")
  expect_identical(length(n3), 10L)
  listlengths <- sapply(X = n3, FUN = length)
  expect_identical(listlengths, rep(100L, times = 10))
  # without resampling, must be unique and with resample size = orig. size identical
  identicals <- sapply(X = n3, FUN = function(x) setequal(x, indices))
  expect_identical(identicals, rep(TRUE, times = 10))

})

test_that("count_indices() works as intended", {

  indices <- 1:10
  R <- 5
  n1 <- nonparametric(indices = indices, R = R, replacement = TRUE, seed = 10)
  # count_indices returns a list with two matrices ()
  c1 <- count_indices(resamples = n1, indices = indices) # all
  c2 <- count_indices(resamples = n1, indices = c(1, 3, 5)) # only subset
  # replacement FALSE
  n2 <- nonparametric(indices = indices, R = R, replacement = FALSE, seed = 10)
  c3 <- count_indices(resamples = n2, indices = indices) # all, should be 1 each

  expect_type(c1, "list")
  expect_type(c2, "list")
  expect_type(c3, "list")
  expect_identical(class(c1), "list")
  expect_identical(class(c2), "list")
  expect_identical(class(c3), "list")
  expect_length(c1, 2)
  expect_length(c2, 2)
  expect_length(c3, 2)
  expect_named(c1, c("count_all", "count_clean"))
  expect_named(c2, c("count_all", "count_clean"))
  expect_named(c3, c("count_all", "count_clean"))
  expect_type(c1$count_all, "integer")
  expect_type(c2$count_all, "integer")
  expect_type(c3$count_all, "integer")
  expect_identical(class(c1$count_all), c("matrix", "array"))
  expect_identical(class(c2$count_all), c("matrix", "array"))
  expect_identical(class(c3$count_all), c("matrix", "array"))
  expect_type(c1$count_clean, "integer")
  expect_type(c2$count_clean, "integer")
  expect_type(c3$count_clean, "integer")
  expect_identical(class(c1$count_clean), c("matrix", "array"))
  expect_identical(class(c2$count_clean), c("matrix", "array"))
  expect_identical(class(c3$count_clean), c("matrix", "array"))
  expect_identical(dim(c1$count_all), c(as.integer(R), length(indices)))
  expect_identical(dim(c2$count_all), c(as.integer(R), as.integer(max(c(1, 3, 5)))))
  expect_identical(dim(c3$count_all), c(as.integer(R), length(indices)))
  expect_identical(dim(c1$count_clean), c(as.integer(R), length(indices)))
  expect_identical(dim(c2$count_clean), c(as.integer(R), length(c(1, 3, 5))))
  expect_identical(dim(c3$count_clean), c(as.integer(R), length(indices)))

  # for no replacement, expect only ones
  matnorepl <- matrix(1L, nrow = R, ncol = length(indices))
  colnames(matnorepl) <- paste("o", 1:10, sep = "")
  expect_identical(c3$count_all, matnorepl)
  expect_identical(c3$count_clean, matnorepl)

  # snapshot to detect changes of input
  expect_snapshot_output(n1)
  expect_snapshot_output(n2)
  expect_snapshot_output(c1)
  expect_snapshot_output(c2)
  expect_snapshot_output(c3)

})

test_that("nonparametric_resampling() works correctly", {

  data <- mtcars
  resamples <- nonparametric(indices = 1:NROW(data), R = 5, replacement = TRUE,
                             seed = 1)
  # use these resamples of indices to actually get resample of mtcars
  df1 <- nonparametric_resampling(df = data, resample = resamples[[1]])

  expect_snapshot_output(df1)

  # do manual check of two observations
  # index 1 was drawn twice, this is Mazda RX4
  expect_true("Mazda RX4" %in% rownames(df1))
  expect_true("Mazda RX4.1" %in% rownames(df1))
  expect_true(!("Mazda RX4.2" %in% rownames(df1)))
  # index 3 was not drawn at all, this is Datsun 710
  expect_true(!("Datsun 710" %in% rownames(df1)))

})

test_that("case_resampling() works correctly", {

  library(future)
  library(parallel)
  library(doFuture)
  library(foreach)

  # setup
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  d <- generate_data(parameters = p, n = 1000)$data
  r <- outlier_detection(data = d, formula = p$setting$formula,
                         ref_dist = "normal", sign_level = 0.05,
                         initial_est = "robustified", iterations = 3)

  set.seed(10)
  cr1 <- case_resampling(robust2sls_object = r, R = 10)
  # only one iteration
  cr2 <- case_resampling(robust2sls_object = r, R = 10, m = 1)
  # only one coefficient by number
  cr3 <- case_resampling(robust2sls_object = r, R = 10, coef = 1, m = 1)
  # only one coefficient by name
  cr4 <- case_resampling(robust2sls_object = r, R = 10, coef = "x2", m = 1)

  # same ones but in parallel
  set.seed(10)
  ncores <- min(max(parallel::detectCores() - 1, 1), 2)
  doFuture::registerDoFuture()
  cl <- parallel::makeCluster(ncores)
  parallel::clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
  future::plan(future::cluster, workers = cl)
  cr11 <- case_resampling(robust2sls_object = r, R = 10, parallel = TRUE)
  # only one iteration
  cr21 <- case_resampling(robust2sls_object = r, R = 10, m = 1, parallel = TRUE)
  # only one coefficient by number
  cr31 <- case_resampling(robust2sls_object = r, R = 10, coef = 1, m = 1,
                          parallel = TRUE)
  # only one coefficient by name
  cr41 <- case_resampling(robust2sls_object = r, R = 10, coef = "x2", m = 1,
                          parallel = TRUE)
  future::plan(future::sequential)

  # first, ensure that same results whether parallel or not
  expect_identical(cr1, cr11)
  expect_identical(cr2, cr21)
  expect_identical(cr3, cr31)
  expect_identical(cr4, cr41)

  # check output structure
  expect_type(cr1, "list")
  expect_type(cr2, "list")
  expect_type(cr3, "list")
  expect_type(cr4, "list")
  expect_identical(class(cr1), "r2sls_boot")
  expect_identical(class(cr2), "r2sls_boot")
  expect_identical(class(cr3), "r2sls_boot")
  expect_identical(class(cr4), "r2sls_boot")
  expect_length(cr1, 3)
  expect_length(cr2, 3)
  expect_length(cr3, 3)
  expect_length(cr4, 3)
  expect_named(cr1, c("boot", "resamples", "original"))
  expect_named(cr2, c("boot", "resamples", "original"))
  expect_named(cr3, c("boot", "resamples", "original"))
  expect_named(cr4, c("boot", "resamples", "original"))
  expect_identical(class(cr1$boot), "data.frame")
  expect_identical(class(cr2$boot), "data.frame")
  expect_identical(class(cr3$boot), "data.frame")
  expect_identical(class(cr4$boot), "data.frame")
  expect_identical(NCOL(cr1$boot), 9L)
  expect_identical(NCOL(cr2$boot), 9L)
  expect_identical(NCOL(cr3$boot), 4L)
  expect_identical(NCOL(cr4$boot), 4L)
  expect_identical(NROW(cr1$boot), 44L)
  expect_identical(NROW(cr2$boot), 11L)
  expect_identical(NROW(cr3$boot), 11L)
  expect_identical(NROW(cr4$boot), 11L)
  expect_identical(colnames(cr1$boot), c("X.Intercept.", "x1", "x2", "x3", "x4", "x5", "m", "gauge", "r"))
  expect_identical(colnames(cr2$boot), c("X.Intercept.", "x1", "x2", "x3", "x4", "x5", "m", "gauge", "r"))
  expect_identical(colnames(cr3$boot), c("X.Intercept.", "m", "gauge", "r"))
  expect_identical(colnames(cr4$boot), c("x2", "m", "gauge", "r"))
  expect_type(cr1$resamples, "list")
  expect_type(cr2$resamples, "list")
  expect_type(cr3$resamples, "list")
  expect_type(cr4$resamples, "list")
  expect_identical(class(cr1$resamples), "list")
  expect_identical(class(cr2$resamples), "list")
  expect_identical(class(cr3$resamples), "list")
  expect_identical(class(cr4$resamples), "list")
  expect_length(cr1$resamples, 10L)
  expect_length(cr2$resamples, 10L)
  expect_length(cr3$resamples, 10L)
  expect_length(cr4$resamples, 10L)
  expect_identical(class(cr1$original), "robust2sls")
  expect_identical(class(cr2$original), "robust2sls")
  expect_identical(class(cr3$original), "robust2sls")
  expect_identical(class(cr4$original), "robust2sls")
  # $original saves original obj, so should be same across cr1, cr2, cr3, cr4
  expect_identical(cr1$original, cr2$original)
  expect_identical(cr1$original, cr3$original)
  expect_identical(cr1$original, cr4$original)

  # save as snapshot
  expect_snapshot_output(cr1)
  expect_snapshot_output(cr2)
  expect_snapshot_output(cr3)
  expect_snapshot_output(cr4)

  # need to check saturated and convergence (both parallel and not)
  set.seed(10)
  r <- outlier_detection(data = d, formula = p$setting$formula,
                         ref_dist = "normal", sign_level = 0.05,
                         initial_est = "saturated", iterations = "convergence",
                         convergence_criterion = 0.5, split = 0.5)
  cr5 <- case_resampling(robust2sls_object = r, R = 10, m = "convergence")

  expect_snapshot_output(cr5)

})
