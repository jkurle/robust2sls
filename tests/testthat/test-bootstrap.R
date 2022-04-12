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
