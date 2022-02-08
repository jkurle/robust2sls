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
