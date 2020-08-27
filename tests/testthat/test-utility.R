test_that("extract_formula() works correctly", {

  f1 <- y ~ x1 + x2 | x1 + z2 + z3
  v1 <- extract_formula(f1)

  expect_equal(length(v1),5)
  expect_equal(length(v1$y_var), 1)
  expect_equal(length(v1$x1_var), 1)
  expect_equal(length(v1$x2_var), 1)
  expect_equal(length(v1$z1_var), 1)
  expect_equal(length(v1$z2_var), 2)
  expect_equal(v1$y_var, "y")
  expect_equal(v1$x1_var, "x1")
  expect_equal(v1$x2_var, "x2")
  expect_equal(v1$z1_var, "x1")
  expect_equal(v1$z2_var, c("z2", "z3"))

  expect_error(extract_formula(y ~ x1 + x2 + z2), "does not include both symbols
    `~` and `|`")
  expect_error(extract_formula(y ~ z1 + z2), "does not include both symbols
    `~` and `|`")
  expect_error(extract_formula(~ x1 + x2 | x1 + z2), "any dependent variable")
  # expect_error(extract_formula(y ~ | z1 + z2)) # this is not possible anyways
  # expect_error(extract_formula(y ~ x1 + x2 |)) # this is not possible anyways

  f2 <- mpg ~ cyl * disp | cyl + wt + gear
  v2 <- extract_formula(f2)

  expect_equal(v2$y_var, "mpg")
  expect_equal(v2$x1_var, "cyl")
  expect_equal(v2$x2_var, "disp")
  expect_equal(v2$z1_var, "cyl")
  expect_equal(v2$z2_var, c("wt", "gear"))

  f3 <- y ~ x1 + x2 + x3 | x1 + z2
  expect_error(extract_formula(f3), "does not fulfill the order condition")

})
