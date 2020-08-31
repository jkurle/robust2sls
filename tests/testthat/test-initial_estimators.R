
test_that("robustified_init() works correctly", {

  data <- mtcars
  data[1, "mpg"] <- NA # y missing
  data[2, "cyl"] <- NA # x1 missing
  data[3, "disp"] <- NA # x2 missing
  data[4, "wt"] <- NA # z2 missing
  data[5, "hp"] <- NA # not relevant for estimation missing
  formula <- mpg ~ cyl + disp | cyl + wt
  shuffle_seed <- 42

  r1 <- robustified_init(data = data, formula = formula, cutoff = 1.96)

  expect_length(r1, 4)
  expect_length(r1$res, 32)
  expect_length(r1$stdres, 32)
  expect_length(r1$sel, 32)
  expect_length(r1$type, 32)

  expect_type(r1$res, "double")
  expect_type(r1$stdres, "double")
  expect_type(r1$sel, "logical")
  expect_type(r1$type, "integer")

  expect_equal(r1$res[1], as.double(NA))
  expect_equal(r1$res[2], as.double(NA))
  expect_equal(r1$res[3], as.double(NA))
  expect_equal(r1$res[4], as.double(NA))




})
