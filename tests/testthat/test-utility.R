library(AER)

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

test_that("selection() works correctly", {
  dta <- mtcars
  dta[1, "mpg"] <- NA
  dta[2, "cyl"] <- NA
  dta[3, "wt"] <- NA
  test <- AER::ivreg(mpg ~ cyl + disp | cyl + wt, data = dta)

  expect_equal(NROW(dta), 32) # 32 observations
  expect_equal(NROW(test$residuals), 29) # 29 obs due to NA values in y, x, z

  sel <- selection(data = dta, yvar = "mpg", model = test, cutoff = 1.96)
  res1 <- res2 <- res3 <- as.double(NA)
  names(res1) <- "Mazda RX4"
  names(res2) <- "Mazda RX4 Wag"
  names(res3) <- "Datsun 710"

  expect_equal(NROW(sel[["res"]]), 32) # 32 when done manually inside res_all()
  # check that these residuals with missing y, x, z are NA
  expect_equal(sel[["res"]][1], res1)
  expect_equal(sel[["res"]][2], res2)
  expect_equal(sel[["res"]][3], res3)
  # check that the model residuals coincide with the res_all(), ignoring NAs
  expect_equal(sel[["res"]][c(-1,-2,-3)], test$residuals)

  # standardised residuals of first three should also be NA
  expect_equal(sel[["stdres"]][1], res1)
  expect_equal(sel[["stdres"]][2], res2)
  expect_equal(sel[["stdres"]][3], res3)

  # in selection vector obs with missing values should be set to FALSE
  sel1 <- sel2 <- sel3 <- FALSE
  names(sel1) <- "Mazda RX4"
  names(sel2) <- "Mazda RX4 Wag"
  names(sel3) <- "Datsun 710"
  expect_equal(sel[["sel"]][1], sel1)
  expect_equal(sel[["sel"]][2], sel2)
  expect_equal(sel[["sel"]][3], sel3)

  # by inspection, only one observation has an absolute residual > 1.96
  # this is "Pontiac Firebird", observation 25 in the original dataframe
  sel25 <- FALSE
  names(sel25) <- "Pontiac Firebird"
  expect_equal(sum(sel[["sel"]]), 28) # 28 because 3 missing and one outlier
  expect_equal(sel[["sel"]][c(1, 2, 3, 25)], c(sel1, sel2, sel3, sel25))
  expect_equal(as.vector(sel[["sel"]][c(-1, -2, -3, -25)]),
               !logical(length = 28)) # strip the names attribute via as.vector

  # check that type has the correct coding
  expect_equal(typeof(sel[["type"]]), "integer")
  expect_equal(as.vector(sel[["type"]][1]), -1)
  expect_equal(as.vector(sel[["type"]][2]), -1)
  expect_equal(as.vector(sel[["type"]][3]), -1)
  expect_equal(as.vector(sel[["type"]][4]), 1)
  expect_equal(as.vector(sel[["type"]][25]), 0)
  expect_equal(as.vector(sel[["type"]][c(1,2,3)]), c(-1, -1, -1))

  test <- AER::ivreg(mpg ~ cyl + disp | cyl + wt, data = mtcars)
  sel <- selection(data = mtcars, yvar = "mpg", model = test, cutoff = 1.96)
  expect_equal(sel[["res"]], test$residuals)

  lmmodel <- stats::lm(mpg ~ cyl + disp, data = mtcars)
  expect_error(selection(data = mtcars, yvar = "mpg", model = lmmodel,
                         cutoff = 1.96), "not of class `ivreg`")
  expect_error(selection(data = mtcars, yvar = 1, model = test, cutoff = 1.96),
               "not a character vector")
  expect_error(selection(data = mtcars, yvar = "notexist", model = test,
                         cutoff = 1.96), "not a variable in the dataframe")




})

test_that("nonmissing() works correctly", {

  expect_equal(nonmissing(data = mtcars, formula = mpg ~ cyl + disp | cyl + wt),
               !logical(length = 32))

  dta <- mtcars
  dta[1, "mpg"] <- NA
  dta[2, "cyl"] <- NA
  dta[3, "wt"] <- NA
  dta[4:5, "disp"] <- NA
  dta[15, "gear"] <- NA # gear NA in obs 15 but not used in estimation
  dta[16, "qsec"] <- NA # qsec NA in obs 16 but not used in estimation

  nm <- !logical(length = 32)
  nm[1:5] <- FALSE
  expect_equal(nonmissing(data = dta, formula = mpg ~ cyl + disp | cyl + wt),
               nm)

})

test_that("constants() works correctly", {

  expect_error(constants(sign_level = 0.05, reference = "notexist"))
  expect_error(constants(sign_level = -0.01, reference = "normal"),
               "has to be > 0 and < 1")
  expect_error(constants(sign_level = 1, reference = "normal"),
               "has to be > 0 and < 1")
  expect_error(constants(sign_level = "a", reference = "normal"))

  expect_equal(constants(sign_level = 0.05, reference = "normal")$cutoff, 1.96,
               tolerance = 0.01)

})
