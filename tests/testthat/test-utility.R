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

  expect_error(nonmissing(data = as.matrix(dta),
      formula = mpg ~ cyl + disp | cyl + wt), "must be a dataframe")
  expect_error(nonmissing(data = dta, formula = mpg ~ cyl + disp), "does not
               include both symbols `~` and `|`")
  expect_error(nonmissing(data = dta, formula = ~ cyl | disp), "does not
               include both symbols `~` and `|`")
  expect_error(nonmissing(data = dta, formula = nonexist ~ cyl + disp))

})

test_that("constants() works correctly", {

  # function to suppress output until outlier_detection has arg to turn off
  # progress messages
  hush=function(code){
    sink("NUL")
    tmp = code
    sink()
    return(tmp)
  }

  # working values
  data <- mtcars
  # since formula has an environment, whose memory address changes each time
  # it is run, it differs by snapshot. So here remove environment.
  formula <- mpg ~ cyl + disp | cyl + wt
  attr(formula, which = ".Environment") <- NULL
  test1 <- hush(outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = NULL, shuffle = FALSE,
            shuffle_seed = 42, split = 0.5))
  call1 <- sys.call()
  test2 <- hush(outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = 0, shuffle = NULL,
            shuffle_seed = NULL, split = NULL))
  call2 <- sys.call()
  test3 <- hush(outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 0.5,
            shuffle = TRUE, shuffle_seed = 42, split = 0.5))
  call3 <- sys.call()
  test4 <- hush(outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 1,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5))
  call4 <- sys.call()

  c1 <- constants(call = call1, formula = formula, data = data,
                  reference = "normal", sign_level = 0.05,
                  estimator = "robustified", split = 0.5, shuffle = FALSE,
                  shuffle_seed = 42, iter = 5, criterion = NULL)
  c2 <- constants(call = call2, formula = formula, data = data,
                  reference = "normal", sign_level = 0.05,
                  estimator = "robustified", split = NULL, shuffle = NULL,
                  shuffle_seed = NULL, iter = 5, criterion = 0)
  c3 <- constants(call = call3, formula = formula, data = data,
                  reference = "normal", sign_level = 0.05,
                  estimator = "saturated", split = 0.5, shuffle = TRUE,
                  shuffle_seed = 42, iter = 5, criterion = 0.5)
  c4 <- constants(call = call4, formula = formula, data = data,
                  reference = "normal", sign_level = 0.05,
                  estimator = "saturated", split = 0.5, shuffle = FALSE,
                  shuffle_seed = 42, iter = "convergence", criterion = 1)

  names <- c("call", "formula", "data", "reference", "sign_level", "psi",
             "cutoff", "bias_corr", "initial", "convergence", "iterations")

  expect_equal(length(c1), 11)
  expect_equal(length(c2), 11)
  expect_equal(length(c3), 11)
  expect_equal(length(c4), 11)
  expect_equal(class(c1), "list")
  expect_equal(class(c2), "list")
  expect_equal(class(c3), "list")
  expect_equal(class(c4), "list")
  expect_equal(names(c1), names)
  expect_equal(names(c2), names)
  expect_equal(names(c3), names)
  expect_equal(names(c4), names)
  expect_equal(length(c1$initial), 4)
  expect_equal(length(c2$initial), 4)
  expect_equal(length(c3$initial), 4)
  expect_equal(length(c4$initial), 4)
  expect_equal(class(c1$initial), "list")
  expect_equal(class(c2$initial), "list")
  expect_equal(class(c3$initial), "list")
  expect_equal(class(c4$initial), "list")
  expect_equal(names(c1$initial),
               c("estimator", "split", "shuffle", "shuffle_seed"))
  expect_equal(names(c2$initial),
               c("estimator", "split", "shuffle", "shuffle_seed"))
  expect_equal(names(c3$initial),
               c("estimator", "split", "shuffle", "shuffle_seed"))
  expect_equal(names(c4$initial),
               c("estimator", "split", "shuffle", "shuffle_seed"))
  expect_equal(length(c1$convergence), 3)
  expect_equal(length(c2$convergence), 3)
  expect_equal(length(c3$convergence), 3)
  expect_equal(length(c4$convergence), 3)
  expect_equal(class(c1$convergence), "list")
  expect_equal(class(c2$convergence), "list")
  expect_equal(class(c3$convergence), "list")
  expect_equal(class(c4$convergence), "list")
  expect_equal(names(c1$convergence), c("criterion", "difference", "converged"))
  expect_equal(names(c2$convergence), c("criterion", "difference", "converged"))
  expect_equal(names(c3$convergence), c("criterion", "difference", "converged"))
  expect_equal(names(c4$convergence), c("criterion", "difference", "converged"))
  expect_equal(length(c1$iterations), 2)
  expect_equal(length(c2$iterations), 2)
  expect_equal(length(c3$iterations), 2)
  expect_equal(length(c4$iterations), 2)
  expect_equal(class(c1$iterations), "list")
  expect_equal(class(c2$iterations), "list")
  expect_equal(class(c3$iterations), "list")
  expect_equal(class(c4$iterations), "list")
  expect_equal(names(c1$iterations), c("setting", "actual"))
  expect_equal(names(c2$iterations), c("setting", "actual"))
  expect_equal(names(c3$iterations), c("setting", "actual"))
  expect_equal(names(c4$iterations), c("setting", "actual"))

  expect_snapshot(c1)
  expect_snapshot(c2)
  expect_snapshot(c3)
  expect_snapshot(c4)

  expect_equal(c1$formula, formula)
  expect_equal(c2$formula, formula)
  expect_equal(c3$formula, formula)
  expect_equal(c4$formula, formula)

  expect_equal(c1$data, mtcars)
  expect_equal(c2$data, mtcars)
  expect_equal(c3$data, mtcars)
  expect_equal(c4$data, mtcars)

  expect_equal(c1$reference, "normal")
  expect_equal(c2$reference, "normal")
  expect_equal(c3$reference, "normal")
  expect_equal(c4$reference, "normal")

  expect_equal(c1$sign_level, 0.05)
  expect_equal(c2$sign_level, 0.05)
  expect_equal(c3$sign_level, 0.05)
  expect_equal(c4$sign_level, 0.05)

  expect_equal(c1$psi, 0.95)
  expect_equal(c2$psi, 0.95)
  expect_equal(c3$psi, 0.95)
  expect_equal(c4$psi, 0.95)

  expect_equal(c1$cutoff, 1.959964)
  expect_equal(c2$cutoff, 1.959964)
  expect_equal(c3$cutoff, 1.959964)
  expect_equal(c4$cutoff, 1.959964)

  expect_equal(c1$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(c2$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(c3$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(c4$bias_corr, 1.317798, tolerance = 0.0000001)

  expect_equal(c1$initial$estimator, "robustified")
  expect_equal(c2$initial$estimator, "robustified")
  expect_equal(c3$initial$estimator, "saturated")
  expect_equal(c4$initial$estimator, "saturated")

  expect_equal(c1$initial$split, NULL)
  expect_equal(c2$initial$split, NULL)
  expect_equal(c3$initial$split, 0.5)
  expect_equal(c4$initial$split, 0.5)

  expect_equal(c1$initial$shuffle, NULL)
  expect_equal(c2$initial$shuffle, NULL)
  expect_equal(c3$initial$shuffle, TRUE)
  expect_equal(c4$initial$shuffle, FALSE)

  expect_equal(c1$initial$shuffle_seed, NULL)
  expect_equal(c2$initial$shuffle_seed, NULL)
  expect_equal(c3$initial$shuffle_seed, 42)
  expect_equal(c4$initial$shuffle_seed, NULL)

  expect_equal(c1$iterations$setting, 5)
  expect_equal(c2$iterations$setting, 5)
  expect_equal(c3$iterations$setting, 5)
  expect_equal(c4$iterations$setting, "convergence")

  expect_equal(c1$iterations$actual, NULL)
  expect_equal(c2$iterations$actual, NULL)
  expect_equal(c3$iterations$actual, NULL)
  expect_equal(c4$iterations$actual, NULL)

  expect_equal(c1$convergence$difference, NULL)
  expect_equal(c2$convergence$difference, NULL)
  expect_equal(c3$convergence$difference, NULL)
  expect_equal(c4$convergence$difference, NULL)

  expect_equal(c1$convergence$converged, NULL)
  expect_equal(c2$convergence$converged, NULL)
  expect_equal(c3$convergence$converged, NULL)
  expect_equal(c4$convergence$converged, NULL)





})
