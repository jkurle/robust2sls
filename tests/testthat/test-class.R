test_that("new_robust2sls() works correctly", {

  # create test lists and see whether class allocation works
  l1 <- list(a = "a", b = 1:5, c = datasets::mtcars)
  l2 <- list(a = "a", b = "b")
  structure(l2, class = "already")

  l1 <- new_robust2sls(l1)
  l2 <- new_robust2sls(l2)

  expect_s3_class(l1, "robust2sls")
  expect_s3_class(l2, "robust2sls")

})

test_that("validate_robust2sls() works correctly", {

  # check that valid classes are recognised as valid
  # since the validate_robust2sls() function returns the object, use snapshot
  # so this test will not only fail when validate_robust2sls() changes
  # but also when outlier_detection() or print.robust2sls() change

  # working values
  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  attr(formula, which = ".Environment") <- NULL

  test1 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = NULL, shuffle = FALSE,
            shuffle_seed = 42, split = 0.5)
  test2 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = 0, shuffle = NULL,
            shuffle_seed = NULL, split = NULL)
  test3 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 0.5,
            shuffle = TRUE, shuffle_seed = 42, split = 0.5)
  test4 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 1,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  test5 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = 0, convergence_criterion = NULL,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  test6 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 0, convergence_criterion = 0,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5)

  # check that the correct error messages are displayed
  t <- test1
  class(t) <- NULL
  expect_error(validate_robust2sls(t), "x is not of class 'robust2sls'")
  class(t) <- "notthisclass"
  expect_error(validate_robust2sls(t), "x is not of class 'robust2sls'")

  z <- 1:10
  class(z) <- "robust2sls"
  expect_error(validate_robust2sls(z), "Object must be a list")

  t <- z <- test1
  t$cons <- NULL
  z[["new"]] <- 1:10
  expect_error(validate_robust2sls(t), "Object must be a list with 6 elements")
  expect_error(validate_robust2sls(z), "Object must be a list with 6 elements")

  t <- z <- test1
  t$constants <- t$cons
  t$cons <- NULL
  names(z) <- c("", "", "", "", "", "")
  expect_error(validate_robust2sls(t), "Object must have 6 named components:")
  expect_error(validate_robust2sls(z), "Object must have 6 named components:")
  names(t) <- NULL
  expect_error(validate_robust2sls(t), "Object must have 6 named components:")

  t <- test1
  t$cons <- 1:5
  expect_error(validate_robust2sls(t), "Component \\$cons must be a list")

  t <- test1
  t$cons$call <- NULL
  expect_error(validate_robust2sls(t),
               "Component \\$cons must be a list with 11 elements")

  t <- z <- test1
  t$cons$ref <- t$cons$reference
  t$cons$reference <- NULL
  names(z$cons) <- NULL
  expect_error(validate_robust2sls(t),
               "Component \\$cons must have 11 named components:")
  expect_error(validate_robust2sls(z),
               "Component \\$cons must have 11 named components:")

  t <- test1
  t$cons$initial <- 1:10
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$initial must be a list")

  t <- z <- test1
  t$cons$initial$estimator <- NULL
  z$cons$initial$new <- 1:5
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$initial must be a list with 4 elements")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$initial must be a list with 4 elements")

  t <- z <- test1
  t$cons$initial$est <- t$cons$initial$estimator
  t$cons$initial$estimator <- NULL
  names(z$cons$initial) <- NULL
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$initial must have 4 named components:")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$initial must have 4 named components:")

  t <- test1
  t$cons$convergence <- 1:10
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$convergence must be a list")

  t <- z <- test1
  t$cons$convergence$criterion <- NULL
  z$cons$convergence$new <- 1:5
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$convergence must be a list with 3 elements")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$convergence must be a list with 3 elements")

  t <- z <- test1
  t$cons$convergence[["diff"]] <- "abc"
  t$cons$convergence$difference <- NULL
  names(z$cons$convergence) <- NULL
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$convergence must have 3 named components:")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$convergence must have 3 named components:")

  t <- test1
  t$cons$iterations <- c("abc", "def", "ghi")
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$iterations must be a list")

  t <- z <- test1
  t$cons$iterations$setting <- NULL
  z$cons$iterations$new <- "abc"
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$iterations must be a list with 2 elements")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$iterations must be a list with 2 elements")

  t <- z <- test1
  t$cons$iterations[["set"]] <- t$cons$iterations$setting
  t$cons$iterations$setting <- NULL
  names(z$cons$iterations) <- NULL
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$iterations must have 2 named components:")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$iterations must have 2 named components:")

  t <- test1
  t$model <- 1:3
  expect_error(validate_robust2sls(t), "Component \\$model must be a list")

  t <- test1
  t$res <- 1:3
  expect_error(validate_robust2sls(t), "Component \\$res must be a list")

  t <- test1
  t$stdres <- 1:3
  expect_error(validate_robust2sls(t), "Component \\$stdres must be a list")

  t <- test1
  t$sel <- 1:3
  expect_error(validate_robust2sls(t), "Component \\$sel must be a list")

  t <- test1
  t$type <- 1:3
  expect_error(validate_robust2sls(t), "Component \\$type must be a list")

  t <- test1
  t$model$m5 <- NULL
  expect_error(validate_robust2sls(t), "length of components \\$model, \\$res, \\$stdres, \\$sel, \\$type must be the same")

  t <- test1
  t$sel$m6 <- 1:5
  expect_error(validate_robust2sls(t), "length of components \\$model, \\$res, \\$stdres, \\$sel, \\$type must be the same")

  t <- test1
  t$cons$iterations$actual <- 4
  expect_error(validate_robust2sls(t), "actual iterations and number of elements in \\$model, \\$res etc. not consistent")

  t <- test1
  t$model$m6 <- t$model$m0
  t$model$m0 <- NULL
  expect_error(validate_robust2sls(t),
               "should have named components called 'm0', 'm1' etc.")

  t <- test1
  t$res$m6 <- t$res$m0
  t$res$m0 <- NULL
  expect_error(validate_robust2sls(t),
               "should have named components called 'm0', 'm1' etc.")

  t <- test1
  t$stdres$m6 <- t$stdres$m0
  t$stdres$m0 <- NULL
  expect_error(validate_robust2sls(t),
               "should have named components called 'm0', 'm1' etc.")

  t <- test1
  t$sel$m6 <- t$sel$m0
  t$sel$m0 <- NULL
  expect_error(validate_robust2sls(t),
               "should have named components called 'm0', 'm1' etc.")

  t <- test1
  t$type$m6 <- t$type$m0
  t$type$m0 <- NULL
  expect_error(validate_robust2sls(t),
               "should have named components called 'm0', 'm1' etc.")

  t <- test1
  t$cons$call <- 1
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$call must be a valid function call")

  t <- test1
  t$cons$formula <- 1
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$formula must be a valid formula")

  t <- test1
  t$cons$data <- as.matrix(t$cons$data)
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$data must be a data frame")

  t <- z <- test1
  t$cons$reference <- "nonexist"
  z$cons$reference <- c("normal", "normal")
  expect_error(validate_robust2sls(z),
               "\\$cons\\$reference must be a character vector of length 1")
  expect_error(validate_robust2sls(t),
               "\\$cons\\$reference must be one of the available reference distributions")

  t <- test1
  t$cons$sign_level <- "a"
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$sign_level must be numeric")

  t <- z <- test1
  t$cons$sign_level <- 1.3
  z$cons$sign_level <- -0.05
  expect_error(validate_robust2sls(t),
               "\\$cons\\$sign_level must be strictly between 0 and 1")
  expect_error(validate_robust2sls(z),
               "\\$cons\\$sign_level must be strictly between 0 and 1")

  t <- test1
  t$cons$psi <- "abc"
  expect_error(validate_robust2sls(t), "\\$cons\\$psi must be numeric")

  t <- z <- test1
  t$cons$psi <- 1.2
  z$cons$psi <- -0.01
  expect_error(validate_robust2sls(t),
               "Component \\$cons\\$psi must be strictly between 0 and 1")
  expect_error(validate_robust2sls(z),
               "Component \\$cons\\$psi must be strictly between 0 and 1")

  t <- test1
  t$cons$cutoff <- "1"
  expect_error(validate_robust2sls(t), "\\$cons\\$cutoff must be numeric")

  t <- z <- test1
  t$cons$bias_corr <- "1.3"
  z$cons$bias_corr <- 0.9
  expect_error(validate_robust2sls(t), "\\$cons\\$bias_corr must be numeric")
  expect_error(validate_robust2sls(z), "\\$cons\\$bias_corr must be > 1")

  t <- z <- test1
  t$cons$initial$estimator <- 1:3
  z$cons$initial$estimator <- c("abc", "def")
  expect_error(validate_robust2sls(t),
        "\\$cons\\$initial\\$estimator must be a character vector of length 1")
  expect_error(validate_robust2sls(z),
        "\\$cons\\$initial\\$estimator must be a character vector of length 1")

  t <- test1
  t$cons$initial$estimator <- "nonexist"
  expect_error(validate_robust2sls(t),
          "\\$cons\\$initial\\$estimator must be one of the available initial")

  t <- z <- w <- test1
  t$cons$initial$split <- 0.5
  z$cons$initial$shuffle <- TRUE
  w$cons$initial$shuffle_seed <- 42
  expect_error(validate_robust2sls(t), "\\$cons\\$initial\\$split should be NULL when the initial estimator is NOT 'saturated'")
  expect_error(validate_robust2sls(z), "\\$cons\\$initial\\$shuffle should be NULL when the initial estimator is NOT 'saturated'")
  expect_error(validate_robust2sls(w), "\\$cons\\$initial\\$shuffle_seed should be NULL when the initial estimator is NOT 'saturated'")

  t <- z <- w <- test3
  t$cons$initial$split <- "a"
  z$cons$initial$split <- 1.1
  w$cons$initial$split <- -0.5
  expect_error(validate_robust2sls(t), "\\$cons\\$initial\\$split must be numeric when the initial estimator is 'saturated'")
  expect_error(validate_robust2sls(z), "\\$cons\\$initial\\$split must be strictly between 0 and 1 when the initial estimator is 'saturated'")
  expect_error(validate_robust2sls(w), "\\$cons\\$initial\\$split must be strictly between 0 and 1 when the initial estimator is 'saturated'")

  t <- z <- test3
  t$cons$initial$shuffle <- 1
  z$cons$initial$shuffle <- c(TRUE, TRUE)
  expect_error(validate_robust2sls(t),
            "\\$cons\\$initial\\$shuffle must be a logical vector of length 1")
  expect_error(validate_robust2sls(z),
            "\\$cons\\$initial\\$shuffle must be a logical vector of length 1")

  t <- z <- test3
  t$cons$initial$shuffle_seed <- "a"
  z$cons$initial$shuffle_seed <- c(42, 24)
  expect_error(validate_robust2sls(t),
    "\\$cons\\$initial\\$shuffle_seed must be a numeric vector of length 1")
  expect_error(validate_robust2sls(z),
    "\\$cons\\$initial\\$shuffle_seed must be a numeric vector of length 1")

  t <- test4
  t$cons$initial$shuffle_seed <- 42
  expect_error(validate_robust2sls(t),
               "\\$cons\\$initial\\$shuffle_seed must be NULL")

  t <- test1
  t$cons$convergence$criterion <- "abc"
  expect_error(validate_robust2sls(t),
        "\\$cons\\$convergence\\$criterion must either be NULL or numeric")

  t <- test3
  t$cons$convergence$criterion <- -1
  z <- test1
  z$cons$iterations$setting <- "convergence"
  expect_error(validate_robust2sls(t),
               "\\$cons\\$convergence\\$criterion must be numeric >= 0")
  expect_error(validate_robust2sls(z),
               "\\$cons\\$convergence\\$criterion must be numeric >= 0")

  t <- z <- test3
  t$cons$convergence$difference <- "a"
  z$cons$convergence$difference <- -2
  expect_error(validate_robust2sls(t),
            "\\$cons\\$convergence\\$difference must be a numeric value >= 0")
  expect_error(validate_robust2sls(z),
            "\\$cons\\$convergence\\$difference must be a numeric value >= 0")
  z <- test1
  z$cons$iterations$setting <- "convergence"
  z$cons$convergence$criterion <- 2
  expect_error(validate_robust2sls(z),
            "\\$cons\\$convergence\\$difference must be a numeric value >= 0")

  t <- z <- test3
  t$cons$convergence$converged <- 1
  z$cons$convergence$converged <- c(TRUE, TRUE)
  expect_error(validate_robust2sls(t),
      "\\$cons\\$convergence\\$converged must be a logical vector of length 1")
  expect_error(validate_robust2sls(z),
      "\\$cons\\$convergence\\$converged must be a logical vector of length 1")
  z <- test1
  z$cons$iterations$setting <- "convergence"
  z$cons$convergence$criterion <- 2
  z$cons$convergence$difference <- 1
  expect_error(validate_robust2sls(z),
      "\\$cons\\$convergence\\$converged must be a logical vector of length 1")

  t <- z <- test1
  t$cons$convergence$difference <- 1
  expect_error(validate_robust2sls(t),
               "\\$cons\\$convergence\\$difference must be NULL")
  z$cons$convergence$converged <- "a"
  expect_error(validate_robust2sls(z),
               "\\$cons\\$convergence\\$converged must be NULL")

  t <- test1
  t$cons$iterations$setting <- "abc"
  expect_error(validate_robust2sls(t), "x\\$cons\\$iterations\\$setting must either be numeric or the character 'convergence'")

  t <- z <- test1
  t$cons$iterations$actual <- FALSE # triggers other error msg
  z$cons$iterations$actual <- -3 # triggers other error msg
  expect_error(validate_robust2sls(t))
  expect_error(validate_robust2sls(z))

  t <- test1
  t$cons$iterations$setting <- 3
  expect_error(validate_robust2sls(t),
               "Cannot have more actual iterations than was set")

  t <- test1
  t$cons$iterations$setting <- 7
  expect_error(validate_robust2sls(t),
               "Can only have fewer actual iterations than the numeric setting")

  t <- z <- w <- test3
  t$model$m0 <- "abc"
  z$model$m0$split1 <- NULL
  w$model$m0$split3 <- "additional element"
  expect_error(validate_robust2sls(t), "x\\$model\\$m0 has to be a list with 2 components when initial estimator is 'saturated'")
  expect_error(validate_robust2sls(z), "x\\$model\\$m0 has to be a list with 2 components when initial estimator is 'saturated'")
  expect_error(validate_robust2sls(w), "x\\$model\\$m0 has to be a list with 2 components when initial estimator is 'saturated'")

  t <- z <- test3
  names(z$model$m0) <- NULL
  t$model$m0$split3 <- t$model$m0$split2
  t$model$m0$split2 <- NULL
  expect_error(validate_robust2sls(t),
      "Component x\\$model\\$m0 must have two named components: split1 split2")
  expect_error(validate_robust2sls(z),
      "Component x\\$model\\$m0 must have two named components: split1 split2")

  t <- z <- test3
  class(t$model$m0$split1) <- "otherclass"
  z$model$m0$split2 <- "abc"
  expect_error(validate_robust2sls(t),
      "Components x\\$model\\$m0\\$split1 and x\\$model\\$m0\\$split2 must be of class 'ivreg'")
  expect_error(validate_robust2sls(z),
      "Components x\\$model\\$m0\\$split1 and x\\$model\\$m0\\$split2 must be of class 'ivreg'")

  t <- z <- test1
  class(t$model$m0) <- "otherclass"
  z$model$m0 <- "abc"
  expect_error(validate_robust2sls(t),
               "Element m0 of list \\$model must be of class 'ivreg'")
  expect_error(validate_robust2sls(z),
               "Element m0 of list \\$model must be of class 'ivreg'")

  t <- test1
  z <- test3
  class(t$model$m2) <- "otherclass"
  z$model$m3 <- "abc"
  expect_error(validate_robust2sls(t),
               "of list \\$model must be of class 'ivreg'")
  expect_error(validate_robust2sls(z),
               "of list \\$model must be of class 'ivreg'")

  t <- test1
  z <- test3
  t$res$m0 <- as.character(t$res$m0)
  z$res$m3 <- c(1, z$res$m3)
  expect_error(validate_robust2sls(t),
      "Element m0 of list \\$res must be a numeric vector with length equal to the number of observations")
  expect_error(validate_robust2sls(z),
      "Element m3 of list \\$res must be a numeric vector with length equal to the number of observations")

  t <- test1
  z <- test3
  t$stdres$m0 <- as.character(t$stdres$m0)
  z$stdres$m3 <- c(1, z$stdres$m3)
  expect_error(validate_robust2sls(t),
               "Element m0 of list \\$stdres must be a numeric vector with length equal to the number of observations")
  expect_error(validate_robust2sls(z),
               "Element m3 of list \\$stdres must be a numeric vector with length equal to the number of observations")

  t <- test1
  z <- test3
  t$sel$m0 <- as.character(t$sel$m0)
  z$sel$m3 <- c(1, z$sel$m3)
  expect_error(validate_robust2sls(t),
               "Element m0 of list \\$sel must be a logical vector with length equal to the number of observations")
  expect_error(validate_robust2sls(z),
               "Element m3 of list \\$sel must be a logical vector with length equal to the number of observations")

  t <- test1
  z <- test3
  t$type$m0 <- as.character(t$type$m0)
  z$type$m3 <- c(1, z$type$m3)
  expect_error(validate_robust2sls(t),
               "Element m0 of list \\$type must be a numeric vector with length equal to the number of observations")
  expect_error(validate_robust2sls(z),
               "Element m3 of list \\$type must be a numeric vector with length equal to the number of observations")

  t <- test1
  t$type$m0[[1]] <- 4
  expect_error(validate_robust2sls(t),
               "Element m0 of list \\$type must be a numeric vector that only contains the values -1, 0, or 1")

  #skip_on_ci()
  expect_snapshot(print(test1))
  expect_snapshot(print(test2))
  expect_snapshot(print(test3))
  expect_snapshot(print(test4))
  expect_snapshot(print(test5))
  expect_snapshot(print(test6))

})

test_that("print-robust2sls() works correctly", {

  # since this is all output, we use snapshot tests
  # when print method changes, all of these will probably fail but that's ok
  # have to check and then accept that all new output is okay

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt

  test1 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = NULL, shuffle = FALSE,
            shuffle_seed = 42, split = 0.5)
  test2 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 10, convergence_criterion = 3, shuffle = NULL,
            shuffle_seed = NULL, split = NULL)
  test3 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 0.5,
            shuffle = TRUE, shuffle_seed = 42, split = 0.5)
  test4 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 1,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  test5 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = 0, convergence_criterion = NULL,
            shuffle = FALSE, shuffle_seed = 42, split = 0.5)

  expect_snapshot(test1)
  expect_snapshot(test2)
  expect_snapshot(test3)
  expect_snapshot(test4)
  expect_snapshot(test5)

})

test_that("plot.robust2sls() works correctly", {

  # this is a plotting function, so we need to compare graphs
  # use expect_snapshot_file() from package 'testthat, edition 3'
  # helper function creating a file from code and returning a path
  save_png <- function(code, width = 1000, height = 600) {
    path <- tempfile(fileext = ".png")
    grDevices::png(path, width = width, height = height)
    on.exit(dev.off())
    code

    path
  }
  # models
  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt

  test1 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 5, convergence_criterion = NULL, shuffle = FALSE,
            shuffle_seed = 42, split = 0.5)
  test2 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
            iterations = 10, convergence_criterion = 3, shuffle = NULL,
            shuffle_seed = NULL, split = NULL)
  test3 <- outlier_detection(data = data, formula = formula,
            ref_dist = "normal", sign_level = 0.05, initial_est = "saturated",
            iterations = "convergence", convergence_criterion = 0.5,
            shuffle = TRUE, shuffle_seed = 42, split = 0.5)

  expect_snapshot_file(path = save_png(plot(test1)), name = "test1_default.png")
  expect_snapshot_file(path = save_png(plot(test1, iteration = 0)), name = "test1_m0.png")
  expect_snapshot_file(path = save_png(plot(test1, iteration = 4)), name = "test1_m4.png")
  expect_snapshot_file(path = save_png(plot(test2)), name = "test2_default.png")
  expect_snapshot_file(path = save_png(plot(test3)), name = "test3_default.png")

})
