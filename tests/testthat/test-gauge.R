test_that("outliers() works correctly", {

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  data[1, "mpg"] <- NA
  data[2, "cyl"] <- NA
  data[3, "disp"] <- NA
  data[4, "wt"] <- NA

  model1 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = 3)

  expect_equal(outliers(model1, 0), 1)
  expect_equal(outliers(model1, 1), 1)
  expect_equal(outliers(model1, 2), 1)
  expect_equal(outliers(model1, 3), 1)

  model2 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = 3, shuffle = TRUE,
              shuffle_seed = 42, split = 0.5)

  expect_equal(outliers(model2, 0), 16)
  expect_equal(outliers(model2, 1), 12)
  expect_equal(outliers(model2, 2), 12)
  expect_equal(outliers(model2, 3), 12)

  model3 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = "convergence",
              convergence_criterion = 1)

  expect_equal(outliers(model3, 0), 1)
  expect_equal(outliers(model3, 1), 1)
  expect_error(outliers(model3, 2), "fewer iterations than argument")

  model4 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = "convergence",
              convergence_criterion = 0.1)

  expect_equal(outliers(model4, 0), 1)
  expect_equal(outliers(model4, 1), 1)
  expect_equal(outliers(model4, 2), 1)
  expect_error(outliers(model4, 3), "fewer iterations than argument")

  model5 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 42, split = 0.5)

  expect_equal(outliers(model5, 0), 16)
  expect_equal(outliers(model5, 1), 12)
  expect_equal(outliers(model5, 2), 12)
  expect_equal(outliers(model5, 3), 12)
  expect_error(outliers(model5, 4), "fewer iterations than argument")

  model6 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 24, split = 0.5)

  expect_equal(outliers(model6, 0), 2)
  expect_equal(outliers(model6, 1), 1)
  expect_equal(outliers(model6, 2), 1)
  expect_equal(outliers(model6, 3), 1)
  expect_error(outliers(model6, 4), "fewer iterations than argument")

  model7 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.01,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 24, split = 0.5)

  expect_equal(outliers(model7, 0), 0)
  expect_equal(outliers(model7, 1), 0)
  expect_equal(outliers(model7, 2), 0)

  # test input error messages

  expect_error(outliers(model1, 1.3), "has to be an integer")
  expect_error(outliers(model2, 1.3), "has to be an integer")
  expect_error(outliers(model1, -1), "has to be >= 0")
  expect_error(outliers(model2, -1), "has to be >= 0")
  expect_error(outliers(model1, "m0"), "has to be numeric")
  expect_error(outliers(model2, "m1"), "has to be numeric")

  class(model1) <- NULL
  class(model2) <- NULL

  expect_error(outliers(model1, 1), "does not have the correct class")
  expect_error(outliers(model2, 1), "does not have the correct class")

})


test_that("outliers_prop() works correctly", {

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  data[1, "mpg"] <- NA
  data[2, "cyl"] <- NA
  data[3, "disp"] <- NA
  data[4, "wt"] <- NA

  model1 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = 3)

  expect_equal(outliers_prop(model1, 0), 1/28)
  expect_equal(outliers_prop(model1, 1), 1/28)
  expect_equal(outliers_prop(model1, 2), 1/28)
  expect_equal(outliers_prop(model1, 3), 1/28)

  model2 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = 3, shuffle = TRUE,
              shuffle_seed = 42, split = 0.5)

  expect_equal(outliers_prop(model2, 0), 16/28)
  expect_equal(outliers_prop(model2, 1), 12/28)
  expect_equal(outliers_prop(model2, 2), 12/28)
  expect_equal(outliers_prop(model2, 3), 12/28)

  model3 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = "convergence",
              convergence_criterion = 1)

  expect_equal(outliers_prop(model3, 0), 1/28)
  expect_equal(outliers_prop(model3, 1), 1/28)
  expect_error(outliers_prop(model3, 2), "fewer iterations than argument")

  model4 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = "convergence",
              convergence_criterion = 0.1)

  expect_equal(outliers_prop(model4, 0), 1/28)
  expect_equal(outliers_prop(model4, 1), 1/28)
  expect_equal(outliers_prop(model4, 2), 1/28)
  expect_error(outliers_prop(model4, 3), "fewer iterations than argument")

  model5 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 42, split = 0.5)

  expect_equal(outliers_prop(model5, 0), 16/28)
  expect_equal(outliers_prop(model5, 1), 12/28)
  expect_equal(outliers_prop(model5, 2), 12/28)
  expect_equal(outliers_prop(model5, 3), 12/28)
  expect_error(outliers_prop(model5, 4), "fewer iterations than argument")

  model6 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 24, split = 0.5)

  expect_equal(outliers_prop(model6, 0), 2/28)
  expect_equal(outliers_prop(model6, 1), 1/28)
  expect_equal(outliers_prop(model6, 2), 1/28)
  expect_equal(outliers_prop(model6, 3), 1/28)
  expect_error(outliers_prop(model6, 4), "fewer iterations than argument")

  model7 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.01,
              initial_est = "saturated", iterations = "convergence",
              convergence_criterion = 0.1, shuffle = TRUE,
              shuffle_seed = 24, split = 0.5)

  expect_equal(outliers_prop(model7, 0), 0)
  expect_equal(outliers_prop(model7, 1), 0)
  expect_equal(outliers_prop(model7, 2), 0)

  # test input error messages

  expect_error(outliers_prop(model1, 1.3), "has to be an integer")
  expect_error(outliers_prop(model2, 1.3), "has to be an integer")
  expect_error(outliers_prop(model1, -1), "has to be >= 0")
  expect_error(outliers_prop(model2, -1), "has to be >= 0")
  expect_error(outliers_prop(model1, "m0"), "has to be numeric")
  expect_error(outliers_prop(model2, "m1"), "has to be numeric")

  class(model1) <- NULL
  class(model2) <- NULL

  expect_error(outliers_prop(model1, 1), "does not have the correct class")
  expect_error(outliers_prop(model2, 1), "does not have the correct class")

})


test_that("outlier() works correctly", {

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  data[1, "mpg"] <- NA
  data[2, "cyl"] <- NA
  data[3, "disp"] <- NA
  data[4, "wt"] <- NA

  model1 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "robustified", iterations = 3)

  model2 <- outlier_detection(data = data, formula = formula,
              ref_dist = "normal", sign_level = 0.05,
              initial_est = "saturated", iterations = 3, shuffle = TRUE,
              shuffle_seed = 42, split = 0.5)

  m1o1 <- matrix(data = c(-1, -1, -1, -1), nrow = 1,
                dimnames = list("Mazda RX4", c("m0", "m1", "m2", "m3")))
  m1o5 <- matrix(data = c(1, 1, 1, 1), nrow = 1,
                dimnames = list("Hornet Sportabout", c("m0", "m1", "m2", "m3")))
  m1o25 <- matrix(data = c(0, 0, 0, 0), nrow = 1,
                dimnames = list("Pontiac Firebird", c("m0", "m1", "m2", "m3")))

  m2o1 <- matrix(data = c(-1, -1, -1, -1), nrow = 1,
                dimnames = list("Mazda RX4", c("m0", "m1", "m2", "m3")))
  m2o5 <- matrix(data = c(0, 0, 0, 0), nrow = 1,
                dimnames = list("Hornet Sportabout", c("m0", "m1", "m2", "m3")))
  m2o6 <- matrix(data = c(1, 1, 1, 1), nrow = 1,
                dimnames = list("Valiant", c("m0", "m1", "m2", "m3")))
  m2o18 <- matrix(data = c(0, 1, 1, 1), nrow = 1,
                dimnames = list("Fiat 128", c("m0", "m1", "m2", "m3")))
  m2o25 <- matrix(data = c(0, 0, 0, 0), nrow = 1,
                dimnames = list("Pontiac Firebird", c("m0", "m1", "m2", "m3")))

  expect_equal(outlier(model1, 1), m1o1)
  expect_equal(outlier(model1, 5), m1o5)
  expect_equal(outlier(model1, 25), m1o25)
  expect_equal(outlier(model2, 1), m2o1)
  expect_equal(outlier(model2, 5), m2o5)
  expect_equal(outlier(model2, 6), m2o6)
  expect_equal(outlier(model2, 18), m2o18)
  expect_equal(outlier(model2, 25), m2o25)

})


test_that("gauge_avar() gives correct errors", {

  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)

  expect_error(gauge_avar(1, 0.05, "robustified", 0, p, 0.5),
               "'ref_dist' must be a character vector of length 1")
  expect_error(gauge_avar(c("n", "n"), 0.05, "robustified", 0, p, 0.5),
               "'ref_dist' must be a character vector of length 1")
  expect_error(gauge_avar("nonexist", 0.05, "robustified", 1, p, 0.4),
               "'ref_dist' must be one of the available reference")
  expect_error(gauge_avar("normal", "a", "saturated", 0, p, 0.4),
               "'sign_level' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", c(0.01, 0.05), "saturated", 0, p, 0.4),
               "'sign_level' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", -0.3, "robustified", 1, p, 0.5),
               "'sign_level' must lie strictly between 0 and 1")
  expect_error(gauge_avar("normal", 1.2, "robustified", 1, p, 0.5),
               "'sign_level' must lie strictly between 0 and 1")
  expect_error(gauge_avar("normal", 0.05, 2, 1, p, 0.5),
               "'initial_est' must be a character vector of length 1")
  expect_error(gauge_avar("normal", 0.05, c("a", "b"), 1, p, 0.5),
               "'initial_est' must be a character vector of length 1")
  expect_error(gauge_avar("normal", 0.05, "nonexist", 1, p, 0.5),
               "'initial_est' must be one of the available initial estimators")
  expect_error(gauge_avar("normal", 0.05, "robustified", "1", p, 0.5),
               "'iteration' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", 0.05, "robustified", c(0,1), p, 0.5),
               "'iteration' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", 0.05, "saturated", -2, p, 0.5),
               "'iteration' must be weakly larger than 0")
  expect_error(gauge_avar("normal", 0.05, "saturated", 1.5, p, 0.5),
               "'iteration' must be an integer")
  expect_error(gauge_avar("normal", 0.01, "saturated", 0, p, "0.5"),
               "'split' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", 0.01, "saturated", 0, p, c(0.2, 0.5)),
               "'split' must be a numeric vector of length 1")
  expect_error(gauge_avar("normal", 0.01, "saturated", 0, p, -1.5),
               "'split' must lie strictly between 0 and 1")
  expect_error(gauge_avar("normal", 0.01, "saturated", 0, p, 2.4),
               "'split' must lie strictly between 0 and 1")

})
