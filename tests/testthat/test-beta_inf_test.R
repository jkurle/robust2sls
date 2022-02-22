test_that("beta_inf_correction() throws correct errors to invalid inputs", {

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  data[1, "mpg"] <- NA
  data[2, "cyl"] <- NA
  data[3, "disp"] <- NA
  data[4, "wt"] <- NA

  model <- outlier_detection(data = data, formula = formula,
                              ref_dist = "normal", sign_level = 0.05,
                              initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_inf_correction(z))
  expect_error(beta_inf_correction(t))

  expect_error(beta_inf_correction(model, iteration = "1"),
               "argument `iteration` has to be numeric")
  expect_error(beta_inf_correction(model, iteration = c(1,2)),
               "argument `iteration` has to be length 1")
  expect_error(beta_inf_correction(model, iteration = 1.3),
               "argument `iteration` has to be an integer")
  expect_error(beta_inf_correction(model, iteration = 0),
               "argument `iteration` must be >= 1")
  expect_error(beta_inf_correction(model, iteration = 4),
               "argument `iteration` specifies a higher iteration than was actually done")

  expect_error(beta_inf_correction(model, exact = "TRUE"),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf_correction(model, exact = 1),
               "argument `exact` has to be a logical value") # might want this to be accepted
  expect_error(beta_inf_correction(model, exact = "a"),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf_correction(model, exact = NULL),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf_correction(model, exact = c(TRUE, FALSE)),
               "argument `exact` has to be length 1")

  expect_error(beta_inf_correction(model, fp = "TRUE"),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf_correction(model, fp = 1),
               "argument `fp` has to be a logical value") # might want this to be accepted
  expect_error(beta_inf_correction(model, fp = "a"),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf_correction(model, fp = NULL),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf_correction(model, fp = c(TRUE, FALSE)),
               "argument `fp` has to be length 1")

})

test_that("beta_inf_correction() produces the correct output", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_inf_correction(z))
  expect_error(beta_inf_correction(t))

  expect_length(beta_inf_correction(model), 1)
  expect_equal(class(beta_inf_correction(model)), "numeric")

  # check correction factor fixed point and fail-safe
  expect_equal(attr(beta_inf_correction(model, iteration = 1, fp = FALSE),
                    "type of correction"), "iteration m = 1")
  expect_equal(attr(beta_inf_correction(model, iteration = 1, fp = TRUE),
                    "type of correction"), "iteration m = 1")
  expect_equal(attr(beta_inf_correction(model, iteration = 2, fp = FALSE),
                    "type of correction"), "iteration m = 2")
  expect_equal(attr(beta_inf_correction(model, iteration = 2, fp = TRUE),
                    "type of correction"), "fixed point")
  expect_equal(attr(beta_inf_correction(model, iteration = 3, fp = FALSE),
                    "type of correction"), "iteration m = 3")
  expect_equal(attr(beta_inf_correction(model, iteration = 3, fp = TRUE),
                    "type of correction"), "fixed point")

  # check a value; get rid of attribute for that purpose
  value <- beta_inf_correction(model, iteration = 1, fp = FALSE)
  attributes(value) <- NULL
  expect_equal(value, 1.531009, tolerance = 0.000001)
  value <- beta_inf_correction(model, iteration = 1, fp = FALSE, exact = TRUE)
  attributes(value) <- NULL
  expect_equal(value, 1.54301, tolerance = 0.000001)


  # synthetic data
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  dat <- generate_data(parameters = p, n = 1000)$data
  # this one does not converge, so $convergence$converged and $iter are NULL
  obj <- outlier_detection(data = dat, formula = p$setting$formula, "normal",
                           0.1, "robustified", iterations = "convergence",
                           convergence_criterion = 0, max_iter = 20)
  # has not converged, so in neither case should use fixed point
  a <- beta_inf_correction(obj, iteration = 1, fp = FALSE)
  b <- beta_inf_correction(obj, iteration = 1, fp = TRUE)
  c <- beta_inf_correction(obj, iteration = 20, fp = FALSE)
  d <- beta_inf_correction(obj, iteration = 20, fp = TRUE)
  expect_equal(a, b)
  expect_equal(c, d)
  expect_equal(attr(a, "type of correction"), "iteration m = 1")
  expect_equal(attr(b, "type of correction"), "iteration m = 1")
  expect_equal(attr(c, "type of correction"), "iteration m = 20")
  expect_equal(attr(d, "type of correction"), "iteration m = 20")

  # has not converged, so should not use fixed point
  obj <- outlier_detection(data = dat, formula = p$setting$formula, "normal",
                           0.1, "robustified", iterations = 1,
                           convergence_criterion = 0)
  a <- beta_inf_correction(obj, iteration = 1, fp = FALSE)
  b <- beta_inf_correction(obj, iteration = 1, fp = TRUE)
  expect_equal(a, b)

})

test_that("beta_inf() throws correct errors to invalid inputs", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_inf(z))
  expect_error(beta_inf(t))

  expect_error(beta_inf(model, iteration = "1"),
               "argument `iteration` has to be numeric")
  expect_error(beta_inf(model, iteration = c(1,2)),
               "argument `iteration` has to be length 1")
  expect_error(beta_inf(model, iteration = 1.3),
               "argument `iteration` has to be an integer")
  expect_error(beta_inf(model, iteration = 0),
               "argument `iteration` must be >= 1")
  expect_error(beta_inf(model, iteration = 4),
               "argument `iteration` specifies a higher iteration than was actually done")

  expect_error(beta_inf(model, exact = "TRUE"),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf(model, exact = 1),
               "argument `exact` has to be a logical value") # might want this to be accepted
  expect_error(beta_inf(model, exact = "a"),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf(model, exact = NULL),
               "argument `exact` has to be a logical value")
  expect_error(beta_inf(model, exact = c(TRUE, FALSE)),
               "argument `exact` has to be length 1")

  expect_error(beta_inf(model, fp = "TRUE"),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf(model, fp = 1),
               "argument `fp` has to be a logical value") # might want this to be accepted
  expect_error(beta_inf(model, fp = "a"),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf(model, fp = NULL),
               "argument `fp` has to be a logical value")
  expect_error(beta_inf(model, fp = c(TRUE, FALSE)),
               "argument `fp` has to be length 1")

})

test_that("beta_inf produces the correct output", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  inf1 <- beta_inf(model, iteration = 1, exact = FALSE, fp = FALSE)
  inf2 <- beta_inf(model, iteration = 1, exact = TRUE, fp = TRUE)
  inf3 <- beta_inf(model, iteration = 2, exact = FALSE, fp = FALSE)
  inf4 <- beta_inf(model, iteration = 2, exact = FALSE, fp = TRUE)
  expect_equal(class(inf1), c("matrix", "array"))
  expect_equal(dim(inf1), c(3, 9))
  # has the correct correction factor been used?
  correction <- beta_inf_correction(model, iteration = 1)
  attributes(correction) <- NULL
  expect_equal(inf1[1, 3] / inf1[1, 2], sqrt(correction))

  # check correction factor fixed point and fail-safe
  expect_equal(attr(beta_inf(model, iteration = 1, fp = FALSE),
                    "type of correction"), "iteration m = 1")
  expect_equal(attr(beta_inf(model, iteration = 1, fp = TRUE),
                    "type of correction"), "iteration m = 1")
  expect_equal(attr(beta_inf(model, iteration = 2, fp = FALSE),
                    "type of correction"), "iteration m = 2")
  expect_equal(attr(beta_inf(model, iteration = 2, fp = TRUE),
                    "type of correction"), "fixed point")
  expect_equal(attr(beta_inf(model, iteration = 3, fp = FALSE),
                    "type of correction"), "iteration m = 3")
  expect_equal(attr(beta_inf(model, iteration = 3, fp = TRUE),
                    "type of correction"), "fixed point")

  # that the functions work as intended has been verified by checking against
  # an equivalent implementation in Stata for a specific (large) dataset (DDCG)
  # but save output for swiss dataset as snapshot so at least recognise changes
  # in the future

  expect_snapshot_output(inf1)
  expect_snapshot_output(inf2)
  expect_snapshot_output(inf3)
  expect_snapshot_output(inf4)

})

test_that("beta_test_avar() throws the correct errors to invalid inputs", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_test_avar(z, iteration = 1))
  expect_error(beta_test_avar(t, iteration = 2))

  expect_error(beta_test_avar(model, iteration = "1"),
               "argument `iteration` has to be numeric")
  expect_error(beta_test_avar(model, iteration = c(1,2)),
               "argument `iteration` has to be length 1")
  expect_error(beta_test_avar(model, iteration = 1.3),
               "argument `iteration` has to be an integer")
  expect_error(beta_test_avar(model, iteration = 0),
               "argument `iteration` must be >= 1")
  expect_error(beta_test_avar(model, iteration = 4),
               "argument `iteration` specifies a higher iteration than was actually done")

  expect_error(beta_test_avar(model, iteration = 1, fp = "TRUE"),
               "argument `fp` has to be a logical value")
  expect_error(beta_test_avar(model, iteration = 1, fp = 1),
               "argument `fp` has to be a logical value") # might want this to be accepted
  expect_error(beta_test_avar(model, iteration = 1, fp = "a"),
               "argument `fp` has to be a logical value")
  expect_error(beta_test_avar(model, iteration = 1, fp = NULL),
               "argument `fp` has to be a logical value")
  expect_error(beta_test_avar(model, iteration = 1, fp = c(TRUE, FALSE)),
               "argument `fp` has to be length 1")

})

test_that("beta_test_avar() produces the correct output", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  avar1 <- beta_test_avar(model, iteration = 1, fp = FALSE)
  avar2 <- beta_test_avar(model, iteration = 1, fp = TRUE)
  avar3 <- beta_test_avar(model, iteration = 2, fp = FALSE)
  avar4 <- beta_test_avar(model, iteration = 2, fp = TRUE)
  avar5 <- beta_test_avar(model, iteration = 3, fp = FALSE)
  avar6 <- beta_test_avar(model, iteration = 3, fp = TRUE)

  expect_equal(class(avar1), c("matrix", "array"))
  expect_equal(dim(avar1), c(3, 3))

  # check correction factor fixed point and fail-safe
  expect_equal(attr(avar1, "type of avar"), "iteration m = 1")
  expect_equal(attr(avar2, "type of avar"), "iteration m = 1")
  expect_equal(attr(avar3, "type of avar"), "iteration m = 2")
  expect_equal(attr(avar4, "type of avar"), "fixed point")
  expect_equal(attr(avar5, "type of avar"), "iteration m = 3")
  expect_equal(attr(avar6, "type of avar"), "fixed point")

  # have verified it works for DDCG dataset
  # cannot verify in swiss data whether is accurate but probably is due to check
  # with DDCG data and implementation in Stata
  # save output as snapshot to detect future changes

  expect_snapshot_output(avar1)
  expect_snapshot_output(avar2)
  expect_snapshot_output(avar3)
  expect_snapshot_output(avar4)
  expect_snapshot_output(avar5)
  expect_snapshot_output(avar6)


  # synthetic data
  p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
  dat <- generate_data(parameters = p, n = 1000)$data
  obj <- outlier_detection(data = dat, formula = p$setting$formula, "normal",
                           0.1, "robustified", iterations = "convergence",
                           convergence_criterion = 0, max_iter = 20)
  # has not converged, so in neither case should use fixed point
  a <- beta_test_avar(obj, iteration = 1, fp = FALSE)
  b <- beta_test_avar(obj, iteration = 1, fp = TRUE)
  c <- beta_test_avar(obj, iteration = 20, fp = FALSE)
  d <- beta_test_avar(obj, iteration = 20, fp = TRUE)
  expect_equal(a, b)
  expect_equal(c, d)
  expect_equal(attr(a, "type of avar"), "iteration m = 1")
  expect_equal(attr(b, "type of avar"), "iteration m = 1")
  expect_equal(attr(c, "type of avar"), "iteration m = 20")
  expect_equal(attr(d, "type of avar"), "iteration m = 20")

  # has not converged, so should not use fixed point
  obj <- outlier_detection(data = dat, formula = p$setting$formula, "normal",
                           0.1, "robustified", iterations = 1,
                           convergence_criterion = 0)
  a <- beta_test_avar(obj, iteration = 1, fp = FALSE)
  b <- beta_test_avar(obj, iteration = 1, fp = TRUE)
  expect_equal(a, b)

  # convergence
  obj <- outlier_detection(data = dat, formula = p$setting$formula, "normal",
                           0.01, "robustified", iterations = "convergence",
                           convergence_criterion = 0)
  a <- beta_test_avar(obj, iteration = 4, fp = FALSE)
  b <- beta_test_avar(obj, iteration = 4, fp = TRUE)
  expect_equal(attr(a, "type of avar"), "iteration m = 4")
  expect_equal(attr(b, "type of avar"), "fixed point")

})

test_that("beta_t() throws the correct errors to invalid inputs", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_t(z, iteration = 1, element = 1))
  expect_error(beta_t(t, iteration = 2, element = 1))

  expect_error(beta_t(model, iteration = "1", element = 1),
               "argument `iteration` has to be numeric")
  expect_error(beta_t(model, iteration = c(1,2), element = 1),
               "argument `iteration` has to be length 1")
  expect_error(beta_t(model, iteration = 1.3, element = 1),
               "argument `iteration` has to be an integer")
  expect_error(beta_t(model, iteration = 0, element = 1),
               "argument `iteration` must be >= 1")
  expect_error(beta_t(model, iteration = 4, element = 1),
               "argument `iteration` specifies a higher iteration than was actually done")

  expect_error(beta_t(model, iteration = 2, element = c(TRUE, FALSE)),
               "argument `element` must be numeric or a string")

  expect_error(beta_t(model, iteration = 2, element = c(1, 1.2)),
               "argument `element` must only contain integers if numeric")
  expect_error(beta_t(model, iteration = 2, element = 4),
               "argument `element` contains indices larger than the number of coefficients in the model")
  expect_error(beta_t(model, iteration = 2, element = c(1,2,3,4)),
               "argument `element` contains indices larger than the number of coefficients in the model")
  expect_error(beta_t(model, iteration = 2, element = 0),
               "argument `element` contains indices < 1")
  expect_error(beta_t(model, iteration = 2, element = c(1,2,0,-1)),
               "argument `element` contains indices < 1")

  expect_error(beta_t(model, iteration = 2, element = "nonexistent"),
               "argument `element` contains names that are not among the coefficients in the model")
  expect_error(beta_t(model, iteration = 2, element = c("nonexistent", "Education")),
               "argument `element` contains names that are not among the coefficients in the model")

  expect_error(beta_t(model, iteration = 1, element = 1, fp = "TRUE"),
               "argument `fp` has to be a logical value")
  expect_error(beta_t(model, iteration = 1, element = 1, fp = 1),
               "argument `fp` has to be a logical value") # might want this to be accepted
  expect_error(beta_t(model, iteration = 1, element = 1, fp = "a"),
               "argument `fp` has to be a logical value")
  expect_error(beta_t(model, iteration = 1, element = 1, fp = NULL),
               "argument `fp` has to be a logical value")
  expect_error(beta_t(model, iteration = 1, element = 1, fp = c(TRUE, FALSE)),
               "argument `fp` has to be length 1")

})

test_that("beta_t() produces the correct output", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)
  # testing a single element
  t1 <- beta_t(model, iteration = 1, fp = FALSE, element = 1)
  t2 <- beta_t(model, iteration = 1, fp = TRUE, element = "(Intercept)")
  t3 <- beta_t(model, iteration = 2, fp = FALSE, element = 2)
  t4 <- beta_t(model, iteration = 2, fp = TRUE, element = 2)

  expect_equal(class(t1), c("matrix", "array"))
  expect_equal(class(t2), c("matrix", "array"))
  expect_equal(class(t3), c("matrix", "array"))
  expect_equal(class(t4), c("matrix", "array"))
  expect_equal(dim(t1), c(1, 7))
  expect_equal(dim(t2), c(1, 7))
  expect_equal(dim(t3), c(1, 7))
  expect_equal(dim(t4), c(1, 7))

  expect_equal(attr(t1, "type of avar"), "iteration m = 1")
  expect_equal(attr(t2, "type of avar"), "iteration m = 1")
  expect_equal(attr(t3, "type of avar"), "iteration m = 2")
  expect_equal(attr(t4, "type of avar"), "fixed point")

  expect_identical(t1, t2) # fp or how element is addressed should not matter
  expect_equal(identical(t3, t4), FALSE) # here fp matters

  # have not double verified whether output is correct but double checked with
  # DDCG dataset and a different implementation in Stata; there command worked
  # so still save snapshot here to detect any future changes
  expect_snapshot_output(t1)
  expect_snapshot_output(t2)
  expect_snapshot_output(t3)
  expect_snapshot_output(t4)

  # several elements tested individually by t-test
  tt1 <- beta_t(model, iteration = 1, element = c(1,2), fp = FALSE)
  tt2 <- beta_t(model, iteration = 1, element = c("(Intercept)", "Education"),
                fp = TRUE)
  tt3 <- beta_t(model, iteration = 2, element = c(1,2,3), fp = FALSE)
  tt4 <- beta_t(model, iteration = 2, element = c(1,2,3), fp = TRUE)

  expect_equal(class(tt1), c("matrix", "array"))
  expect_equal(class(tt2), c("matrix", "array"))
  expect_equal(class(tt3), c("matrix", "array"))
  expect_equal(class(tt4), c("matrix", "array"))
  expect_equal(dim(tt1), c(2, 7))
  expect_equal(dim(tt2), c(2, 7))
  expect_equal(dim(tt3), c(3, 7))
  expect_equal(dim(tt4), c(3, 7))

  expect_equal(attr(tt1, "type of avar"), "iteration m = 1")
  expect_equal(attr(tt2, "type of avar"), "iteration m = 1")
  expect_equal(attr(tt3, "type of avar"), "iteration m = 2")
  expect_equal(attr(tt4, "type of avar"), "fixed point")

  expect_identical(tt1, tt2) # fp or how element is addressed should not matter
  expect_equal(identical(tt3, tt4), FALSE) # here fp matters

  # have not double verified whether output is correct but double checked with
  # DDCG dataset and a different implementation in Stata; there command worked
  # so still save snapshot here to detect any future changes
  expect_snapshot_output(tt1)
  expect_snapshot_output(tt2)
  expect_snapshot_output(tt3)
  expect_snapshot_output(tt4)

  # compare values from multiple tests and single tests
  expect_identical(t1[1,], tt1[1,])
  expect_identical(t1[1,], tt2[1,])

  # test that errors raised if index addresses an NA value or different elements
  t <- z <- model
  z$model$m1$coefficients[[1]] <- NA # set intercept to NA
  names(t$model$m1$coefficients) <- c("othername", "Education", "Infant.Mortality")
  expect_error(beta_t(z, iteration = 1, element = 1),
               "At least one of the coefficients is NA. Check elements.")
  expect_error(beta_t(z, iteration = 1, element = "(Intercept)"),
               "At least one of the coefficients is NA. Check elements.")
  expect_error(beta_t(t, iteration = 1, element = 1),
               "index selects different coefficients in the robust and full")

})

test_that("beta_hausman() throws the correct errors to invalid inputs", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)

  t <- z <- model
  z$cons$iterations <- NULL
  class(t) <- NULL
  expect_error(beta_hausman(z, iteration = 1, subset = 1))
  expect_error(beta_hausman(t, iteration = 2, subset = 1))

  expect_error(beta_hausman(model, iteration = "1", subset = 1),
               "argument `iteration` has to be numeric")
  expect_error(beta_hausman(model, iteration = c(1,2), subset = 1),
               "argument `iteration` has to be length 1")
  expect_error(beta_hausman(model, iteration = 1.3, subset = 1),
               "argument `iteration` has to be an integer")
  expect_error(beta_hausman(model, iteration = 0, subset = 1),
               "argument `iteration` must be >= 1")
  expect_error(beta_hausman(model, iteration = 4, subset = 1),
               "argument `iteration` specifies a higher iteration than was actually done")

  expect_error(beta_hausman(model, iteration = 2, subset = c(TRUE, FALSE)),
               "argument `subset` must be NULL or numeric or a string")

  expect_error(beta_hausman(model, iteration = 2, subset = c(1, 1.2)),
               "argument `subset` must only contain integers if numeric")
  expect_error(beta_hausman(model, iteration = 2, subset = 4),
               "argument `subset` contains indices larger than the number of coefficients in the model")
  expect_error(beta_hausman(model, iteration = 2, subset = c(1,2,3,4)),
               "argument `subset` contains indices larger than the number of coefficients in the model")
  expect_error(beta_hausman(model, iteration = 2, subset = 0),
               "argument `subset` contains indices < 1")
  expect_error(beta_hausman(model, iteration = 2, subset = c(1,2,0,-1)),
               "argument `subset` contains indices < 1")

  expect_error(beta_hausman(model, iteration = 2, subset = "nonexistent"),
               "argument `subset` contains names that are not among the coefficients in the model")
  expect_error(beta_hausman(model, iteration = 2, subset = c("nonexistent", "Education")),
               "argument `subset` contains names that are not among the coefficients in the model")

  expect_error(beta_hausman(model, iteration = 1, subset = 1, fp = "TRUE"),
               "argument `fp` has to be a logical value")
  expect_error(beta_hausman(model, iteration = 1, subset = 1, fp = 1),
               "argument `fp` has to be a logical value") # might want this to be accepted
  expect_error(beta_hausman(model, iteration = 1, subset = 1, fp = "a"),
               "argument `fp` has to be a logical value")
  expect_error(beta_hausman(model, iteration = 1, subset = 1, fp = NULL),
               "argument `fp` has to be a logical value")
  expect_error(beta_hausman(model, iteration = 1, subset = 1, fp = c(TRUE, FALSE)),
               "argument `fp` has to be length 1")

})

test_that("beta_hausman() produces the correct output", {

  data <- datasets::swiss
  formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
  # this example only converges after the second iteration
  model <- outlier_detection(data = data, formula = formula,
                             ref_dist = "normal", sign_level = 0.05,
                             initial_est = "robustified", iterations = 3)
  # testing a single element
  h1 <- beta_hausman(model, iteration = 1, fp = FALSE, subset = c(1, 2))
  h2 <- beta_hausman(model, iteration = 1, fp = TRUE, subset = c("(Intercept)", "Education"))
  h3 <- beta_hausman(model, iteration = 2, fp = FALSE, subset = c(1, 2, 3))
  h4 <- beta_hausman(model, iteration = 2, fp = FALSE)
  h5 <- beta_hausman(model, iteration = 2, fp = TRUE)

  expect_equal(class(h1), c("matrix", "array"))
  expect_equal(class(h2), c("matrix", "array"))
  expect_equal(class(h3), c("matrix", "array"))
  expect_equal(class(h4), c("matrix", "array"))
  expect_equal(class(h5), c("matrix", "array"))
  expect_equal(dim(h1), c(1, 2))
  expect_equal(dim(h2), c(1, 2))
  expect_equal(dim(h3), c(1, 2))
  expect_equal(dim(h4), c(1, 2))
  expect_equal(dim(h5), c(1, 2))

  expect_equal(attr(h1, "type of avar"), "iteration m = 1")
  expect_equal(attr(h2, "type of avar"), "iteration m = 1")
  expect_equal(attr(h3, "type of avar"), "iteration m = 2")
  expect_equal(attr(h4, "type of avar"), "iteration m = 2")
  expect_equal(attr(h5, "type of avar"), "fixed point")

  expect_equal(attr(h1, "coefficients"), c("(Intercept)", "Education"))
  expect_equal(attr(h2, "coefficients"), c("(Intercept)", "Education"))
  expect_equal(attr(h3, "coefficients"), c("(Intercept)", "Education", "Infant.Mortality"))
  expect_equal(attr(h4, "coefficients"), c("(Intercept)", "Education", "Infant.Mortality"))
  expect_equal(attr(h5, "coefficients"), c("(Intercept)", "Education", "Infant.Mortality"))

  expect_identical(h1, h1) # fp or how subset is addressed should not matter
  expect_identical(h3, h4)
  expect_equal(identical(h4, h5), FALSE) # here fp matters

  # have not double verified whether output is correct but double checked with
  # DDCG dataset and a different implementation in Stata; there command worked
  # so still save snapshot here to detect any future changes
  expect_snapshot_output(h1)
  expect_snapshot_output(h2)
  expect_snapshot_output(h3)
  expect_snapshot_output(h4)
  expect_snapshot_output(h5)

  # test that errors raised if index addresses an NA value or different elements
  t <- z <- model
  z$model$m1$coefficients[[1]] <- NA # set intercept to NA
  names(t$model$m1$coefficients) <- c("othername", "Education", "Infant.Mortality")
  expect_error(beta_hausman(z, iteration = 1, subset = c(1, 2)),
               "At least one of the coefficients is NA. Check elements.")
  expect_error(beta_hausman(t, iteration = 1, subset = c(1, 2)),
               "index selects different coefficients in the robust and full")

})
