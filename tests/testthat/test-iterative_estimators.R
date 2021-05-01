
test_that("outlier_detection() produces correct output", {

  data <- datasets::mtcars
  formula <- mpg ~ cyl + disp | cyl + wt
  data[1, "mpg"] <- NA
  data[2, "cyl"] <- NA
  data[3, "disp"] <- NA
  data[4, "wt"] <- NA
  data[5, "gear"] <- NA

  # check settings with robustified as initial estimator
  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "robustified",
          iterations = 0, convergence_criterion = NULL,
          shuffle = FALSE, shuffle_seed = 42, split = 0.5)

  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "robustified")
  expect_equal(t$cons$initial$split, NULL)
  expect_equal(t$cons$initial$shuffle, NULL)
  expect_equal(t$cons$initial$shuffle_seed, NULL)
  expect_equal(t$cons$convergence$criterion, NULL)
  expect_equal(t$cons$convergence$difference, NULL)
  expect_equal(t$cons$convergence$converged, NULL)
  expect_equal(t$cons$convergence$iter, NULL)
  expect_equal(t$cons$iterations$setting, 0)
  expect_equal(t$cons$iterations$actual, 0)
  expect_equal(t$model$m0$coefficients[[1]], 29.57671472)
  expect_equal(t$model$m0$coefficients[[2]], 0.64358782)
  expect_equal(t$model$m0$coefficients[[3]], -0.05744568)
  expect_equal(sum(t$sel$m0), 27)
  # can use validate_robust2sls(t) to ensure that the structure of the
  # robust2sls object is valid; the fun() returns the object itself if no errors
  expect_equal(validate_robust2sls(t), t)

  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "robustified",
          iterations = 5, convergence_criterion = NULL,
          shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "robustified")
  expect_equal(t$cons$initial$split, NULL)
  expect_equal(t$cons$initial$shuffle, NULL)
  expect_equal(t$cons$initial$shuffle_seed, NULL)
  expect_equal(t$cons$convergence$criterion, NULL)
  expect_equal(t$cons$convergence$difference, 0) # has actually converged
  expect_equal(t$cons$convergence$converged, TRUE) # has converged
  expect_equal(t$cons$convergence$iter, 1) # has converged at first iteration
  expect_equal(t$cons$iterations$setting, 5)
  expect_equal(t$cons$iterations$actual, 5)
  expect_equal(t$model$m0$coefficients[[1]], 29.57671472)
  expect_equal(t$model$m0$coefficients[[2]], 0.64358782)
  expect_equal(t$model$m0$coefficients[[3]], -0.05744568)
  # in this setup, the model converges already after the first iteration so all
  # $model should be the same from $m1 onwards; same for other components
  expect_equal(t$model$m1, t$model$m2)
  expect_equal(t$model$m1, t$model$m3)
  expect_equal(t$model$m1, t$model$m4)
  expect_equal(t$model$m1, t$model$m5)
  expect_equal(t$res$m1, t$res$m2)
  expect_equal(t$res$m1, t$res$m3)
  expect_equal(t$res$m1, t$res$m4)
  expect_equal(t$res$m1, t$res$m5)
  expect_equal(t$stdres$m1, t$stdres$m2)
  expect_equal(t$stdres$m1, t$stdres$m3)
  expect_equal(t$stdres$m1, t$stdres$m4)
  expect_equal(t$stdres$m1, t$stdres$m5)
  expect_equal(t$sel$m1, t$sel$m2)
  expect_equal(t$sel$m1, t$sel$m3)
  expect_equal(t$sel$m1, t$sel$m4)
  expect_equal(t$sel$m1, t$sel$m5)
  expect_equal(t$type$m1, t$type$m2)
  expect_equal(t$type$m1, t$type$m3)
  expect_equal(t$type$m1, t$type$m4)
  expect_equal(t$type$m1, t$type$m5)
  expect_equal(t$model$m1$coefficients[[1]], 30.52914035, tolerance = 0.0000001)
  expect_equal(t$model$m1$coefficients[[2]], 0.36662884, tolerance = 0.0000001)
  expect_equal(t$model$m1$coefficients[[3]], -0.05532395, tolerance = 0.0000001)
  expect_equal(validate_robust2sls(t), t)

  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "robustified",
          iterations = 5, convergence_criterion = 0,
          shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "robustified")
  expect_equal(t$cons$initial$split, NULL)
  expect_equal(t$cons$initial$shuffle, NULL)
  expect_equal(t$cons$initial$shuffle_seed, NULL)
  expect_equal(t$cons$convergence$criterion, 0)
  expect_equal(t$cons$convergence$difference, 0)
  expect_equal(t$cons$convergence$converged, TRUE)
  expect_equal(t$cons$iterations$setting, 5)
  expect_equal(t$cons$iterations$actual, 2)
  expect_equal(validate_robust2sls(t), t)

  # check settings with saturated as initial estimator
  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "saturated",
          iterations = 5, convergence_criterion = NULL,
          shuffle = TRUE, shuffle_seed = 42, split = 0.5)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "saturated")
  expect_equal(t$cons$initial$split, 0.5)
  expect_equal(t$cons$initial$shuffle, TRUE)
  expect_equal(t$cons$initial$shuffle_seed, 42)
  expect_equal(t$cons$convergence$criterion, NULL)
  expect_equal(t$cons$convergence$difference, 0) # has actually converged
  expect_equal(t$cons$convergence$converged, TRUE) # has converged
  expect_equal(t$cons$convergence$iter, 2) # for saturated took 2 iterations
  expect_equal(t$cons$iterations$setting, 5)
  expect_equal(t$cons$iterations$actual, 5)
  expect_equal(t$model$m0$split1$coefficients[[1]], 25.00242247,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[2]], 0.19528941,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[3]], -0.03178598,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[1]], 21.2315540,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[2]], 6.3640406,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[3]], -0.1920017,
               tolerance = 0.000001)
  # has converged after iteration 2
  expect_equal(t$model$m2, t$model$m3)
  expect_equal(t$model$m2, t$model$m4)
  expect_equal(t$model$m2, t$model$m5)
  expect_equal(sum(t$sel$m0), 12)
  expect_equal(sum(t$sel$m1), 16)
  expect_equal(sum(t$sel$m2), 16)
  expect_equal(validate_robust2sls(t), t)

  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "saturated",
          iterations = 5, convergence_criterion = 1,
          shuffle = FALSE, shuffle_seed = 42, split = 0.5)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "saturated")
  expect_equal(t$cons$initial$split, 0.5)
  expect_equal(t$cons$initial$shuffle, FALSE)
  expect_equal(t$cons$initial$shuffle_seed, NULL)
  expect_equal(t$cons$convergence$criterion, 1)
  expect_equal(t$cons$convergence$difference, 0)
  expect_equal(t$cons$convergence$converged, TRUE)
  expect_equal(t$cons$iterations$setting, 5)
  expect_equal(t$cons$iterations$actual, 3)
  expect_equal(t$model$m0$split1$coefficients[[1]], 16.0480545,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[2]], 6.3738414,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[3]], -0.1552722,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[1]], 30.75780490,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[2]], -0.24236552,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[3]], -0.04049272,
               tolerance = 0.000001)
  # has converged after iteration 2
  expect_equal(t$model$m2, t$model$m3)
  expect_equal(sum(t$sel$m0), 23)
  expect_equal(sum(t$sel$m1), 24)
  expect_equal(sum(t$sel$m2), 24)
  expect_equal(validate_robust2sls(t), t)


  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "saturated",
          iterations = 10, convergence_criterion = 0.01,
          shuffle = TRUE, shuffle_seed = 24, split = 0.5)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "saturated")
  expect_equal(t$cons$initial$split, 0.5)
  expect_equal(t$cons$initial$shuffle, TRUE)
  expect_equal(t$cons$initial$shuffle_seed, 24)
  expect_equal(t$cons$convergence$criterion, 0.01)
  expect_equal(t$cons$convergence$difference, 0)
  expect_equal(t$cons$convergence$converged, TRUE)
  expect_equal(t$cons$iterations$setting, 10)
  expect_equal(t$cons$iterations$actual, 3)
  expect_equal(t$model$m0$split1$coefficients[[1]], 28.19194229,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[2]], 0.81974473,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[3]], -0.05968683,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[1]], 28.02964414,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[2]], 1.45054826,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[3]], -0.06818865,
               tolerance = 0.000001)
  expect_equal(t$model$m1$coefficients[[1]], 30.00601297, tolerance = 0.000001)
  expect_equal(t$model$m1$coefficients[[2]], 0.23228773, tolerance = 0.000001)
  expect_equal(t$model$m1$coefficients[[3]], -0.05053831, tolerance = 0.000001)
  expect_equal(t$model$m2$coefficients[[1]], 30.52914035, tolerance = 0.000001)
  expect_equal(t$model$m2$coefficients[[2]], 0.36662884, tolerance = 0.000001)
  expect_equal(t$model$m2$coefficients[[3]], -0.05532395, tolerance = 0.000001)
  # has converged after 2 iterations
  expect_equal(t$model$m2, t$model$m3)
  expect_equal(sum(t$sel$m0), 26)
  expect_equal(sum(t$sel$m1), 27)
  expect_equal(sum(t$sel$m2), 27)
  expect_equal(validate_robust2sls(t), t)

  t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
          sign_level = 0.05, initial_est = "saturated",
          iterations = 10, convergence_criterion = 0.01,
          shuffle = TRUE, shuffle_seed = 42, split = 0.4)
  expect_equal(t$cons$formula, mpg ~ cyl + disp | cyl + wt)
  expect_equal(t$cons$data, data)
  expect_equal(t$cons$reference, "normal")
  expect_equal(t$cons$sign_level, 0.05)
  expect_equal(t$cons$psi, 0.95)
  expect_equal(t$cons$cutoff, 1.959964)
  expect_equal(t$cons$bias_corr, 1.317798, tolerance = 0.0000001)
  expect_equal(t$cons$initial$estimator, "saturated")
  expect_equal(t$cons$initial$split, 0.4)
  expect_equal(t$cons$initial$shuffle, TRUE)
  expect_equal(t$cons$initial$shuffle_seed, 42)
  expect_equal(t$cons$convergence$criterion, 0.01)
  expect_equal(t$cons$convergence$difference, 0)
  expect_equal(t$cons$convergence$converged, TRUE)
  expect_equal(t$cons$iterations$setting, 10)
  expect_equal(t$cons$iterations$actual, 5)
  expect_equal(t$model$m0$split1$nobs, 17)
  expect_equal(t$model$m0$split2$nobs, 11)
  expect_equal(sum(t$sel$m0), 10)
  expect_equal(sum(t$sel$m1), 13)
  expect_equal(sum(t$sel$m2), 16)
  expect_equal(sum(t$sel$m3), 17)
  expect_equal(sum(t$sel$m4), 17)
  expect_equal(sum(t$sel$m5), 17)
  expect_length(t$type$m0[t$type$m0 == -1], 4)
  expect_length(t$type$m0[t$type$m0 == 0], 18)
  expect_length(t$type$m0[t$type$m0 == 1], 10)
  expect_equal(identical(t$sel$m3, t$sel$m4), TRUE)
  expect_equal(identical(t$sel$m4, t$sel$m5), TRUE)
  expect_equal(t$model$m0$split1$coefficients[[1]], 26.87806516,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[2]], 0.11273174,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split1$coefficients[[3]], -0.03573733,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[1]], 22.7624061,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[2]], 6.4309284,
               tolerance = 0.000001)
  expect_equal(t$model$m0$split2$coefficients[[3]], -0.2055043,
               tolerance = 0.000001)
  expect_equal(t$model$m5$coefficients[[1]], 25.9061700, tolerance = 0.000001)
  expect_equal(t$model$m5$coefficients[[2]], 4.2692588, tolerance = 0.000001)
  expect_equal(t$model$m5$coefficients[[3]], -0.1516304, tolerance = 0.000001)
  expect_equal(identical(t$model$m5, t$model$m4), TRUE)
  expect_equal(validate_robust2sls(t), t)

})




