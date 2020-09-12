
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

  expect_length(r1, 5)
  expect_length(r1$model, 19)
  expect_length(r1$res, 32)
  expect_length(r1$stdres, 32)
  expect_length(r1$sel, 32)
  expect_length(r1$type, 32)

  expect_type(r1$model, "list")
  expect_type(r1$res, "double")
  expect_type(r1$stdres, "double")
  expect_type(r1$sel, "logical")
  expect_type(r1$type, "integer")

  expect_named(r1$res)
  expect_named(r1$stdres)
  expect_named(r1$sel)
  expect_named(r1$type)

  expect_equal(class(r1$model), "ivreg")
  expect_equal(r1$model$coefficients[[1]], 29.57671472)
  expect_equal(r1$model$coefficients[[2]], 0.64358782)
  expect_equal(r1$model$coefficients[[3]], -0.05744568)
  expect_equal(r1$model$nobs, 28)

  res1 <- res2 <- res3 <- res4 <- as.double(NA)
  names(res1) <- "Mazda RX4"
  names(res2) <- "Mazda RX4 Wag"
  names(res3) <- "Datsun 710"
  names(res4) <- "Hornet 4 Drive"
  expect_equal(r1$res[1], res1)
  expect_equal(r1$res[2], res2)
  expect_equal(r1$res[3], res3)
  expect_equal(r1$res[4], res4)
  expect_equal(r1$stdres[1], res1)
  expect_equal(r1$stdres[2], res2)
  expect_equal(r1$stdres[3], res3)
  expect_equal(r1$stdres[4], res4)

  sel1 <- sel2 <- sel3 <- sel4 <- FALSE
  names(sel1) <- "Mazda RX4"
  names(sel2) <- "Mazda RX4 Wag"
  names(sel3) <- "Datsun 710"
  names(sel4) <- "Hornet 4 Drive"
  expect_equal(r1$sel[1], sel1)
  expect_equal(r1$sel[2], sel2)
  expect_equal(r1$sel[3], sel3)
  expect_equal(r1$sel[4], sel4)

  type1 <- type2 <- type3 <- type4 <- -1
  names(type1) <- "Mazda RX4"
  names(type2) <- "Mazda RX4 Wag"
  names(type3) <- "Datsun 710"
  names(type4) <- "Hornet 4 Drive"
  expect_equal(r1$type[1], type1)
  expect_equal(r1$type[2], type2)
  expect_equal(r1$type[3], type3)
  expect_equal(r1$type[4], type4)

  expect_equal(sum(is.na(r1$res)), 4)
  expect_equal(sum(is.na(r1$stdres)), 4)
  expect_equal(length(r1$stdres[abs(r1$stdres) > 1.96 & !is.na(r1$stdres)]), 1)
  expect_equal(names(r1$stdres[abs(r1$stdres) > 1.96 & !is.na(r1$stdres)]),
               "Pontiac Firebird")
  expect_equal(length(r1$sel[r1$sel == FALSE]), 5)
  expect_equal(names(r1$sel[r1$sel == FALSE]), c("Mazda RX4", "Mazda RX4 Wag",
                            "Datsun 710", "Hornet 4 Drive", "Pontiac Firebird"))
  expect_equal(length(r1$type[r1$type == -1]), 4)
  expect_equal(length(r1$type[r1$type == 0]), 1)
  expect_equal(length(r1$type[r1$type == 1]), 27)

  # test a few specific values (I copied shown values, so need to set tolerance)
  # got these values from the original function not the package function
  val5 <- structure(4.6550278, names = "Hornet Sportabout")
  val10 <- structure(-4.610346, names = "Merc 280")
  val15 <- structure(2.788944, names = "Cadillac Fleetwood")
  expect_equal(r1$res[5], val5, tolerance = 0.0001)
  expect_equal(r1$res[10], val10, tolerance = 0.0001)
  expect_equal(r1$res[15], val15, tolerance = 0.0001)
  val5 <- structure(1.308022, names = "Hornet Sportabout")
  val10 <- structure(-1.295467, names = "Merc 280")
  val15 <- structure(0.7836691, names = "Cadillac Fleetwood")
  expect_equal(r1$stdres[5], val5, tolerance = 0.0001)
  expect_equal(r1$stdres[10], val10, tolerance = 0.0001)
  expect_equal(r1$stdres[15], val15, tolerance = 0.0001)

  # snapshot tests that compare to the initial output of this list
  # initialised in version 0.0.0.9000
  # output coincided with the original function so confidence high
  expect_snapshot_output(cat(r1$res))
  expect_snapshot_output(cat(r1$stdres))
  expect_snapshot_output(cat(r1$sel))
  expect_snapshot_output(cat(r1$type))

})



test_that("saturated_init() works correctly", {

  data <- mtcars
  data[1, "mpg"] <- NA # y missing
  data[2, "cyl"] <- NA # x1 missing
  data[3, "disp"] <- NA # x2 missing
  data[4, "wt"] <- NA # z2 missing
  data[5, "hp"] <- NA # not relevant for estimation missing
  formula <- mpg ~ cyl + disp | cyl + wt
  shuffle_seed <- 42

  r1 <- saturated_init(data = data, formula = formula, cutoff = 1.96, shuffle =
                         TRUE, shuffle_seed = shuffle_seed, split = 0.5)

  expect_length(r1, 5)
  expect_length(r1$model, 2)
  expect_equal(names(r1$model), c("split1", "split2"))
  expect_type(r1$model, "list")
  expect_type(r1$model$split1, "list")
  expect_type(r1$model$split2, "list")

  # length of models in full case is 19 because has an additional element called
  # "na.action", which lists the obs that are omitted b/c they have missing
  # since I exclude these obs in the selection vector already, they don't have
  # to be excluded by the model estimation command -> 1 fewer element
  expect_length(r1$model$split1, 18)
  expect_length(r1$model$split2, 18)

  expect_equal(class(r1$model$split1), "ivreg")
  expect_equal(class(r1$model$split2), "ivreg")
  expect_equal(r1$model$split1$coefficients[[1]], 25.00242247,
               tolerance = 0.000001)
  expect_equal(r1$model$split1$coefficients[[2]], 0.19528941,
               tolerance = 0.000001)
  expect_equal(r1$model$split1$coefficients[[3]], -0.03178598,
               tolerance = 0.000001)
  expect_equal(r1$model$split2$coefficients[[1]], 21.2315540,
               tolerance = 0.000001)
  expect_equal(r1$model$split2$coefficients[[2]], 6.3640406,
               tolerance = 0.000001)
  expect_equal(r1$model$split2$coefficients[[3]], -0.1920017,
               tolerance = 0.000001)
  expect_equal(r1$model$split1$nobs, 14)
  expect_equal(r1$model$split2$nobs, 14)

  expect_length(r1$res, 32)
  expect_length(r1$stdres, 32)
  expect_length(r1$sel, 32)
  expect_length(r1$type, 32)

  expect_type(r1$res, "double")
  expect_type(r1$stdres, "double")
  expect_type(r1$sel, "logical")
  expect_type(r1$type, "integer")

  expect_named(r1$res)
  expect_named(r1$stdres)
  expect_named(r1$sel)
  expect_named(r1$type)

  res1 <- res2 <- res3 <- res4 <- as.double(NA)
  names(res1) <- "Mazda RX4"
  names(res2) <- "Mazda RX4 Wag"
  names(res3) <- "Datsun 710"
  names(res4) <- "Hornet 4 Drive"
  expect_equal(r1$res[1], res1)
  expect_equal(r1$res[2], res2)
  expect_equal(r1$res[3], res3)
  expect_equal(r1$res[4], res4)
  expect_equal(r1$stdres[1], res1)
  expect_equal(r1$stdres[2], res2)
  expect_equal(r1$stdres[3], res3)
  expect_equal(r1$stdres[4], res4)

  sel1 <- sel2 <- sel3 <- sel4 <- FALSE
  names(sel1) <- "Mazda RX4"
  names(sel2) <- "Mazda RX4 Wag"
  names(sel3) <- "Datsun 710"
  names(sel4) <- "Hornet 4 Drive"
  expect_equal(r1$sel[1], sel1)
  expect_equal(r1$sel[2], sel2)
  expect_equal(r1$sel[3], sel3)
  expect_equal(r1$sel[4], sel4)

  type1 <- type2 <- type3 <- type4 <- -1
  names(type1) <- "Mazda RX4"
  names(type2) <- "Mazda RX4 Wag"
  names(type3) <- "Datsun 710"
  names(type4) <- "Hornet 4 Drive"
  expect_equal(r1$type[1], type1)
  expect_equal(r1$type[2], type2)
  expect_equal(r1$type[3], type3)
  expect_equal(r1$type[4], type4)

  expect_equal(sum(is.na(r1$res)), 4)
  expect_equal(sum(is.na(r1$stdres)), 4)
  expect_equal(length(r1$stdres[abs(r1$stdres) > 1.96 & !is.na(r1$stdres)]), 16)

  outlier_names <- c("Hornet Sportabout", "Duster 360", "Merc 240D", "Merc 280",
                     "Merc 280C", "Cadillac Fleetwood", "Lincoln Continental",
                     "Chrysler Imperial", "Fiat 128", "Honda Civic",
                     "Toyota Corolla", "Camaro Z28", "Pontiac Firebird",
                     "Lotus Europa", "Ford Pantera L", "Ferrari Dino")
  expect_equal(names(r1$stdres[abs(r1$stdres) > 1.96 & !is.na(r1$stdres)]),
               outlier_names)
  expect_equal(length(r1$sel[r1$sel == FALSE]), 20)
  false_names <- c(c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710",
                     "Hornet 4 Drive"), outlier_names)
  expect_equal(names(r1$sel[r1$sel == FALSE]), false_names)
  expect_equal(length(r1$type[r1$type == -1]), 4)
  expect_equal(length(r1$type[r1$type == 0]), 16)
  expect_equal(length(r1$type[r1$type == 1]), 12)

  # test a few specific values (I copied shown values, so need to set tolerance)
  # got these values from the original function not the package function
  val5 <- structure(15.6767409, names = "Hornet Sportabout")
  val10 <- structure(-8.0363090, names = "Merc 280")
  val15 <- structure(28.8809336, names = "Cadillac Fleetwood")
  expect_equal(r1$res[5], val5, tolerance = 0.0001)
  expect_equal(r1$res[10], val10, tolerance = 0.0001)
  expect_equal(r1$res[15], val15, tolerance = 0.0001)
  val5 <- structure(5.8832370, names = "Hornet Sportabout")
  val10 <- structure(-3.0159018, names = "Merc 280")
  val15 <- structure(10.8385651, names = "Cadillac Fleetwood")
  expect_equal(r1$stdres[5], val5, tolerance = 0.0001)
  expect_equal(r1$stdres[10], val10, tolerance = 0.0001)
  expect_equal(r1$stdres[15], val15, tolerance = 0.0001)

  # snapshot tests that compare to the initial output of this list
  # initialised in version 0.0.0.9000
  # output coincided with the original function so confidence high
  expect_snapshot_output(cat(r1$res))
  expect_snapshot_output(cat(r1$stdres))
  expect_snapshot_output(cat(r1$sel))
  expect_snapshot_output(cat(r1$type))

})
