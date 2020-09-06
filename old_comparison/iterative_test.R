data <- mtcars
formula <- mpg ~ cyl + disp | cyl + wt
split <- 0.5
shuffle <- TRUE
shuffle_seed <- 42
cutoff <- 1.96
data[1, "mpg"] <- NA
data[2, "cyl"] <- NA
data[3, "disp"] <- NA
data[4, "wt"] <- NA

testr <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                          sign_level = 0.05, initial_est = "robustified",
                          iterations = 1, convergence_criterion = NULL,
                          shuffle = FALSE, shuffle_seed = 42, split = 0.5)

tests <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                           sign_level = 0.05, initial_est = "saturated",
                           iterations = 1, convergence_criterion = NULL,
                           shuffle = TRUE, shuffle_seed = 42, split = 0.5)

testr <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                           sign_level = 0.05, initial_est = "robustified",
                           iterations = "convergence", convergence_criterion = 1,
                           shuffle = FALSE, shuffle_seed = 42, split = 0.5)

tests <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                           sign_level = 0.05, initial_est = "saturated",
                           iterations = "convergence", convergence_criterion = 0.5,
                           shuffle = TRUE, shuffle_seed = 42, split = 0.5)


t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = 5, convergence_criterion = NULL,
                       shuffle = FALSE, shuffle_seed = 42, split = 0.5)

t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = 0, convergence_criterion = NULL,
                       shuffle = FALSE, shuffle_seed = 42, split = 0.5)

t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "robustified",
                       iterations = "convergence", convergence_criterion = 1,
                       shuffle = FALSE, shuffle_seed = 42, split = 0.5)

t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "saturated",
                       iterations = 5, convergence_criterion = NULL,
                       shuffle = FALSE, shuffle_seed = 42, split = 0.5)

t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "saturated",
                       iterations = "convergence", convergence_criterion = 1,
                       shuffle = TRUE, shuffle_seed = 42, split = 0.5)

t <- outlier_detection(data = data, formula = formula, ref_dist = "normal",
                       sign_level = 0.05, initial_est = "saturated",
                       iterations = "convergence", convergence_criterion = 0.1,
                       shuffle = TRUE, shuffle_seed = 42, split = 0.5)

