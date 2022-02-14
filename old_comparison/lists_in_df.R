
# df represents the results for 4 replications
df <- data.frame(name = LETTERS[1:4], iter = c(2, 2:4))

# now need to extract how often each iteration occurred
occur <- as.list(table(df$iter))

# now add this to a "normal" dataframe with atomic vectors
M <- 1000
n <- 100
mean_gauge <- 0.049
res <- data.frame(M, n, mean_gauge, freq = I(list(occur)))


# test which is the converged iteration
library(robust2sls)
p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)
d <- generate_data(parameters = p, n = 1000)$data
model <- outlier_detection(data = d, formula = y~x1+x2+x3|x1+x2+z3,
                           ref_dist = "normal", sign_level = 0.05, initial_est = "robustified",
                           iterations = "convergence", convergence_criterion = 0)
identical(model$model$m5, model$model$m6) # TRUE
# so actually the iteration before is the one that is the converged one!
