# Manual reference for the reduced mc_grid() convergence fixtures.
# Run from the package root with Rscript, after installing/loading the package.

library(robust2sls)

p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
formula <- y ~ x2 + x3 + x4 + x5 | x2 + x3 + z4 + z5 + z6
root <- file.path("tests", "testthat", "testdata", "mcgrid", "d4")

run_reference <- function(max_iter = NULL) {
  result <- list()

  for (n in c(50, 100)) {
    runs <- vector("list", 4)

    for (m in seq_len(4)) {
      fixture <- readRDS(file.path(root, paste0("n", n), paste0("n", n, "_m", m, ".rds")))
      data <- if (is.list(fixture) && !is.null(fixture$data)) fixture$data else fixture
      model <- outlier_detection(
        data = data,
        formula = formula,
        ref_dist = "normal",
        sign_level = 0.05,
        initial_est = "robustified",
        iterations = "convergence",
        convergence_criterion = 0,
        max_iter = max_iter,
        shuffle = FALSE,
        shuffle_seed = NULL,
        split = 0.5,
        verbose = FALSE
      )

      last_iteration <- length(model$model) - 1L
      runs[[m]] <- list(
        n = n,
        m = m,
        iterations = last_iteration,
        max_iter = max_iter,
        n_outliers = sum(model$type[[length(model$type)]] == 0),
        n_nonmissing = sum(model$type[[length(model$type)]] != -1)
      )
    }

    result[[paste0("n", n)]] <- runs
  }

  result
}

print(run_reference())
print(run_reference(max_iter = 3))
