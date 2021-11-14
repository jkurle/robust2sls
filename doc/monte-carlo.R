## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, echo = FALSE-------------------------------------------
Sys.setenv(LANGUAGE="en")

## ---- echo = FALSE------------------------------------------------------------
utils::vignette("overview", package = "robust2sls")

## -----------------------------------------------------------------------------
library(robust2sls)
p <- generate_param(dx1 = 3, dx2 = 2, dz2 = 3, intercept = TRUE, seed = 10)

## -----------------------------------------------------------------------------
library(parallel)
ncores <- min(max(detectCores() - 1, 1), 2)
cl <- makeCluster(ncores)
# export libraries to all workers in the cluster
invisible(clusterCall(cl = cl, function(x) .libPaths(x), .libPaths()))

## -----------------------------------------------------------------------------
library(doParallel)
registerDoParallel(cl)
sim1 <- mc_grid(M = 100, n = c(100, 1000), seed = 42, parameters = p, 
               formula = p$setting$formula, ref_dist = "normal", 
               sign_level = 0.05, initial_est = "robustified", iterations = 0,
               shuffle = FALSE, shuffle_seed = 42, split = 0.5)

## -----------------------------------------------------------------------------
library(doFuture)
registerDoFuture()
plan(cluster, workers = cl)
sim2 <- mc_grid(M = 100, n = c(100, 1000), seed = 42, parameters = p, 
               formula = p$setting$formula, ref_dist = "normal", 
               sign_level = 0.05, initial_est = "robustified", iterations = 0,
               shuffle = FALSE, shuffle_seed = 42, split = 0.5)
stopCluster(cl)

# check identical results
identical(sim1, sim2)

## -----------------------------------------------------------------------------
library(doFuture)
registerDoFuture()
plan(sequential)
sim3 <- mc_grid(M = 100, n = c(100, 1000), seed = 42, parameters = p, 
               formula = p$setting$formula, ref_dist = "normal", 
               sign_level = 0.05, initial_est = "robustified", iterations = 0,
               shuffle = FALSE, shuffle_seed = 42, split = 0.5)

# check identical results
identical(sim1, sim3)

