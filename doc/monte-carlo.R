## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(r2sls)
p <- generate_param(dx1 = 3, dx2 = 2, dz2 = 3, intercept = TRUE, seed = 42)

## -----------------------------------------------------------------------------
#library(r2sls)
#library(doParallel)
#ncores <- min(max(detectCores() - 1, 1), 2)
#cl <- makeCluster(ncores)
#registerDoParallel(cl)
#clusterCall(cl, function(x) .libPaths(x), .libPaths())
#sim1 <- mc_grid(M = 100, n = 100, seed = 42, parameters = p, 
#               formula = p$setting$formula, ref_dist = "normal", 
#               sign_level = 0.05, initial_est = "robustified", iterations = 0,
#               shuffle = FALSE, shuffle_seed = 42, split = 0.5)
#stopCluster(cl)

## -----------------------------------------------------------------------------
#library(r2sls)
library(doFuture)
library(parallel)
ncores <- min(max(detectCores() - 1, 1), 2)
registerDoFuture()
plan(cluster, workers = ncores)
sim2 <- mc_grid(M = 100, n = 100, seed = 42, parameters = p, 
               formula = p$setting$formula, ref_dist = "normal", 
               sign_level = 0.05, initial_est = "robustified", iterations = 0,
               shuffle = FALSE, shuffle_seed = 42, split = 0.5)

## -----------------------------------------------------------------------------
#library(r2sls)
library(doFuture)
library(parallel)
ncores <- min(max(detectCores() - 1, 1), 2)
registerDoFuture()
plan(sequential)
sim3 <- mc_grid(M = 100, n = 100, seed = 42, parameters = p, 
               formula = p$setting$formula, ref_dist = "normal", 
               sign_level = 0.05, initial_est = "robustified", iterations = 0,
               shuffle = FALSE, shuffle_seed = 42, split = 0.5)

## -----------------------------------------------------------------------------
#identical(sim1, sim2)
identical(sim2, sim3)

