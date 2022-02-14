# test explicitly the new "convergence" functionality
library(robust2sls)
p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)
pth <- "C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/r2sls/old_comparison"

out <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
               formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
               sign_level = 0.05, initial_est = "robustified",
               iterations = "convergence", convergence_criterion = 0)


out <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
               formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
               sign_level = 0.05, initial_est = "robustified",
               iterations = "convergence", convergence_criterion = 0,
               path = pth)

# check the results
a <- readRDS("C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/r2sls/old_comparison/M10n1000g0.05irobustifieds0.5")
b <- readRDS("C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/r2sls/old_comparison/M10n10000g0.05irobustifieds0.5")

## analysing and comparing
table(a$conv)
out[1, "conv_freq"]
# yes, correct

table(b$conv)
out[2, "conv_freq"]
# yes, correct

# test the new "max_iter" functionality
aprev <- table(a$conv)
bprev <- table(b$conv)

# now limit number of iterations to 5
## expect:
## a: 2:1, 3:3, 4:3, 5:3
## b: 5:10

out2 <- mc_grid(M = 10, n = c(1000, 10000), seed = 20, parameters = p,
               formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
               sign_level = 0.05, initial_est = "robustified",
               iterations = "convergence", convergence_criterion = 0, max_iter = 5,
               path = pth)

out2$conv_freq # is exactly as we expect, also confirmed below

a <- readRDS("C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/r2sls/old_comparison/M10n1000g0.05irobustifieds0.5")
b <- readRDS("C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/r2sls/old_comparison/M10n10000g0.05irobustifieds0.5")

table(a$conv)
table(b$conv)

library(doFuture)
library(robust2sls)
plan(cluster, workers = 2)
p <- generate_param(dx1 = 2, dx2 = 1, dz2 = 1, seed = 42)
out <- mc_grid(M = 3000, n = c(100, 1000), seed = 20, parameters = p,
               formula = y~x1+x2+x3|x1+x2+z3, ref_dist = "normal",
               sign_level = 0.05, initial_est = "robustified",
               iterations = 0, convergence_criterion = 0)
