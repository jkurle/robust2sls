# used to test the bootstrap functions during development to check whether
# they work as intended

all_ind <- c(1, 2, 3, 4, 6, 7, 8, 9, 10) # leaves out the 5
R <- 3 # only do 5 resamples to keep the overview

resamples <- nonparametric(all_ind, R = R)
resamples

count <- count_indices(resamples, all_ind)
count

data <- datasets::swiss
data <- data[1:10, ]
data

for (i in (1:R)) {

  sample <- nonparametric_resampling(df = data, resample = resamples[[i]])
  print(resamples[[i]])
  print(sample)

  # notice that if an observations is sampled several times and is named, then
  # its name is extended with .1, .2 etc.

}

data2 <- data
rownames(data2) <- NULL # get rid of rownames

for (i in (1:R)) {

  sample <- nonparametric_resampling(df = data2, resample = resamples[[i]])
  print(resamples[[i]])
  print(sample)

  # if not named then still have numbers 1, 2, 3 etc.
  # the duplicate resamples are then called 1.1, 1.2, 1.3 etc.

}

# toy case

data <- datasets::swiss
# make first case not useable bc missing
data[1, "Fertility"] <- NA
formula <- Fertility ~ Education + Infant.Mortality | Catholic + Infant.Mortality
# this example only converges after the second iteration
model <- outlier_detection(data = data, formula = formula,
                           ref_dist = "normal", sign_level = 0.05,
                           initial_est = "robustified", iterations = 3)
# model$cons$data stores the original data, i.e. not only the onse which can
# be used during estimation
model$model$m0$model # this one excludes non-usable data
NROW(model$cons$data) # 47
NROW(model$model$m0$model) # 46
NROW(model$model$m1$model) # 44 b/c some were removed as outliers

# this would be produced by utility function "nonmissing()"
nonmiss <- rep(TRUE, 47)
nonmiss[1] <- FALSE

indices <- 1:NROW(data) # if every observation was useable
indices <- indices[nonmiss] # exclude the missing ones

orig_data <- model$cons$data
nonmiss <- rep(TRUE, 47)
nonmiss[1] <- FALSE

indices <- 1:NROW(orig_data) # if every observation was useable
indices <- indices[nonmiss] # exclude the missing ones

resamples <- nonparametric(indices = indices, R = 3)

#eval(sys)
for (r in 1:3) {

  data <- nonparametric_resampling(df = orig_data, resample = resamples[[r]])
  new_model <- eval(sys)
  print(new_model)

}
# new_model is the last resample model, check whether worked
df <- orig_data[resamples[[3]],]
model3 <- outlier_detection(data = df, formula = formula,
                           ref_dist = "normal", sign_level = 0.05,
                           initial_est = "robustified", iterations = 3)
identical(model3$model, new_model$model) # TRUE -> seems to work, awesome!!!

btrap <- case_resampling(model, R = 3000)

original <- model$model$m3$coefficients["Education"]
mean(btrap$output[, "Education"])

# proper setting to check whether the model works
p <- generate_param(2, 1, 1)
d <- generate_data(p, 30000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
p$params$beta
base$model$m0$coefficients
base$model$m1$coefficients
beta_inf(base, iteration = 1, exact = TRUE)

b <- case_resampling(base, R = 10000)
b$output
colMeans(b$output)
apply(b$output, 2, sd)

# actual coefficients in the model
# 0.66 0.71 0.46
# estimates form the full model m0
# 0.6512028   0.7045501   0.4704196
# estimates from the robust model m1
# 0.6456637   0.6981663   0.4788257
# average of 10000 bootstrap estimates, 30000 datapoints in the sample
# 0.6454732   0.6983143   0.4784703 (close to m1 model baseline)
# theoretical inference on the coefficients: H0Std. Errors
# 0.04611505  0.04049706  0.06374668
# bootstrap s.e.
# 0.04726939  0.04183003  0.06586835
# seems to work pretty well as soon as the sample size is large enough

# check with contamination, just as a toy example
# 30000 observations, contaminate 5% = 1500
# add deterministic error of let's say +/- 3

add.pos <- rep(c(3, rep(0, 1499)))
add.neg <- rep(c(-3, rep(0, 1499)))
add.outliers <- c(add.pos, add.neg)
add.outliers <- rep(add.outliers, 10)
length(add.outliers) # 30000
sum(add.outliers) # 0

# data[, "y"] + add.outliers # works
data$y <- data$y + add.outliers

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
p$params$beta
base$model$m0$coefficients
base$model$m1$coefficients
beta_inf(base, iteration = 1, exact = TRUE)

b <- case_resampling(base, R = 10000)
b$output
colMeans(b$output)
apply(b$output, 2, sd)
# bootstrap se only slightly larger

# try only positive outliers

add.pos <- rep(c(4, rep(0, 1499)))
add.outliers <- rep(add.pos, 20)
length(add.outliers) # 30000
sum(add.outliers) # 80

# data[, "y"] + add.outliers # works
p <- generate_param(2, 1, 1)
d <- generate_data(p, 30000)
data <- d$data
data$y <- data$y + add.outliers

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
p$params$beta
base$model$m0$coefficients
base$model$m1$coefficients
beta_inf(base, iteration = 1, exact = TRUE)

b <- case_resampling(base, R = 10000)
b$output
colMeans(b$output)
apply(b$output, 2, sd)
# H0Std. Error
# 0.04603163  0.04041093  0.06358844
# bootstrap se
# 0.04690651  0.04155295  0.06539450
# again, bootstrap s.e. only slightly larger
# try a more realistic number of resamples
b <- case_resampling(base, R = 200)
b$output
colMeans(b$output)
apply(b$output, 2, sd)
# bootstrap se underestimate variance? but even if so...not by much
# 0.04382863  0.03878504  0.06093026


# test parallel
p <- generate_param(2, 1, 1)
d <- generate_data(p, 7500)
data <- d$data

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)

set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 10000)
end <- timestart <- proc.time()
print(end - start)
apply(do1$output, 2, sd)
stopCluster(cl)

set.seed(23)
library(doFuture)
registerDoFuture()
plan(sequential)
start <- timestart <- proc.time()
do2 <- case_resampling(base, R = 10000)
end <- timestart <- proc.time()
print(end - start)
apply(do2$output, 2, sd)

# parallel output is weird, check
p <- generate_param(2, 1, 1)
d <- generate_data(p, 7500)
data <- d$data

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
set.seed(23)
library(doFuture)
registerDoFuture()
plan(sequential)
start <- timestart <- proc.time()
do2 <- case_resampling(base, R = 100)
end <- timestart <- proc.time()
print(end - start)
apply(do2$output, 2, sd)
# does it work without setting up any parallel structure?
p <- generate_param(2, 1, 1)
d <- generate_data(p, 7500)
data <- d$data

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
do2 <- case_resampling(base, R = 100)

# # test new parallel argument
p <- generate_param(2, 1, 1)
d <- generate_data(p, 7500)
data <- d$data

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)

set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 3000, parallel = TRUE)
end <- timestart <- proc.time()
print(end - start)
apply(do1$output, 2, sd)
stopCluster(cl)

set.seed(23)
library(doFuture)
registerDoFuture()
plan(sequential)
start <- timestart <- proc.time()
do2 <- case_resampling(base, R = 3000, parallel= TRUE)
end <- timestart <- proc.time()
print(end - start)
apply(do2$output, 2, sd)

set.seed(23)
start <- timestart <- proc.time()
do3 <- case_resampling(base, R = 3000, parallel= FALSE)
end <- timestart <- proc.time()
print(end - start)
apply(do3$output, 2, sd)

# # extracting several m; not needed, found a direct solution using sapply
#
# extract <- function(x) {
#
#   a <- x[["coefficients"]]
#   return(a)
#
# }

base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)


a <- sapply(X = 0:5, FUN = outliers_prop, robust2sls_object = base)
b <- sapply(X = 1:6, FUN = function(robust2sls_object, iteration) robust2sls_object$model[[iteration]][["coefficients"]], robust2sls_object = base)

NROW(a)
dim(b)


outliers_prop(base, 0)
outliers_prop(base, 1)

o <- sapply(X = base$model, FUN = extract)

p <- generate_param(2, 1, 1)
d <- generate_data(p, 7500)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(23)
start <- timestart <- proc.time()
do3 <- case_resampling(base, R = 5000, parallel= FALSE)
end <- timestart <- proc.time()
duration3 <- end - start
print(duration3)

# recorded all: 3 coefficients and gauge; for m = 0:5 per r; total 6*500 = 3000 records
NROW(do3$output) # 3000
NROW(do3$resamples) # 500 because did 500 resamples

set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)

NROW(do1$output) # 3000

identical(do1$output, do3$output) # TRUE, so both parallel and not produce the same output

# 5000 replications took approx 4.5 minutes sequentially and approx. 2 minutes in parallel
# so e.g. if want to do Monte Carlo to assess bootstrap (in this case the MC replication
# is in parallel and not the bootstrap) with M = 10,000 then upper bound is
# 45,000 minutes if all sequentially
# if can do in parallel (e.g. on compute server more likely to cut to 1/10)
# then 4,500 minutes -> 75 hours, approx 3 days
# this used a dataset size of 7,500 -> is it faster if the sample size is considerably smaller?

p <- generate_param(2, 1, 1)
d <- generate_data(p, 300)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(23)
start <- timestart <- proc.time()
do3 <- case_resampling(base, R = 5000, parallel= FALSE)
end <- timestart <- proc.time()
duration3 <- end - start
print(duration3)

# recorded all: 3 coefficients and gauge; for m = 0:5 per r; total 6*5000 = 30000 records
NROW(do3$output) # 30000
NROW(do3$resamples) # 5000 because did 5000 resamples

set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)

NROW(do1$output) # 30000

identical(do1$output, do3$output) # TRUE, so both parallel and not produce the same output
# yes is faster
# approx. 2.5 minutes sequentially and 1 minute in parallel (factor of 0.5)

b1 <- do1$output[do1$output$m == "m1", ]
NROW(b1) # 5000 because now only coefficients and gauge for first iteration
colMeans(b1[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b1[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
# what is theoretical avar of this setup?
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5)
avar_theory <- gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 300
avar_theory


# enough R?
set.seed(12345)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do5 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration5 <- end - start
print(duration1)
stopCluster(cl)

b5 <- do5$output[do1$output$m == "m1", ]
NROW(b5) # 5000 because now only coefficients and gauge for first iteration
colMeans(b5[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b5[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)


# run higher R
set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 15000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)

b1 <- do1$output[do1$output$m == "m1", ]
NROW(b1) # 15000 because now only coefficients and gauge for first iteration
colMeans(b1[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b1[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)



p <- generate_param(2, 1, 1)
d <- generate_data(p, 30000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)
b1 <- do1$output[do1$output$m == "m1", ]
NROW(b1) # 5000 because now only coefficients and gauge for first iteration
colMeans(b1[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b1[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 30000

set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 15000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)
b1 <- do1$output[do1$output$m == "m1", ]
NROW(b1) # 5000 because now only coefficients and gauge for first iteration
colMeans(b1[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b1[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 30000



p <- generate_param(2, 1, 1)
d <- generate_data(p, 100000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(23)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do1 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration1 <- end - start
print(duration1)
stopCluster(cl)
b1 <- do1$output[do1$output$m == "m1", ]
NROW(b1) # 5000 because now only coefficients and gauge for first iteration
colMeans(b1[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b1[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 100000
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5) / 100000




p <- generate_param(2, 1, 1)
d <- generate_data(p, 10000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 2, shuffle_seed = 5)
set.seed(12345)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do5 <- case_resampling(base, R = 5000, parallel = TRUE)
end <- timestart <- proc.time()
duration5 <- end - start
print(duration1)
stopCluster(cl)

b5 <- do5$output[do5$output$m == "m1", ]
NROW(b5) # 5000 because now only coefficients and gauge for first iteration
colMeans(b5[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b5[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 10000

b6 <- do5$output[do5$output$m == "m0", ]
apply(b6[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5) / 10000

p <- generate_param(2, 1, 1)
d <- generate_data(p, 100)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 + x3 | x2 + z3, sign_level = 0.05,
                          initial_est = "robustified", iterations = 2, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
do5 <- case_resampling(base, R = 50000, parallel = TRUE)
end <- timestart <- proc.time()
duration5 <- end - start
print(duration5)
stopCluster(cl)

b5 <- do5$output[do5$output$m == "m1", ]
NROW(b5) # 50000 because now only coefficients and gauge for first iteration
colMeans(b5[, c("X.Intercept.", "x2", "x3", "gauge")])
base$model$m1$coefficients
apply(b5[, c("X.Intercept.", "x2", "x3", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 100
# get huge bootstrap s.e. > 200 all
# but theoretical ones are all between 0.7 and 0.9 (corrected)
# true intercept: 0.66
max(b5[, "X.Intercept."]) # 65214
min(b5[, "X.Intercept."]) # -11195
which(b5[, "X.Intercept."] > 65214) # row 28108
which(b5[, "X.Intercept."] < -11195) # row 14522
b5[28108, ] # this is r 28108
do5$output[84323, ] # this is r 28108
b5[14522, ] # this is r 14522
sum(b5[, "X.Intercept."] > 10)
# look at those resamples
do5$resamples[[28108]]
a <- count_indices(do5$resamples[c(28108, 14522)], 1:100)
a$count_all # doesn't even look that strange

# find nice example of parameter values

# y = 2 + 4 x2 (endog) + u
# 1 = 1 + 0 z2 + 0
# x2 = 2 + 3 z2 + r2


p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 10000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 2, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt <- case_resampling(base, R = 50000, parallel = TRUE)
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

bt1 <- bt$output[bt$output$m == "m1", ]
NROW(bt1) # 50000 because now only coefficients and gauge for first iteration
colMeans(bt1[, c("X.Intercept.", "x2", "gauge")])
base$model$m1$coefficients
apply(bt1[, c("X.Intercept.", "x2", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 10000)

bt0 <- bt$output[bt$output$m == "m0", ]
NROW(bt0) # 50000 because now only coefficients and gauge for first iteration
colMeans(bt0[, c("X.Intercept.", "x2", "gauge")])
base$model$m0$coefficients
apply(bt0[, c("X.Intercept.", "x2", "gauge")], 2, sd)
summary(base$model$m0)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5) / 10000)
# now works! sqrt()


p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 100)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 2, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt <- case_resampling(base, R = 50000, parallel = TRUE)
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

bt1 <- bt$output[bt$output$m == "m1", ]
NROW(bt1) # 50000 because now only coefficients and gauge for first iteration
colMeans(bt1[, c("X.Intercept.", "x2", "gauge")])
base$model$m1$coefficients
apply(bt1[, c("X.Intercept.", "x2", "gauge")], 2, sd)
beta_inf(base, iteration = 1, exact = TRUE)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 100)

bt0 <- bt$output[bt$output$m == "m0", ]
NROW(bt0) # 50000 because now only coefficients and gauge for first iteration
colMeans(bt0[, c("X.Intercept.", "x2", "gauge")])
base$model$m0$coefficients
apply(bt0[, c("X.Intercept.", "x2", "gauge")], 2, sd)
summary(base$model$m0)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5) / 100)


p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 200)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 2, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt <- case_resampling(base, R = 1000, parallel = TRUE, coef = "x2")
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

m0 <- extract_boot(bt, 0)






p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 200)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = "convergence", convergence_criterion = 0, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt <- case_resampling(base, R = 1000, parallel = TRUE, coef = NULL, m = "convergence")
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)


f <- function(object) {

  a <- 2 * outliers_prop(robust2sls_object = object, iteration = 0)
  return(a)

}
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 200)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt <- case_resampling(base, R = 1000, parallel = TRUE, coef = NULL, m = c(0,1))
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

m0 <- extract_boot(bt, 0)
apply(m0[, c("X.Intercept.", "x2", "gauge")], 2, sd)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 0, parameters = p, split = 0.5) / 200)
m1 <- extract_boot(bt, 1)
apply(m1[, c("X.Intercept.", "x2", "gauge")], 2, sd)
sqrt(gauge_avar_mc(ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iteration = 1, parameters = p, split = 0.5) / 200)


# test new functions
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 200)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
start <- timestart <- proc.time()
bt200 <- case_resampling(base, R = 10000, parallel = TRUE, coef = NULL, m = NULL)
end <- timestart <- proc.time()
stopCluster(cl)

b200 <- evaluate_boot(bt200, 0:5)

p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 50)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
bt50 <- case_resampling(base, R = 10000, parallel = TRUE, coef = NULL, m = NULL)
end <- timestart <- proc.time()
stopCluster(cl)

b50 <- evaluate_boot(bt50, 0:5)

p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 1000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 5, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
bt1000 <- case_resampling(base, R = 1000, parallel = TRUE, coef = NULL, m = NULL)
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

b1000 <- evaluate_boot(bt1000, 0:5)

# convergence
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 1000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = "convergence", convergence_criterion = 0, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
bt1000 <- case_resampling(base, R = 10000, parallel = TRUE, coef = NULL, m = "convergence")
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

b1000 <- evaluate_boot(bt1000, "convergence")








# convergence
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 10000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = "convergence", convergence_criterion = 0, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
bt10000 <- case_resampling(base, R = 10000, parallel = TRUE, coef = NULL, m = "convergence")
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

b10000 <- evaluate_boot(bt10000, "convergence")








p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 2, mean_z = 1, cov_z = matrix(1), Sigma2_half = matrix(2), Omega2 = matrix(1/2), Pi = t(matrix(c(1, 2, 0, 3), nrow = 2)))
d <- generate_data(p, 1000)
data <- d$data
base <- outlier_detection(data = data, formula = y ~ x2 | z2, sign_level = 0.05,
                          initial_est = "robustified", iterations = 1, shuffle_seed = 5)
set.seed(123)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
clusterCall(cl = cl, function(x) .libPaths(x), .libPaths())
bt1000 <- case_resampling(base, R = 10000, parallel = TRUE, coef = NULL, m = 1)
end <- timestart <- proc.time()
duration <- end - start
print(duration)
stopCluster(cl)

b1000 <- evaluate_boot(bt1000, iterations = 1)
sqrt(gauge_avar_mc("normal", 0.05, "robustified", 1, p = p, split = 0.5) / 1000)
