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

