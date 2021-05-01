
library(r2sls)

p <- generate_param(1,1,1)
set.seed(123)
d <- generate_data(p, 100)

model <- outlier_detection(data = d$data, formula = y ~ x1 + x2 | x1 + z2, ref_dist = "normal", sign_level = 0.05, initial_est = "robustified", iterations = 3, split = 0.5, shuffle_seed = 2)
model$sel$m0
outliers(model,0) # 7 outliers
outliers_prop(model,0)

model$model$m0$nobs # 100 used initially
model$model$m1$nobs # then 93 used, so excluding the 7 from step 0

outliers(model,1) # still 7 observations classified as outliers
identical(model$type$m0, model$type$m1) # TRUE, so the same observations were classified as outliers as before, so from here all models stay the same
identical(model$stdres$m0, model$stdres$m1) # not identical, which is good because it means that they were re-calculated
model$type$m1
model$type$m0
