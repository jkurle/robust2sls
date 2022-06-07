set.seed(10)
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                    mean_z = 0, cov_z = matrix(1),
                    Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                    Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
d <- generate_data(parameters = p, n = 50)$data

gamma <- 0.05
overid <- NULL
weak <- NULL
turbo <- FALSE
do.pet <- FALSE
normality.JarqueB <- NULL
formula <- y ~ 0+x1+x2|0+x1+z2
formula <- y ~ 1+x1+x2|1+x1+z2
formula <- y ~ -1+x1+x2 | -1+x1+z2
extract_formula(formula = formula)

nonmissing(data = d, formula = formula)

iismodel <- ivgets::ivisat(formula = formula, data = d, iis = TRUE,
                           sis = FALSE, tis = FALSE, uis = FALSE,
                           blocks = NULL, ratio.threshold = 0.8,
                           max.block.size = 30, t.pval = gamma,
                           wald.pval = t.pval, do.pet = do.pet,
                           # don't need these two tests b/c iid model
                           ar.LjungB = NULL, arch.LjungB = NULL,
                           normality.JarqueB = normality.JarqueB,
                           info.method = "sc", include.1cut = FALSE,
                           include.empty = FALSE, max.paths = NULL,
                           parallel.options = NULL, turbo = turbo,
                           tol = 1e-07, max.regs = NULL,
                           print.searchinfo = FALSE, plot = NULL,
                           alarm = FALSE, overid = overid, weak = weak)
# check what selection does currently (if hand this object)
#selection(data = d, yvar = "y", model = iismodel$final, cutoff = 1.96, bias_correction = NULL)
# decide to create a new selection function; existing one is residual-based, now is indicator-based

# model detected obs 19 as outlier
iismodel$selection$ISnames
# run model without obs 19
dwo19 <- d
dwo19 <- dwo19[-19, ]
modelwo19 <- ivreg::ivreg(formula = formula, data = dwo19)

library(waldo)
compare(iismodel$final$coefficients[-2], modelwo19$coefficients) # very similar
compare(iismodel$final$residuals[-19], modelwo19$residuals) # very similar

# check with more extreme example, blocking out first 10 of 50 observations
d1to10 <- d
d1to10 <- cbind(d1to10, diag(50)[, 1:10])
colnames(d1to10)[9:18] <- c("iis1", "iis2", "iis3", "iis4", "iis5", "iis6", "iis7", "iis8", "iis9", "iis10")
formulaiis <- y ~ -1+x1+iis1+iis2+iis3+iis4+iis5+iis6+iis7+iis8+iis9+iis10+x2 | -1+x1+iis1+iis2+iis3+iis4+iis5+iis6+iis7+iis8+iis9+iis10+z2
iis1to10 <- ivreg::ivreg(formula = formulaiis, data = d1to10)
dwo1to10 <- d
dwo1to10 <- dwo1to10[-c(1:10), ]
wo1to10 <- ivreg::ivreg(formula = formula, data = dwo1to10)
compare(iis1to10$coefficients[-c(2:11)], wo1to10$coefficients) # almost identical
compare(iis1to10$residuals[-c(1:10)], wo1to10$residuals) # almost identical
compare(iis1to10$sigma, wo1to10$sigma) # almost identical

# check whether t-statistic is actually the standardised residual
res <- d[1:10, "y"] - predict(wo1to10, newdata = d[1:10, ])
stdres <- res / wo1to10$sigma

sqrt(mean(wo1to10$residuals^2)*40/wo1to10$df.residual)
wo1to10$sigma # so is already the df corrected estimate of sigma

stdres2 <- res / (wo1to10$sigma*sqrt(wo1to10$df.residual/40))
# coefficient estimates of the impulses is their residual in the other model
# but using standardised residuals versus test on coefficient is slightly different
# eg with coefficient, get a different standard error for each coefficient
# would orthogonalising help?

# check what happens under missings
d2 <- d
d2[1, "y"] <- NA

iismodel2 <- iis_init(data= d2, formula = formula, gamma = gamma)

d2 <- d
d2[4, "y"] <- NA

iismodel2 <- iis_init(data= d2, formula = formula, gamma = gamma)
# I think gets::isat might not even allow for missing values in y or x
# hm, doc says that it is allowed at the beginning or end, because is removed with na.trim()
# not sure how we could detect missings in z though because isat probably only checks y and x and not z
# ask Genaro how to cope with that; is not really sensible to deal with it that way

# ok, error occurs in my function ivregFun() and I think it occurs when I run cbind of y, x, z
# but dimensions of y and x differ after removing NA values there; would have to remove same rows in z

# two options: check for missings before running ivisat (later no problem anymore because ivreg can handle missings in x, y, and z because takes whole df)
# or modify getsFun to check whether different number of rows in the three objects but will be difficult to rectify because do not know which rows have been deleted (would have to find out by comparison but that could take really long for large datasets; also, could break the code since is workhorse function)
# prefer option 1 for now

d3 <- d
d3[1, "z2"] <- NA
iismodel3 <- iis_init(data = d3, formula = formula, gamma = gamma)

# also, need to urgently test whether t-test on impulse is equivalent to testing standardised residual in a split regression
# check this manually, e.g. split half with 100 observations
