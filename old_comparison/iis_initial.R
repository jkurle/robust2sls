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
formula <- y ~ -1+x1+x2 | -1+x1+z2

iismodel <- iis_init(data = d, formula = formula, gamma = gamma)

# check what happens under missings
d2 <- d
d2[1, "y"] <- NA

iismodel2 <- iis_init(data= d2, formula = formula, gamma = gamma)
# I think gets::isat might not even allow for missing values in y or x
# hm, doc says that it is allowed at the beginning or end, because is removed with na.trim()
# not sure how we could detect missings in z though because isat probably only checks y and x and not z
# ask Genaro how to cope with that; is not really sensible to deal with it that way

# ok, error occurs in my function getsFun() and I think it occurs when I run cbind of y, x, z
# but dimensions of y and x differ after removing NA values there; would have to remove same rows in z

# two options: check for missings before running ivisat (later no problem anymore because ivreg can handle missings in x, y, and z because takes whole df)
# or modify getsFun to check whether different number of rows in the three objects but will be difficult to rectify because do not know which rows have been deleted (would have to find out by comparison but that could take really long for large datasets; also, could break the code since is workhorse function)
# prefer option 1 for now

d3 <- d
d3[1, "z2"] <- NA
iismodel3 <- iis_init(data = d3, formula = formula, gamma = gamma)

# also, need to urgently test whether t-test on impulse is equivalent to testing standardised residual in a split regression
# check this manually, e.g. split half with 100 observations
