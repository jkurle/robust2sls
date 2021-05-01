
n <- 98
gamma <- 0.05
expected <- n*gamma
observed <- 7
alpha <- 0.05
cv_lower <- qpois(alpha/2, lambda = expected, lower.tail = TRUE)
cv_upper <- qpois(alpha/2, lambda = expected, lower.tail = FALSE)

p_two <- 2 * min(ppois(observed, lambda = expected, lower.tail = FALSE), ppois(observed, lambda = expected, lower.tail = TRUE))

qpois(p_two/2, lambda = expected, lower.tail = FALSE)
qpois(p_two/2, lambda = expected, lower.tail = TRUE)

poisson.test(x = observed, T = observed/n, r = expected, alternative = "two.sided")

ppois(observed, lambda = expected, lower.tail = TRUE)


cv_upper <- qpois(0.05, lambda = 5, lower.tail = FALSE)



haven::read_dta
read_dta("")

setwd("C:/Users/jonas/OneDrive - Nexus365/Econometric Game 2021")
read_dta("DataEG18.dta")
