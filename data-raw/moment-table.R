# for the variance formula of the gauge, we need several moments. most of them
# are theoretically known but some may need to be simulated since simulations
# need a considerable amount of time, we may want to store some of the values of
# the moments for settings that might occur frequently so that other functions
# can cache them.

library(MASS)
library(doFuture)
library(doRNG)

# this function simulates the truncated covariance
sim_trunc_cov <- function(dx1, dx2, Omega2, gamma, ref = c("normal"), n, M,
                          seed = 5) {

  # technically, knowing Omega2 is enough because it already tells us the dim
  # but we need to know how many zeros to fill for the u*r2*ind part

  Omega2 <- as.matrix(Omega2, ncol = 1) # turn Omega2 into a column vector

  if (!identical(NROW(Omega2), as.integer(dx2))) {
    stop("Dimensions dx2 and Omega2 do not correspond.")
  }

  # convert gamma into c
  # currently only normal distribution is supported
  ref <- match.arg(ref, c("normal"))
  if (!identical(ref, "normal")) {
    stop("Only supports normal distribution as reference distribution so far.")
  } else {
    c <- qnorm(gamma/2, lower.tail = FALSE)
  }

  # build the variance covariance matrix for simulating the errors u and r2
  var_r2 <- diag(dx2) # var of standardised r2 always identity matrix
  varcov <- cbind(1, t(Omega2)) # var of u/sigma always 1
  varcov <- rbind(varcov, cbind(Omega2, var_r2))

  # build the mean vector
  mu <- matrix(rep(0, dx2+1), ncol = 1)

  # need to give names to r2 errors
  namenum <- (dx1+1):(dx1+dx2)
  namevec <- c("u", paste("r", namenum, sep = ""))
  nameindvec <- paste(paste("E", namevec, sep = ""), "uind", sep = "")

  # initialise storage for the averages
  Eruind_mat <- foreach (i = 1:M, .combine = rbind, .options.RNG = seed,
                         .export = c("n", "mu", "varcov", "namevec",
                                     "nameindvec", "c"), .packages = "MASS") %dorng% {
      dat <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = varcov))
      colnames(dat) <- namevec
      dat$ind <- ifelse(abs(dat$u) <= c, 1, 0)
      for (j in 2:(dx2+1)) {
        dat[, nameindvec[j]] <- dat[, "u"] * dat[, namevec[j]] * dat[, "ind"]
      }
      Eruind <- matrix(apply(X = dat[, nameindvec[-1], drop = FALSE],
                             MARGIN = 2, FUN = mean), nrow = 1)
    }

  # calculate average of averages
  w <- matrix(apply(X = Eruind_mat, MARGIN = 2, FUN = mean), nrow = 1)

  # pad with zeros for exogenous variables (errors deterministically zero)
  w <- cbind(matrix(rep(0, dx1), nrow = 1), w)

  # give names to output
  if (dx1 == 0) {
    r1ind <- NULL
  } else {
    r1ind <- paste(paste("E", paste("r", 1:dx1, sep = ""), sep = ""), "uind", sep = "")
  }
  colnames(w) <- c(r1ind, nameindvec[2:length(nameindvec)])
  return(w)

}

x <- matrix(data = list(), nrow = 5, ncol = 10)
colnames(x) <- c("dx1", "dx2", "gamma", "reference", "cutoff", "Omega2", "omega", "n", "M", "seed")
dx1 <- 1
dx2 <- 1
Omega2settings <- c(3/4, 1/2, 1/3, 1/6, 0)
gamma <- 0.05
ref <- "normal"
c <- qnorm(gamma/2, lower.tail = FALSE)
n <- 100000
M <- 10000
seed <- 5

registerDoFuture()
plan(cluster, workers = 4)

for (s in 1:5) {

  x[[s, "dx1"]] <- dx1
  x[[s, "dx2"]] <- dx2
  x[[s, "gamma"]] <- gamma
  x[[s, "reference"]] <- ref
  x[[s, "cutoff"]] <- c
  x[[s, "Omega2"]] <- Omega2settings[s]
  x[[s, "n"]] <- n
  x[[s, "M"]] <- M
  x[[s, "seed"]] <- seed
  x[[s, "omega"]] <- sim_trunc_cov(dx1=dx1, dx2=dx2, Omega2=Omega2settings[s],
                                   gamma=gamma, ref=ref, n=n, M=M, seed=seed)

}


y <- matrix(data = list(), nrow = 5, ncol = 10)
colnames(y) <- c("dx1", "dx2", "gamma", "reference", "cutoff", "Omega2", "omega", "n", "M", "seed")
dx1 <- 1
dx2 <- 1
Omega2settings <- c(3/4, 1/2, 1/3, 1/6, 0)
gamma <- 0.01
ref <- "normal"
c <- qnorm(gamma/2, lower.tail = FALSE)
n <- 100000
M <- 10000
seed <- 5

registerDoFuture()
plan(cluster, workers = 4)

for (s in 1:5) {

  y[[s, "dx1"]] <- dx1
  y[[s, "dx2"]] <- dx2
  y[[s, "gamma"]] <- gamma
  y[[s, "reference"]] <- ref
  y[[s, "cutoff"]] <- c
  y[[s, "Omega2"]] <- Omega2settings[s]
  y[[s, "n"]] <- n
  y[[s, "M"]] <- M
  y[[s, "seed"]] <- seed
  y[[s, "omega"]] <- sim_trunc_cov(dx1=dx1, dx2=dx2, Omega2=Omega2settings[s],
                                   gamma=gamma, ref=ref, n=n, M=M, seed=seed)

}

# save these two
trunc_cov <- rbind(x, y)

usethis::use_data(trunc_cov, internal = TRUE, overwrite = FALSE)
