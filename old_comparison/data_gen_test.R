library(ggplot2) # to draw graphs
library(assertthat) # to test the inputs of the function
library(AER) # for ivreg command
library(foreign) # just to write Stata .dta file
library(datasets) # just to have more datasets to test
library(mvtnorm) # multivariate normal (and t) --> seems to have dropped this function in more recent versions?
library(matrixcalc) # matrix functions such as checking positive definiteness etc.
library(MASS) # multivariate normal sampling
library(pracma) # for roots of matrices
library(expm) # for matrix roots and exponentiation
library(dplyr) #
library(tidyr) #
library(doFuture)
library(doRNG)
library(tidyverse)

param_gen_2SLS <- function(dx1, dx2, dz2, intercept = TRUE, mean_z = NULL, var_z = NULL, sigma, Sigma_half = NULL, Omega = NULL, beta = NULL, Pi = NULL, seed = 42) {
  # dx1 = dimension of exogenous regressors excluding constant/intercept; also equals dz1, because also used as instruments
  # dx2 = dimension of endogenous regressors
  # dz2 = dimension of "outside"/excluded instruments
  # intercept = TRUE means the first element of dx1 (and also beta) is modelled as a deterministic intercept, not a random variable to be drawn
  # mean_z = vector of length dz = dz1 + dz2 specifying the mean of the exogenous variables
  # var_z = variance-covariance matrix of exogenous variables
  # sigma = standard deviation of structural error
  # Sigma = variance-covariance matrix of dimension dx2 of first stage errors that are random
  # Omega = vector of length dx1 specifying correlation between (scaled) random first stage error and structural error
  # beta = vector of length dx specifying the parameters of the structural equation
  # Pi = matrix of dimensions dz by dx specifying the first stage parameter matrix
  # seed = number to set random seed)

  set.seed(seed)

  dz1 <- dx1
  dx <- dx1 + dx2
  dz <- dz1 + dz2

  if (is.null(Sigma_half)) { # if Sigma not specified, create random Sigma matrix that is positive definite
    Sigma_half <- matrix(runif(dx2^2)*2-1, ncol=dx2)
    Sigma <- Sigma_half %*% t(Sigma_half) # var-cov matrix of first stage errors
    Sigma_half <- expm(1/2 * logm(Sigma))
    Sigma_half <- round(Sigma_half, digits=2)
    Sigma <- Sigma_half %*% Sigma_half
  }

  Sigma <- Sigma_half %*% Sigma_half

  if (is.null(Omega)) { # if Omega is not specified, create random Omega vector
    Omega <- matrix(runif(dx2)*2-1, ncol = 1) # covariance of standardised first and second stage errors
    Omega <- round(Omega, digits=2) # needed to avoid numerical imprecision later
  }

  # creating matrix for correlation and mean of first&second-stage errors
  A_scaled <- cbind(1, t(Omega))
  A_scaled <- rbind(A_scaled, cbind(Omega, diag(dx2))) # variance-covariance matrix for the standardised first & second stage errors

  scale_matrix <- cbind(sigma, matrix(0,1,dx2))
  scale_matrix <- rbind(scale_matrix, cbind(matrix(0,dx2,1), Sigma_half))
  A_unscaled <- scale_matrix %*% A_scaled %*% t(scale_matrix)

  mean_ru <- matrix(0, dx2+1, 1)

  # creating matrix for correlation and mean of z

  if (is.null(mean_z)) {

    if (intercept == TRUE) { # if the model includes an intercept, want to draw one exogenous variable less
      mean_z <- matrix(runif(dz-1),dz-1,1)
      mean_z <- round(mean_z, digits=2)
    } else { # if no intercept, then all dz variables are random, so draw all of them
      mean_z <- matrix(runif(dz),dz,1)
      mean_z <- round(mean_z, digits=2)
    }
  } # end if mean_z to be created

  if (is.null(var_z)) {

    if (intercept == TRUE) { # if the model includes an intercept, want to draw one exogenous variable less
      var_z_half <- matrix(runif((dz-1)^2)*2-1, ncol=(dz-1))
      var_z <- var_z_half %*% t(var_z_half) # var-cov matrix of instruments

      var_z_half <- expm(1/2 * logm(var_z))
      var_z_half <- round(var_z_half, digits=2)
      var_z <- var_z_half %*% var_z_half
    } else {
      var_z_half <- matrix(runif((dz)^2)*2-1, ncol=(dz))
      var_z <- var_z_half %*% t(var_z_half) # var-cov matrix of instruments

      var_z_half <- expm(1/2 * logm(var_z))
      var_z_half <- round(var_z_half, digits=2)
      var_z <- var_z_half %*% var_z_half
    }
  } # end if var_z to be created

  Ezz <- var_z + mean_z %*% t(mean_z)

  # creating first and second stage parameter vectors

  if (is.null(beta)) {
    beta <- matrix(runif(dx),dx,1)
    beta <- round(beta, digits=2) # needed to avoid numerical imprecision later
  }

  if (is.null(Pi)) {
    Pi <- cbind(diag(dx1), matrix(0,dx1,dz2))
    Pi1 <- t(matrix(runif(dx2*dz1)*2-1, ncol=dz1))

    pd_half <- matrix(runif(dx2*dx2)*2-1, ncol=dx2)
    pd <- pd_half %*% t(pd_half)
    pd_half <- expm(1/2 * logm(pd))
    pd_half <- round(pd_half, digits=2)
    pd <- pd_half %*% pd_half
    random <- matrix(runif(dx2*(dz2-dx2))*2-1, nrow=dx2, ncol=(dz2-dx2)) # if dz2 = dx2 then creates an empty 0x0 matrix
    Pi2 <- t(cbind(pd, random))
    Pi <- rbind(Pi, cbind(t(Pi1), t(Pi2)))
    Pi <- t(Pi) # to make consistent with notation in theory
    Pi <- round(Pi, digits=2) # needed to avoid numerical imprecision later
  }

  #creating vectors to represent non-correlation with z and the errors

  if (intercept == TRUE) {
    zero_zu <- matrix(0,dz-1,1)
    zero_zr <- matrix(0,dz-1,dx2)
  } else {
    zero_zu <- matrix(0,dz,1)
    zero_zr <- matrix(0,dz,dx2)
  }

  #combine all matrices for the then actual draw of z, u, r

  structural_cov <- cbind(A_unscaled, rbind(t(zero_zu),t(zero_zr)))
  structural_cov <- rbind(structural_cov, cbind(zero_zu, zero_zr, var_z))

  structural_mean <- rbind(mean_ru,mean_z)

  names_u <- "u"

  names_x1 <- c()
  names_x2 <- c()
  names_x <- c()
  names_z2 <- c()
  names_z <- c()
  names_r <- c()

  for (i in 1:dx1) { #"x1" is variable name of the first exogenous regressors (may be constant if intercept = TRUE, random if = FALSE)
    new <- paste("x", i, sep="")
    names_x1 <- cbind(names_x1, new)
  }

  for (i in (dx1+1):dx) {
    new <- paste("x", i, sep="")
    names_x2 <- cbind(names_x2, new)
  }

  names_x <- cbind(names_x1, names_x2)

  for (i in (dx1+1):dz) {
    new <- paste("z", i, sep="")
    names_z2 <- cbind(names_z2, new)
  }

  names_z <- cbind(names_x1, names_z2)

  for (i in 1:dx) {
    new <- paste("r", i, sep="")
    names_r <- cbind(names_r, new)
  }

  names_x1 <- as.character(names_x1)
  names_x2 <- as.character(names_x2)
  names_x <- as.character(names_x)
  names_z2 <- as.character(names_z2)
  names_z <- as.character(names_z)
  names_r <- as.character(names_r)

  #create formula for 2SLS in ivreg()
  x_fmla <- paste(names_x, collapse=" + ")
  z_fmla <- paste(names_z, collapse=" + ")
  fmla <- paste(c("y ~", x_fmla), collapse=" ")
  fmla <- paste(c(fmla, "|", z_fmla), collapse=" ")
  fmla <- as.formula(fmla)

  #return parameters

  return(list(structural_mean, structural_cov, Pi, beta, names_x1, names_x2, names_x, names_z2, names_z, names_u, names_r, fmla, Omega, Sigma_half, var_z, intercept, Ezz))

}

data_gen_2SLS <- function(structural_mean, structural_cov, Pi, beta, names_x1, names_x2, names_x, names_z2, names_z, names_u, names_r, intercept = TRUE, sample.size = 1000) {

  # structural_mean = mean vector for random draws
  # structural_cov = var-cov matrix for random draws
  # Pi = matrix of dimensions dz by dx specifying the first stage parameter matrix
  # beta = vector of length dx specifying the parameters of the structural equation
  # names_xx = character vector to name columns in dataframe
  # intercept = TRUE means the first element of dx1 (and also beta) is modelled as a deterministic intercept, not a random variable to be drawn
  # sample.size = number of observations to be drawn from multivariate normal

  dx1 <- length(names_x1)
  dx2 <- length(names_x2)
  dx <- length(names_x)
  dz1 <- dx1
  dz2 <- length(names_z2)
  dz <- length(names_z)

  zru <- mvrnorm(n = sample.size, structural_mean, structural_cov)

  #add intercept column if intercept = TRUE
  if (intercept == TRUE) {
    constant <- matrix(1,nrow=sample.size,ncol=1)
    zru <- cbind(zru[,1:(dx2+1)], constant, zru[,(dx2+2):ncol(zru)])
  }


  #extract the different parts of the matrices
  u <- as.matrix(zru[, 1])
  colnames(u) <- names_u

  R2 <- zru[, (2:(dx2+1))]
  R <- cbind(matrix(0,sample.size,dx1),R2)
  colnames(R) <- names_r
  Z <- zru[, (dx2+2):ncol(zru)]
  colnames(Z) <- names_z

  #create the matrix of regressors
  X <- Z %*% Pi + R
  colnames(X) <- names_x

  #create the vector of y
  y <- X %*% beta + u
  colnames(y) <- "y"

  #bundle all data
  data <- cbind(y, X, u, Z, R)
  data <- as.data.frame(data)

  #return final dataset
  return(list(data, beta, Pi))

}

p1 <- generate_param(3, 2, 3, seed = 42)
set.seed(42)
d1 <- generate_data(p1, n = 100)
avar0 <- gauge_avar_mc("normal", 0.01, "robustified", 0, p1)

p2 <- param_gen_2SLS(3, 2, 3, sigma = 1, seed = 42)
set.seed(42)
d2 <- data_gen_2SLS(p2[[1]], p2[[2]], p2[[3]], p2[[4]], p2[[5]], p2[[6]], p2[[7]], p2[[8]], p2[[9]], p2[[10]], p2[[11]], sample.size= 100)

identical(d1[[1]], d2[[1]]) # TRUE
