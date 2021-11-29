#' Parameters of 2SLS model (Monte Carlo)
#'
#' By default, \code{generate_param} creates random parameters of a 2SLS model
#' that satisfy conditions for 2SLS models, such as positive definite
#' variance-covariance matrices. The user can also specify certain parameters
#' directly, which are then checked for their validity.
#'
#' @param dx1 An integer value specifying the number of exogenous regressors.
#' This should include the intercept if it is present in the model
#' (see argument \code{intercept}).
#' @param dx2 An integer value specifying the number of endogenous regressors.
#' @param dz2 An integer value specifying the number of outside /
#' excluded instruments.
#' @param intercept A logical value (\code{TRUE} / \code{FALSE}) indicating
#' whether the model should contain an intercept.
#' @param sigma A strictly positive numeric value specifying the standard
#' deviation of the error in the structural model.
#' @param beta A numeric vector of length \code{dx1 + dx2} specifying the
#' parameters of the structural equation.
#' @param mean_z A numeric vector of length \code{dx1 + dz2} specifying the mean
#' of the exogenous variables, x1 and z2.
#' @param cov_z A numeric positive definite matrix specifying the
#' variance-covariance matrix of the exogenous variables, x1 and z2.
#' @param Omega2 A numeric vector of length \code{dx1} specifying the
#' correlation between the scaled random first stage error and the structural
#' error.
#' @param Sigma2_half A numeric positive definite matrix of dimension
#' \code{dx2} by \code{dx2} such that its square is the variance-covariance
#' matrix of the random first stage errors (Sigma2).
#' @param Pi A numeric matrix of dimension \code{(dx1 + dz2)} by
#' \code{(dx1 + dx2)} specifying the first stage parameter matrix.
#' @param seed An integer for setting the seed for the random number generator.
#'
#' @return \code{generate_param} returns a list with the (randomly created or
#' user-specified) parameters that are required for drawing random data that.
#' The parameters are generated to fulfill the 2SLS model assumptions.
#'
#' \describe{
#'   \item{\code{$structural}}{A list with two components storing the mean
#'   (\code{$mean}) and variance-covariance matrix (\code{$cov}) for the
#'   structural error (u), the random first stage errors (r2), and all
#'   instruments (excluding the intercept since it is not random) (z).}
#'   \item{\code{$params}}{A list storing the parameters of the 2SLS model.
#'   \code{$beta} is the coefficient vector (including intercept if present) of
#'   the structural equation, \code{$Pi} the coefficient matrix of the first
#'   stage projections, \code{$Omega2} the covariance between the structural
#'   error and the endogenous first stage errors, \code{$Sigma2_half} the square
#'   root of the variance-covariance matrix of the endogenous first stage
#'   errors, \code{$mean_z} the mean of all instruments (excluding the intercept
#'   since it is not random), \code{$cov_z} the variance-covariance matrix of
#'   the endogenous first-stage errors, \code{$Ezz} the expected value of the
#'   squared instruments.}
#'   \item{\code{$settings}}{A list storing the function call (\code{$call}),
#'   whether an intercept is included in the model (\code{$intercept}), a
#'   regression formula for the model setup (\code{$formula}), and the
#'   dimensions of the regressors and instruments (\code{$dx1}, \code{$dx2},
#'   \code{$dz2}.}
#'   \item{\code{$names}}{A list storing generic names for the regressors,
#'   instruments, and errors as character vectors (\code{$x1}, \code{$x2},
#'   \code{$x}, \code{$z2}, \code{$z}, \code{$r}, and \code{$u}).}
#' }
#'
#' @export

generate_param <- function(dx1, dx2, dz2, intercept = TRUE, beta = NULL,
                           sigma = 1, mean_z = NULL, cov_z = NULL,
                           Sigma2_half = NULL, Omega2 = NULL,  Pi = NULL,
                           seed = 42) {

  # capture function call
  cll <- sys.call()

  # check that the suggested packages required "pracma" package is installed

  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("Package 'pracma' needed for this function to work.
         Please install it.",
         call. = FALSE)
  }

  # check input values
  if (!is.numeric(dx1)) {
    stop(strwrap("Argument 'dx1' must be numeric", prefix = " ", initial = ""))
  }
  if (!identical(length(dx1), 1L)) {
    stop(strwrap("Argument 'dx1' must have length 1", prefix = " ",
                 initial = ""))
  }
  if (!(identical(dx1 %% 1, 0)) | !(dx1 >= 0)) {
    stop(strwrap("Argument 'dx1' must be a single numeric integer value >= 0",
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(dx2)) {
    stop(strwrap("Argument 'dx2' must be numeric", prefix = " ", initial = ""))
  }
  if (!identical(length(dx2), 1L)) {
    stop(strwrap("Argument 'dx2' must have length 1", prefix = " ",
                 initial = ""))
  }
  if (!(identical(dx2 %% 1, 0)) | !(dx2 >= 0)) {
    stop(strwrap("Argument 'dx2' must be a single numeric integer value >= 0",
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(dz2)) {
    stop(strwrap("Argument 'dz2' must be numeric", prefix = " ", initial = ""))
  }
  if (!identical(length(dz2), 1L)) {
    stop(strwrap("Argument 'dz2' must have length 1", prefix = " ",
                 initial = ""))
  }
  if (!(identical(dz2 %% 1, 0)) | !(dz2 >= 0)) {
    stop(strwrap("Argument 'dz2' must be a single numeric integer value >= 0",
                 prefix = " ", initial = ""))
  }
  if (!(dz2 >= dx2)) {
    stop(strwrap("Argument 'dz2' must be weakly larger than 'dx2'
                 (order condition)", prefix = " ", initial = ""))
  }
  if (!is.logical(intercept) | !identical(length(intercept), 1L)) {
    stop(strwrap("Argument 'intercept' must be a single logical value,
                 i.e. TRUE or FALSE", prefix = " ", initial = ""))
  }
  if (!is.null(beta)) {
    if (!is.numeric(beta)) {
      stop(strwrap("'beta' must be a numeric vector", prefix = " ",
                   initial = ""))
    }
    if (!identical(length(beta), as.integer(dx1+dx2))) {
      stop(strwrap("'beta' must have length dx1 + dx2", prefix = " ",
                   initial = ""))
    }
  }
  if (!identical(length(sigma), 1L)) {
    stop(strwrap("Argument 'sigma' must have length 1", prefix = " ",
                 initial = ""))
  }
  if (!is.numeric(sigma) | !(sigma > 0)) {
    stop(strwrap("Argument 'sigma' must be a numeric value strictly greater than
                 zero", prefix = " ", initial = ""))
  }
  if (!is.null(mean_z)) {
    if (!is.numeric(mean_z)) {
      stop(strwrap("Argument 'mean_z' must either be NULL or a numeric vector",
                   prefix = " ", initial = ""))
    }
    if (identical(intercept, TRUE)) { # intercept
      if (!identical(length(mean_z), as.integer(dx1+dz2-1))) {
        stop(strwrap("Length of 'mean_z' must be dx1 + dz2 - 1 when an intercept
                     is included in the model (because the intercept is
                     non-random)", prefix = " ", initial = ""))
      }
    } else { # no intercept
      if (!identical(length(mean_z), as.integer(dx1+dz2))) {
        stop(strwrap("Length of 'mean_z' must be dx1 + dz2 when no intercept
                     is included in the model", prefix = " ", initial = ""))
      }
    }
  } # end mean_z
  if (!is.null(cov_z)) {
    if (!is.numeric(cov_z)) {
      stop(strwrap("'cov_z' must be a numeric matrix", prefix = " ",
                   initial = ""))
    }
    if (identical(intercept, TRUE)) { # intercept
      if (!identical(dim(cov_z), as.integer(c(dx1 + dz2 - 1, dx1 + dz2 - 1)))) {
        stop(strwrap("'cov_z' must be a square matrix with dimensions
                     (dx1 + dz2 - 1) when an intercept is included in the model
                     (because the intercept is non-random)", prefix = " ",
                     initial = ""))
      }
    } else { # no intercept
      if (!identical(dim(cov_z), as.integer(c(dx1 + dx2, dx1 + dz2)))) {
        stop(strwrap("'cov_z' must be a square matrix with dimensions
                    (dx1 + dz2) when no intercept is included in the model",
                    prefix = " ", initial = ""))
      }
    }
    if (!pracma::isposdef(cov_z)) {
      stop(strwrap("'cov_z' must be positive definite", prefix = " ",
                   initial = " "))
    }
  } # end cov_z
  if (!is.null(Sigma2_half)) {
    if (!is.numeric(Sigma2_half)) {
      stop(strwrap("'Sigma2_half' must be a numeric matrix", prefix = " ",
                   initial = ""))
    }
    if (!identical(dim(Sigma2_half), as.integer(c(dx2, dx2)))) {
      stop(strwrap("'Sigma2_half' must be a square matrix with dimensions dx2",
                   prefix = " ", initial = ""))
    }
    if (!pracma::isposdef(Sigma2_half)) {
      stop(strwrap("'Sigma2_half' must be positive definite", prefix = " ",
                   initial = " "))
    }
  } # end Sigma2_half
  if (!is.null(Omega2)) {
    if (!is.numeric(Omega2)) {
      stop(strwrap("'Omega' must be a numeric vector", prefix = " ",
                   initial = ""))
    }
    if (!identical(dim(Omega2), as.integer(c(dx2, 1)))) {
      stop(strwrap("'Omega' must have dimensions dx2 by 1", prefix = " ",
                   initial = ""))
    }
  } # end Omega2
  if (!is.null(Pi)) {
    if (!is.numeric(Pi)) {
      stop(strwrap("'Pi' must be a numeric matrix", prefix = " ",
                   initial = ""))
    }
    if (!identical(dim(Pi), as.integer(c(dx1 + dz2, dx1 + dx2)))) {
      stop(strwrap("'Pi' must have dimensions dz by dx", prefix = " ",
                   initial = ""))
    }
    if (!identical(pracma::Rank(Pi), as.integer(dx1+dx2))) {
      stop(strwrap("'Pi' must have full rank, i.e. dx1+dx2", prefix = " ",
                   initial = ""))
    }
    Pi_test <- Pi
    colnames(Pi) <- NULL
    rownames(Pi) <- NULL
    if (!identical(as.matrix(Pi_test[1:dx1, 1:dx1]), diag(dx1))) {
      stop(strwrap("The dx1 by dx1 submatrix of Pi in the upper left corner
                   has to be the identity matrix because the exogenous
                   regressors can be exactly explained by themselves",
                   prefix = " ", initial = ""))
    }
  } # end Pi

  # initialise values needed throughout calculations
  dz1 <- dx1
  dx <- dx1 + dx2
  dz <- dz1 + dz2

  # set seed
  set.seed(seed)

  # create Sigma2 = variance-covariance matrix of the random first stage errors
  # remember that the first dx1 projection errors of the first stage are 0
  # because they can be prefectly explained by themselves, so are nonrandom
  # hence when generating first stage errors, only dx2 elements are random
  if (is.null(Sigma2_half)) { # not specified, create random pd Sigma2 matrix
    Sigma2_half <- matrix(stats::runif(dx2^2)*2-1, ncol=dx2) # not symm
    Sigma2 <- Sigma2_half %*% t(Sigma2_half) # symm
    Sigma2_half <- pracma::sqrtm(Sigma2)$B # symm
    Sigma2_half <- round(Sigma2_half, digits=2) # to avoid numeric inaccuracies
    Sigma2 <- Sigma2_half %*% Sigma2_half
  } else { # user-specified
    Sigma2 <- Sigma2_half %*% Sigma2_half
  }
  # create Sigma_half, which follows notation in paper
  Sigma_half <- Sigma2_half
  Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
  Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

  # create Omega2 = covariance vector between scaled / normalised random first
  # stage errors and the structural errors
  if (is.null(Omega2)) { # not specified, create random Omega2 vector
    Omega2 <- matrix(stats::runif(dx2)*2-1, ncol = 1)
    Omega2 <- round(Omega2, digits=2) # to avoid numeric inaccuracies
  }
  # create Omega
  Omega <- Omega2
  Omega <- rbind(matrix(0,dx1,1), Omega)

  # want to draw structural error and first stage errors together
  # covariance
  A_scaled <- cbind(1, t(Omega2))
  A_scaled <- rbind(A_scaled, cbind(Omega2, diag(dx2)))
  scale_matrix <- cbind(sigma, matrix(0,1,dx2))
  scale_matrix <- rbind(scale_matrix, cbind(matrix(0,dx2,1), Sigma2_half))
  A_unscaled <- scale_matrix %*% A_scaled %*% t(scale_matrix)
  # mean
  mean_ru <- matrix(0, dx2+1, 1)

  # prepare drawing exogenous variables (z)
  # intercept included -> draw one less exogenous variable because is non-random
  # mean
  if (is.null(mean_z)) {
    if (intercept == TRUE) {
      mean_z <- matrix(stats::runif(dz-1),dz-1,1)
    } else {
      mean_z <- matrix(stats::runif(dz),dz,1)
    }
    mean_z <- round(mean_z, digits=2) # to avoid numeric inaccuracies
  }
  # var-cov
  if (is.null(cov_z)) {
    if (intercept == TRUE) {
      cov_z_half <- matrix(stats::runif((dz-1)^2)*2-1, ncol=(dz-1))
    } else {
      cov_z_half <- matrix(stats::runif((dz)^2)*2-1, ncol=(dz))
    }
    cov_z <- cov_z_half %*% t(cov_z_half) # var-cov matrix of instruments
    cov_z_half <- pracma::sqrtm(cov_z)$B
    cov_z_half <- round(cov_z_half, digits=2) # to avoid numeric inaccuracies
    cov_z <- cov_z_half %*% cov_z_half
  }
  Ezz <- cov_z + mean_z %*% t(mean_z)
  # Mzz corresponds to the paper (is Ezz if no intercept; otherwise need to add)
  Mzz <- Ezz
  if (intercept == TRUE) {
    Mzz_sub <- Mzz
    Mzz <- cbind(1, t(mean_z))
    Mzz <- rbind(Mzz, cbind(mean_z, Mzz_sub))
  }

  # create structural equation parameter vector
  if (is.null(beta)) {
    beta <- matrix(stats::runif(dx), dx, 1)
    beta <- round(beta, digits=2) # to avoid numerical inaccuracies
  }

  # create first stage parameter vector
  if (is.null(Pi)) {
    Pi <- cbind(diag(dx1), matrix(0,dx1,dz2))
    Pi1 <- t(matrix(stats::runif(dx2*dz1)*2-1, ncol=dz1))
    pd_half <- matrix(stats::runif(dx2*dx2)*2-1, ncol=dx2)
    pd <- pd_half %*% t(pd_half)
    pd_half <- pracma::sqrtm(pd)$B
    pd_half <- round(pd_half, digits=2)
    pd <- pd_half %*% pd_half
    # NOTE: if dz2 = dx2 then the next line creates an empty 0x0 matrix
    random <- matrix(stats::runif(dx2*(dz2-dx2))*2-1, nrow=dx2, ncol=(dz2-dx2))
    Pi2 <- t(cbind(pd, random))
    Pi <- rbind(Pi, cbind(t(Pi1), t(Pi2)))
    Pi <- t(Pi) # to make consistent with notation in theory
    Pi <- round(Pi, digits=2) # to avoid numerical inaccuracies
  }

  # create vectors to represent non-correlation between
  # z and the (first stage and structural) errors
  if (intercept == TRUE) {
    zero_zu <- matrix(0,dz-1,1)
    zero_zr <- matrix(0,dz-1,dx2)
  } else {
    zero_zu <- matrix(0,dz,1)
    zero_zr <- matrix(0,dz,dx2)
  }

  # combine all matrices for the then actual draw of z, u, r
  structural_cov <- cbind(A_unscaled, rbind(t(zero_zu),t(zero_zr)))
  structural_cov <- rbind(structural_cov, cbind(zero_zu, zero_zr, cov_z))
  structural_mean <- rbind(mean_ru,mean_z)

  # create names for the regressors; names will be:
  # "x1", "x2", ... for the structural regressors
  # "z1", "z2", ... for the instruments
  # "r1", "r2", ... for the first stage projection errors
  # "u" for the structural error

  names_u <- "u"
  names_x1 <- character()
  names_x2 <- character()
  names_x <- character()
  names_z2 <- character()
  names_z <- character()
  names_r <- character()
  for (i in 1:dx1) {
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

  # remove the names of the elements of the vector
  names_x1 <- as.character(names_x1)
  names_x2 <- as.character(names_x2)
  names_x <- as.character(names_x)
  names_z2 <- as.character(names_z2)
  names_z <- as.character(names_z)
  names_r <- as.character(names_r)

  # create formula for 2SLS in ivreg()
  x_fmla <- paste(names_x, collapse=" + ")
  z_fmla <- paste(names_z, collapse=" + ")
  fmla <- paste(c("y ~", x_fmla), collapse=" ")
  fmla <- paste(c(fmla, "|", z_fmla), collapse=" ")
  fmla <- stats::as.formula(fmla)

  # Mxx_tilde
  Mxx_tilde <- t(Pi) %*% Mzz %*% Pi # says is symmetric and pd
  Mxx_tilde_inv <- pracma::inv(Mxx_tilde)

  # return parameters
  st <- list(mean = structural_mean, cov = structural_cov)
  par <- list(beta = beta, sigma = sigma, Pi = Pi, Omega2 = Omega2,
              Omega = Omega, Sigma2_half = Sigma2_half, Sigma_half = Sigma_half,
              mean_z = mean_z, cov_z = cov_z, Ezz = Ezz, Mzz = Mzz,
              Mxx_tilde_inv = Mxx_tilde_inv)
  set <- list(call = cll, intercept = intercept, formula = fmla, dx1 = dx1,
              dx2 = dx2, dz2 = dz2)
  nam <- list(x1 = names_x1, x2 = names_x2, x = names_x, z2 = names_z2,
                z = names_z, r = names_r, u = names_u)
  out <- list(structural = st, params = par, setting = set, names = nam)

  return(out)

}

#' Random data of 2SLS model (Monte Carlo)
#'
#' \code{generate_data} draws random data for a 2SLS model given the parameters.
#'
#' @param parameters A list with 2SLS model parameters as created by
#' \link{generate_param}.
#' @param n Sample size to be drawn.
#'
#' @return \code{generate_data} returns a data frame with \code{n} rows
#' (observations) and the following variables of the 2SLS model: dependent
#' variable y, exogenous regressors x1, endogenous regressors x2, structural
#' error u, outside instruments z2, first stage projection errors r1 (identical
#' to zero) and r2.
#'
#' @export

generate_data <- function(parameters, n) {

  dx1 <- parameters$setting$dx1
  dx2 <- parameters$setting$dx2
  dx <- dx1 + dx2
  dz1 <- parameters$setting$dx1
  dz2 <- parameters$setting$dz2
  dz <- dz1 + dz2

  # draw random data for u, r, z
  zru <- MASS::mvrnorm(n = n, mu = parameters$structural$mean,
                       Sigma = parameters$structural$cov)

  # add intercept column if intercept = TRUE
  if (parameters$setting$intercept == TRUE) {
    constant <- matrix(1, nrow= n, ncol = 1)
    zru <- cbind(zru[ ,1:(dx2+1)], constant, zru[ ,(dx2+2):ncol(zru)])
  }

  # extract the different parts of the matrices
  u <- as.matrix(zru[, 1])
  colnames(u) <- parameters$names$u

  R2 <- zru[ ,(2:(dx2+1))]
  R <- cbind(matrix(0, n, dx1), R2)
  colnames(R) <- parameters$names$r
  Z <- zru[ ,(dx2+2):ncol(zru)]
  colnames(Z) <- parameters$names$z

  # create the matrix of regressors
  X <- Z %*% parameters$params$Pi + R
  colnames(X) <- parameters$names$x

  # create the vector of y
  y <- X %*% parameters$params$beta + u
  colnames(y) <- "y"

  # bundle all data
  data <- cbind(y, X, u, Z, R)
  data <- as.data.frame(data)

  #return final dataset
  return(list(data = data, beta = parameters$params$beta,
              Pi = parameters$params$Pi))

}


#' Monte Carlo simulations parameter grid
#'
#' WARNING: not for average user - function not completed yet
#'
#' \code{mc_grid} runs Monte Carlo simulations to assess the performance of
#' the theory of the gauge, simple proportion tests, and count tests.
#'
#' @param M Number of replications.
#' @param n Sample size for each replication.
#' @param seed Random seed for the iterations.
#' @param parameters A list as created by \link{generate_param} that specifies
#' the true model.
#' @param formula A formula that specifies the 2SLS model to be estimated. The
#' format has to follow \code{y ~ x1 + x2 | x1 + z2}, where \code{y} is the
#' dependent variable, \code{x1} are the exogenous regressors, \code{x2} the
#' endogenous regressors, and \code{z2} the outside instruments.
#' @param ref_dist A character vector that specifies the reference distribution
#' against which observations are classified as outliers. \code{"normal"} refers
#' to the normal distribution.
#' @param sign_level A numeric value between 0 and 1 that determines the cutoff
#' in the reference distribution against which observations are judged as
#' outliers or not.
#' @param initial_est A character vector that specifies the initial estimator
#' for the outlier detection algorithm. \code{"robustified"} means that the full
#' sample 2SLS is used as initial estimator. \code{"saturated"} splits the
#' sample into two parts and estimates a 2SLS on each subsample. The
#' coefficients of one subsample are used to calculate residuals and determine
#' outliers in the other subsample. \code{"user"} allows the user to specify a
#' model based on which observations are classified as outliers.
#' @param iterations An integer >= 0 that specifies how often the outlier
#' detection algorithm is iterated and for which summary statistics will be
#' calculated. The value \code{0} means that outlier classification based on the
#' initial estimator is done. Alternatively, the character \code{"convergence"}
#' for iteration until convergence.
#' @param convergence_criterion A numeric value that determines whether the
#'   algorithm has converged as measured by the L2 norm of the difference in
#'   coefficients between the current and the previous iteration. Only used when
#'   argument \code{iterations} is set to \code{"convergence"}.
#' @param max_iter A numeric value >= 1 or NULL. If
#' \code{iterations = "convergence"} is chosen, then the algorithm is stopped
#' after at most \code{max_iter} iterations. If also a
#' \code{convergence_criterion} is chosen then the algorithm stops when either
#' the criterion is fulfilled or the maximum number of iterations is reached.
#' @param shuffle A logical value or \code{NULL}.
#' \code{initial_est == "saturated"}. If \code{TRUE} then the sample is shuffled
#' before creating the subsamples.
#' @param shuffle_seed An integer value that will set the seed for shuffling the
#' sample or \code{NULL}. Only used if \code{initial_est == "saturated"} and
#' \code{shuffle == TRUE}.
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split.
#' @param path A character string or \code{FALSE}. The simulation grid can save
#' the individual results of each different entry in the grid to this
#' location. Individual results not saved if argument set to \code{FALSE}.
#' @param verbose A logical value whether any messages should be printed.
#'
#' @section Details:
#' The following arguments can also be supplied as a vector of their type:
#' \code{n}, \code{sign_level}, \code{initial_est}, and \code{split}. This makes
#' the function estimate all possible combinations of the arguments. Note that
#' the initial estimator \code{"robustified"} is not affected by the argument
#' \code{split} and hence is not varied in this case.
#'
#' For example, specifying \code{n = c(100, 1000)} and
#' \code{sign_level = c(0.01, 0.05)} estimates four Monte Carlo experiments with
#' the four possible combinations of the parameters.
#'
#' The \code{path} argument allows users to store the \code{M} replication
#' results for all of the individual Monte Carlo simulations that are part of
#' the grid. The results are saved both as \code{.Rds} and \code{.csv} files.
#' The file name is indicative of the simulation setting.
#'
#' @return \code{mc_grid} returns a data frame with the results of the Monte
#' Carlo experiments. Each row corresponds to a specific simulation setup. The
#' columns record the simulation setup and its results. Currently, the average
#' proportion of detected outliers ("mean_gauge") and their variance
#' ("var_gauge") are being recorded. Moreover, the theoretical asymptotic
#' variance ("avar") and the ratio of simulated to theoretical variance -
#' adjusted by the sample size - are calculated ("var_ratio"). Furthermore,
#' tentative results of size and power for the tests are calculated.
#'
#' @import doRNG
#' @importFrom foreach %dopar% %do%
#' @importFrom stats poisson.test
#' @export

mc_grid <- function(M, n, seed, parameters, formula, ref_dist, sign_level,
                    initial_est, iterations, convergence_criterion = NULL,
                    max_iter = NULL, shuffle = FALSE, shuffle_seed = 10,
                    split = 0.5, path = FALSE, verbose = FALSE) {

  gamma <- sign_level

  # robustified does not vary with split, shuffle, shuffle_seed
  # so create two separate grids, then append them
  # initialise as empty data frames
  grid1 <- data.frame()
  grid2 <- data.frame()

  if ("robustified" %in% initial_est) {
    grid1 <- expand.grid(list(sample_size = n, sign_level = sign_level,
                              initial_est = "robustified", split = 0.5),
                         stringsAsFactors = FALSE)
  }
  if ("saturated" %in% initial_est) {
    grid2 <- expand.grid(list(sample_size = n, sign_level = sign_level,
                              initial_est = "saturated", split = split),
                         stringsAsFactors = FALSE)
  }
  grid <- rbind(grid1, grid2)

  # storing all results
  results_all <- data.frame()
  # start recording time
  timestart <- proc.time()

  if (verbose == TRUE) {
    cat("Total number of Monte Carlo experiments: ", NROW(grid), "\n")
    cat("Monte Carlo experiment: ")
  }

  if (is.numeric(iterations)) {

    for (i in 1:NROW(grid)) {

      if (verbose == TRUE) {
        cat(i, " ")
      }

      # which parameters in this run?
      n <- grid$sample_size[[i]]
      sign_level <- grid$sign_level[[i]]
      initial_est <- grid$initial_est[[i]]
      split <- grid$split[[i]]

      avar <- gauge_avar(ref_dist = ref_dist, sign_level = sign_level,
                         initial_est = initial_est, iteration = iterations,
                         parameters = parameters, split = split)

      # store results in a data frame
      results <- foreach::foreach(m = (1:M), .combine = "rbind",
                                  .options.RNG = seed) %dorng% {

        # draw random data of the 2SLS model, sample size n
        d <- generate_data(parameters = parameters, n = n)

        # run the model
        model <- outlier_detection(data = d$data, formula = formula,
                                   ref_dist = ref_dist, sign_level = sign_level,
                                   initial_est = initial_est, user_model = NULL,
                                   iterations = iterations,
                                   convergence_criterion = NULL,
                                   shuffle = shuffle, shuffle_seed = shuffle_seed,
                                   split = split, verbose = FALSE)

        # calculate metrics of interest
        num.outliers <- sum((model$type[[(iterations + 1)]] == 0))
        num.expected <- n * sign_level
        num.nonmissing <- n - sum((model$type[[(iterations + 1)]] == -1))
        gauge <- num.outliers / num.nonmissing
        p_est <- estimate_param_null(model)
        avar_est <- gauge_avar(ref_dist = ref_dist, sign_level = sign_level,
                               initial_est = initial_est,
                               iteration = iterations, parameters = p_est,
                               split = split)
        prop_t <- abs((gauge - sign_level) / sqrt((avar_est / n)))
        prop_p <- 2 * stats::pnorm(prop_t, lower.tail = FALSE)
        prop_reject_001 <- (prop_p < 0.01)
        prop_reject_005 <- (prop_p < 0.05)
        prop_reject_010 <- (prop_p < 0.1)
        count_p <- stats::poisson.test(x = num.outliers, r = num.expected,
                                       alternative = "two.sided")$p.value
        count_reject_001 <- (count_p < 0.01)
        count_reject_005 <- (count_p < 0.05)
        count_reject_010 <- (count_p < 0.1)

        data.frame(M, n, iterations, sign_level, num.outliers, num.expected,
                   gauge, avar, avar_est, prop_t, prop_p, prop_reject_001,
                   prop_reject_005, prop_reject_010, count_p, count_reject_001,
                   count_reject_005, count_reject_010)

      } # end foreach

      if (!(path == FALSE)) {

        # path should not end with a separator
        # use base R function file.path() because uses path separator for platform
        ending <- substr(x = path, start = nchar(path), stop = nchar(path))
        if (ending %in% c("/", "\\")) {
          stop("Argument 'path' should not end with a path separator.")
        }

        filename <- paste("M",M,"n",n,"g",sign_level,"i",initial_est,"s",split,sep = "")
        filename_csv <- paste("M",M,"n",n,"g",sign_level,"i",initial_est,"s",split,".csv",sep = "")
        pathi <- file.path(path, filename)
        pathi_csv <- file.path(path, filename_csv)
        saveRDS(results, file = pathi)
        utils::write.csv(results, file = pathi_csv)

      }

      mean_gauge <- mean(results$gauge)
      var_gauge <- stats::var(results$gauge)
      var_ratio <- (var_gauge / (avar/n))
      var_ratio2 <- var_gauge * n / avar

      mean_avar_est <- mean(results$avar_est)
      mean_prop_t <- mean(results$prop_t)
      mean_prop_p <- mean(results$prop_p)
      prop_size_001 <- mean(results$prop_reject_001)
      prop_size_005 <- mean(results$prop_reject_005)
      prop_size_010 <- mean(results$prop_reject_010)
      mean_count_p <- mean(results$count_p)
      count_size_001 <- mean(results$count_reject_001)
      count_size_005 <- mean(results$count_reject_005)
      count_size_010 <- mean(results$count_reject_010)

      res <- data.frame(M, n, iterations, sign_level, initial_est, split,
                        mean_gauge, avar, mean_avar_est, var_gauge, var_ratio, var_ratio2,
                        mean_prop_t, mean_prop_p, prop_size_001, prop_size_005,
                        prop_size_010, mean_count_p, count_size_001,
                        count_size_005, count_size_010)

      results_all <- rbind(results_all, res)

    } # end grid search
  } else if (identical(iterations, "convergence")) { # end if iteration is numeric

    for (i in 1:NROW(grid)) {

      if (verbose == TRUE) {
        cat(i, " ")
      }

      # which parameters in this run?
      n <- grid$sample_size[[i]]
      sign_level <- grid$sign_level[[i]]
      initial_est <- grid$initial_est[[i]]
      split <- grid$split[[i]]

      avar <- gauge_avar(ref_dist = ref_dist, sign_level = sign_level,
                         initial_est = initial_est, iteration = iterations,
                         parameters = parameters, split = split)

      # store results in a data frame
      results <- foreach::foreach(m = (1:M), .combine = "rbind",
                                  .options.RNG = seed) %dorng% {

        # draw random data of the 2SLS model, sample size n
        d <- generate_data(parameters = parameters, n = n)

        # run the model
        model <- outlier_detection(data = d$data, formula = formula,
                                   ref_dist = ref_dist, sign_level = sign_level,
                                   initial_est = initial_est, user_model = NULL,
                                   iterations = iterations,
                                   convergence_criterion = convergence_criterion,
                                   max_iter = max_iter,
                                   shuffle = shuffle, shuffle_seed = shuffle_seed,
                                   split = split, verbose = FALSE)

        last_it <- length(model$model)

        # calculate metrics of interest
        num.outliers <- sum((model$type[[last_it]] == 0))
        num.expected <- n * sign_level
        num.nonmissing <- n - sum((model$type[[(last_it)]] == -1))
        gauge <- num.outliers / num.nonmissing
        p_est <- estimate_param_null(model)
        avar_est <- gauge_avar(ref_dist = ref_dist, sign_level = sign_level,
                               initial_est = initial_est,
                               iteration = iterations, parameters = p_est,
                               split = split)
        prop_t <- abs((gauge - sign_level) / sqrt((avar_est / n)))
        prop_p <- 2 * stats::pnorm(prop_t, lower.tail = FALSE)
        prop_reject_001 <- (prop_p < 0.01)
        prop_reject_005 <- (prop_p < 0.05)
        prop_reject_010 <- (prop_p < 0.1)
        count_p <- stats::poisson.test(x = num.outliers, r = num.expected,
                                       alternative = "two.sided")$p.value
        count_reject_001 <- (count_p < 0.01)
        count_reject_005 <- (count_p < 0.05)
        count_reject_010 <- (count_p < 0.1)
        conv <- last_it-1

        data.frame(M, n, iterations, conv, sign_level, num.outliers,
                   num.expected, gauge, avar, avar_est, prop_t, prop_p,
                   prop_reject_001, prop_reject_005, prop_reject_010, count_p,
                   count_reject_001, count_reject_005, count_reject_010)

      } # end foreach

      if (!(path == FALSE)) {

        # path should not end with a separator
        # use base R function file.path() because uses path separator for platform
        ending <- substr(x = path, start = nchar(path), stop = nchar(path))
        if (ending %in% c("/", "\\")) {
          stop("Argument 'path' should not end with a path separator.")
        }

        filename <- paste("M",M,"n",n,"g",sign_level,"i",initial_est,"s",split,sep = "")
        filename_csv <- paste("M",M,"n",n,"g",sign_level,"i",initial_est,"s",split,".csv",sep = "")
        pathi <- file.path(path, filename)
        pathi_csv <- file.path(path, filename_csv)
        saveRDS(results, file = pathi)
        utils::write.csv(results, file = pathi_csv)

      }

      mean_gauge <- mean(results$gauge)
      var_gauge <- stats::var(results$gauge)
      var_ratio <- (var_gauge / (avar/n))
      var_ratio2 <- var_gauge * n / avar

      mean_avar_est <- mean(results$avar_est)
      mean_prop_t <- mean(results$prop_t)
      mean_prop_p <- mean(results$prop_p)
      prop_size_001 <- mean(results$prop_reject_001)
      prop_size_005 <- mean(results$prop_reject_005)
      prop_size_010 <- mean(results$prop_reject_010)
      mean_count_p <- mean(results$count_p)
      count_size_001 <- mean(results$count_reject_001)
      count_size_005 <- mean(results$count_reject_005)
      count_size_010 <- mean(results$count_reject_010)

      conv_freq <- as.list(table(results$conv))

      if (is.null(max_iter)) {max <- "NULL"} else {max <- max_iter}

      res <- data.frame(M, n, iterations, max, sign_level, initial_est, split,
                        mean_gauge, avar, mean_avar_est, var_gauge, var_ratio, var_ratio2,
                        mean_prop_t, mean_prop_p, prop_size_001, prop_size_005,
                        prop_size_010, mean_count_p, count_size_001,
                        count_size_005, count_size_010, conv_freq = I(list(conv_freq)))

      results_all <- rbind(results_all, res)

    } # end grid search

  } else { # not "convergence"

    stop("Argument iterations not correctly specified.")

  }

  # might want to make clear that robustified is independent of split
  # but then turns all of them to characters, so leave at 0.5 for now
  # results_all$split[results_all$initial_est == "robustified"] <- "NULL"

  timeend <- proc.time()
  duration <- timeend - timestart
  if (verbose == TRUE) {
    print(duration)
  }

  return(results_all)

}




