
#' Number of outliers
#'
#' \code{outliers} calculates the number of outliers from a \code{"robust2sls"}
#' object for a given iteration.
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer >= 0 representing the iteration for which the
#' outliers are calculated.
#'
#' @return \code{outliers} returns the number of outliers  for a given iteration
#' as determined by the outlier-detection algorithm.
#'
#' @export

outliers <- function(robust2sls_object, iteration) {

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }

  if (iteration %% 1 != 0) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }

  if (iteration < 0) {
    stop(strwrap("The argument `iteration` has to be >= 0", prefix = " ",
                 initial = ""))
  }

  if (iteration > robust2sls_object$cons$iterations$actual) {
    stop(strwrap("The 'robust2sls' object has fewer iterations than argument
                 `iteration` specifies.", prefix = " ", initial = ""))
  }

  if (class(robust2sls_object) != "robust2sls") {
    stop(strwrap("The argument `robust2sls_object` does not have the correct
                 class", prefix = " ", initial = ""))
  }

  # add check that "robust2sls" object is valid? validator returns object though

  accessor <- paste("m", iteration, sep = "")
  num <- sum(robust2sls_object$type[[accessor]] == 0)
  return(num)

}

#' Proportion of outliers
#'
#' \code{outliers_prop} calculates the proportion of outliers relative to all
#' non-missing observations in the full sample from a \code{"robust2sls"} obejct
#' for a given iteration.
#'
#' @inheritParams outliers
#'
#' @return \code{outliers_prop} returns the proportion of outliers for a given
#' iteration as determined by the outlier-detection algorithm.
#'
#' @export

outliers_prop <- function(robust2sls_object, iteration) {

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }

  if (iteration %% 1 != 0) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }

  if (iteration > robust2sls_object$cons$iterations$actual) {
    stop(strwrap("The 'robust2sls' object has fewer iterations than argument
                 `iteration` specifies.", prefix = " ", initial = ""))
  }

  if (class(robust2sls_object) != "robust2sls") {
    stop(strwrap("The argument `robust2sls_object` does not have the correct
                 class", prefix = " ", initial = ""))
  }

  accessor <- paste("m", iteration, sep = "")
  num_out <- outliers(robust2sls_object = robust2sls_object,
                      iteration = iteration)
  num_nonmiss <- (NROW(robust2sls_object$cons$data) -
                    sum(robust2sls_object$type[[accessor]] == -1))
  prop <- num_out / num_nonmiss
  return(prop)

}

#' Outlier history of single observation
#'
#' \code{outlier} takes a \code{"robust2sls"} object and the index of a specific
#' observation and returns its history of classification across the different
#' iterations contained in the \code{"robust2sls"} object.
#'
#' @inheritParams outliers
#' @param obs An index (row number) of an observation
#'
#' @return \code{outlier} returns a vector that contains the 'type' value for
#' the given obsverations across the different iterations. There are three
#' possible values: 1 if the observations is judged to be an outlier, 0 if not,
#' and -1 if any of its x, y, or z values required for estimation is missing.
#'
#' @export

outlier <- function(robust2sls_object, obs) {

  iterations <- robust2sls_object$cons$iterations$actual
  hist <- numeric(length = 0)
  coln <- character(length = 0)

  for (i in seq_len(iterations + 1)) {

    acc <- paste("m", i-1, sep = "")
    hist <- c(hist, robust2sls_object$type[[acc]][[obs]])
    coln <- c(coln, acc)

  }

  hist <- t(as.matrix(hist, nrow = 1))
  rownames(hist) <- rownames(robust2sls_object$cons$data)[[obs]]
  colnames(hist) <- coln

  return(hist)

}

#' Asymptotic variance of gauge
#'
#' \code{gauge_avar} calculates the asymptotic variance of the gauge for a
#' given iteration using a given set of parameters (true or estimated).
#'
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
#' outliers in the other subsample.
#' @param iteration An integer >= 0 or character \code{"convergence"}
#' representing the iteration for which the outliers are calculated. Uses the
#' fixed point value if set to \code{"convergence"}.
#' @param parameters A list created by \link{generate_param} or
#' \link{estimate_param_null} that stores the parameters (true or estimated).
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split.
#'
#' @export

gauge_avar <- function(ref_dist = c("normal"), sign_level,
                       initial_est = c("robustified", "saturated"),
                       iteration, parameters, split) {
browser()
  if (!is.numeric(sign_level) | !identical(length(sign_level), 1L)) {
    stop(strwrap("Argument 'sign_level' must be a numeric vector of length 1",
                 prefix = " ", initial = ""))
  }
  if (!(sign_level > 0) | !(sign_level < 1)) {
    stop(strwrap("Argument 'sign_level' must lie strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }
  if (!is.character(ref_dist) | !identical(length(ref_dist), 1L)) {
    stop(strwrap("Argument 'ref_dist' must be a character vector of length 1",
                 prefix = " ", initial = ""))
  }
  # available reference distributions (so far only "normal"):
  ref_dist_avail <- c("normal")
  if (!(ref_dist %in% ref_dist_avail)) {
    stop(strwrap(paste(c("Argument 'ref_dist' must be one of the available
                 reference distributions:", ref_dist_avail), collapse = " "),
                 prefix = " ", initial = ""))
  }
  if (!(is.numeric(iteration) | identical(iteration, "convergence"))) {
    stop(strwrap("Argument 'iteration' must either be numeric or 'convergence'",
                 prefix = " ", initial = ""))
  }
  if (is.numeric(iteration)) { # following tests only if numeric
    if (!identical(length(iteration), 1L)) {
      stop(strwrap("Argument 'iteration' must be a numeric vector of length 1",
                   prefix = " ", initial = ""))
    }
    if (!(iteration %% 1 == 0)) {
      stop(strwrap("Argument 'iteration' must be an integer", prefix = " ",
                   initial = ""))
    }
    if (!(iteration >= 0)) {
      stop(strwrap("Argument 'iteration' must be weakly larger than 0",
                   prefix = " ", initial = ""))
    }
  }
  if (!is.numeric(split) | !identical(length(split), 1L)) {
    stop(strwrap("Argument 'split' must be a numeric vector of length 1",
                 prefix = " ", initial = ""))
  }
  if (!(split > 0) | !(split < 1)) {
    stop(strwrap("Argument 'split' must lie strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }
  if (!is.character(initial_est) | !identical(length(initial_est), 1L)) {
    stop(strwrap("Argument 'initial_est' must be a character vector of
                 length 1", prefix = " ", initial = ""))
  }
  # available initial estimators:
  initial_avail <- c("robustified", "saturated")
  if (!(initial_est %in% initial_avail)) {
    stop(strwrap(paste(c("Argument 'initial_est' must be one of the available
                 initial estimators:", initial_avail), collapse = " "),
                 prefix = " ", initial = ""))
  }

  if (initial_est == "robustified") {
    if (ref_dist == "normal") {

      # create parameters needed for calculating asymptotic variance
      gamma <- sign_level
      c <- stats::qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE)
      phi <- 1 - gamma
      f <- stats::dnorm(c, mean=0, sd=1)
      tau_c_2 <- phi - 2 * c * f
      tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f
      tau_2 <- 1
      tau_4 <- 3
      varsigma_c_2 <- tau_c_2 / phi
      sigma <- parameters$params$sigma
      Omega <- parameters$params$Omega
      w <- Omega * tau_c_2
      zeta_c_minus <- 2 * Omega * c
      Sigma_half <- parameters$params$Sigma_half
      Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

      if (iteration == 0) {

        # calculate asymptotic variance
        term1 <- gamma * (1 - gamma)
        term2 <- (c * f)^2 * (tau_4 - 1)
        term3 <- -2 * c * f * (1 - gamma - tau_c_2)
        term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*%
          Mxx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
        avar <- term1 + term2 + term3 + term4

      } else if (is.numeric(iteration)) { # m >= 1

        # calculate varrho parameters, already for iteration
        m <- iteration
        v <- varrho(sign_level = gamma, ref_dist = "normal",
                    iteration = iteration)
        vbb <- v$c$vbb
        vss <- v$c$vss
        vbxu <- v$c$vbxu
        vsuu <- v$c$vsuu
        vsb <- v$c$vsb
        vsxu <- v$c$vsxu

        # calculate asymptotic variance
        # to avoid mistakes, I don't implement the formula explicitly
        # instead, use vector notation of gauge and get avar by inner product

        # vector with scalars
        v1 <- rbind(1, -c*f*vss, -c*f*vsuu)
        # vector with vectors
        v21 <- t(vbb*zeta_c_minus + 2*c*f/tau_c_2*vsb*((c^2-varsigma_c_2)/2*zeta_c_minus - 2*c/phi*w) - 2*c*vss*Omega) %*% Sigma_half %*% Mxx_tilde_inv
        v22 <- t(vbxu*zeta_c_minus + 2*c*(vsxu*((c^2-varsigma_c_2)/2*zeta_c_minus - 2*c/phi*w) - vsuu/phi*w)) %*% Sigma_half %*% Mxx_tilde_inv
        v2 <- -f/sigma * t(cbind(v21, v22))

        # var-cov matrices
        var1 <- cbind(gamma*phi, phi-tau_c_2, 0)
        var1 <- rbind(var1, cbind(phi-tau_c_2, tau_4-1, tau_c_4-tau_c_2*varsigma_c_2))
        var1 <- rbind(var1, cbind(0, tau_c_4-tau_c_2*varsigma_c_2, tau_c_4-tau_c_2*varsigma_c_2))
        var2 <- cbind(Mxx_tilde_inv*sigma^2, Mxx_tilde_inv*sigma^2*tau_c_2)
        var2 <- rbind(var2, cbind(Mxx_tilde_inv*sigma^2*tau_c_2, Mxx_tilde_inv*sigma^2*tau_c_2))

        # calculate avar
        avar <- t(v1) %*% var1 %*% v1 + t(v2) %*% var2 %*% v2

      } else { # "convergence"

        # varrho parameters
        # varrho always also gives fixed point parameters so "iteration" does not matter
        # might change in future if varrho() takes also "convergence" as value
        v <- varrho(sign_level = gamma, ref_dist = "normal",
                    iteration = 1)
        vbb <- v$fp$vbb
        vss <- v$fp$vss
        vbxu <- v$fp$vbxu
        vsuu <- v$fp$vsuu
        vsb <- v$fp$vsb
        vsxu <- v$fp$vsxu

        # here use explicit formula because is not too complicated (varrho then not even needed)
        term1 <- gamma*(1-gamma) + (c*f/(tau_c_2 - c*(c^2-varsigma_c_2)*f))^2 * (tau_c_4 - tau_c_2 * varsigma_c_2)
        term2 <- tau_c_2 * (f / ((phi - 2*c*f)(tau_c_2 - c*(c^2-varsigma_c_2)*f)))^2 * t(2*c*w - tau_c_2*zeta_c_minus) %*%
          Sigma_half %*% Mxx_tilde_inv %*% Sigma_half %*% (2*c*w - tau_c_2*zeta_c_minus)

        avar <- term1 + term2

      }
    } # end normal
  } else if (initial_est == "saturated") { # end robustified
    if (ref_dist == "normal") {
      if (iteration == 0) {

        # create parameters needed for calculating asymptotic variance
        gamma <- sign_level
        c <- stats::qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE)
        phi <- 1 - gamma
        f <- stats::dnorm(c, mean=0, sd=1)
        tau_c_2 <- phi - 2 * c * f
        tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f
        tau_2 <- 1
        tau_4 <- 3
        varsigma_c_2 <- tau_c_2 / phi
        Omega <- parameters$params$Omega
        w <- Omega * tau_c_2
        zeta_c_minus <- 2 * Omega * c
        Sigma_half <- parameters$params$Sigma_half
        Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

        # calculate asymptotic variance
        term1 <- gamma * (1 - gamma)
        term2 <- (c * f)^2 * (tau_4 - 1) *
          ((split^2 / (1-split)) + ((1-split)^2 / split))
        term3 <- -2 * c * f * (1 - gamma - tau_c_2)
        term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*%
          Mxx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus) *
          ((split^2 / (1-split)) + ((1-split)^2 / split))
        avar <- term1 + term2 + term3 + term4

      } else { # m >= 1

        stop(strwrap("No theory for m >= 1 and saturated initial estimator
                     available", prefix = " ", initial = ""))

      } # end m >= 1
    } # end normal
  } # end saturated

  return(avar)

}
