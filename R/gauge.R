
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
#' non-missing observations in the full sample from a \code{"robust2sls"} object
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
#' the given observations across the different iterations. There are three
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
#'   \link{estimate_param_null} that stores the parameters (true or estimated).
#'   \code{NULL} permitted if \code{ref_dist == "normal"}.
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split. Can be \code{NULL} if
#' \code{initial_est == "robustified"}.
#'
#' @return \code{gauge_avar} returns a numeric value.
#'
#' @export

gauge_avar <- function(ref_dist = c("normal"), sign_level,
                       initial_est = c("robustified", "saturated"),
                       iteration, parameters, split) {

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
  if (is.null(split) & !identical(initial_est, "robustified")) {
    stop("Argument 'split' cannot be NULL unless initial estimator is 'robustified'")
  }
  if (!is.null(split)) { # only need to check these when is not NULL
    if (!is.numeric(split) | !identical(length(split), 1L)) {
      stop(strwrap("Argument 'split' must be a numeric vector of length 1",
                   prefix = " ", initial = ""))
    }
    if (!(split > 0) | !(split < 1)) {
      stop(strwrap("Argument 'split' must lie strictly between 0 and 1",
                   prefix = " ", initial = ""))
    }
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

  # robustified and (saturated + split half) have identical expansion for m >= 0
  # therefore if this is the case, can use the existing formulas for robustified
  sat_split_half <- (initial_est == "saturated" && split == 0.5)

  if (initial_est == "robustified" | sat_split_half) {
    if (ref_dist == "normal") {

      # NOTE: under normality, general formula simplifies a lot
      # comment out the obsolete parts but keep code for other distributions in future

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
      # sigma <- parameters$params$sigma
      # Omega <- parameters$params$Omega
      # w <- Omega * tau_c_2
      # zeta_c_minus <- 2 * Omega * c
      # Sigma_half <- parameters$params$Sigma_half
      # Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

      if (iteration == 0) {

        # calculate asymptotic variance
        term1 <- gamma * (1 - gamma)
        term2 <- (c * f)^2 * (tau_4 - 1)
        term3 <- -2 * c * f * (1 - gamma - tau_c_2)
        # term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*%
        #   Mxx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
        avar <- term1 + term2 + term3 #+ term4

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
        # v21 <- t(vbb*zeta_c_minus + 2*c*f/tau_c_2*vsb*((c^2-varsigma_c_2)/2*zeta_c_minus - 2*c/phi*w) - 2*c*vss*Omega) %*% Sigma_half %*% Mxx_tilde_inv
        # v22 <- t(vbxu*zeta_c_minus + 2*c*(vsxu*((c^2-varsigma_c_2)/2*zeta_c_minus - 2*c/phi*w) - vsuu/phi*w)) %*% Sigma_half %*% Mxx_tilde_inv
        # v2 <- -f/sigma * t(cbind(v21, v22))

        # var-cov matrices
        var1 <- cbind(gamma*phi, phi-tau_c_2, 0)
        var1 <- rbind(var1, cbind(phi-tau_c_2, tau_4-1, tau_c_4-tau_c_2*varsigma_c_2))
        var1 <- rbind(var1, cbind(0, tau_c_4-tau_c_2*varsigma_c_2, tau_c_4-tau_c_2*varsigma_c_2))
        # var2 <- cbind(Mxx_tilde_inv*sigma^2, Mxx_tilde_inv*sigma^2*tau_c_2)
        # var2 <- rbind(var2, cbind(Mxx_tilde_inv*sigma^2*tau_c_2, Mxx_tilde_inv*sigma^2*tau_c_2))

        # calculate avar
        avar <- t(v1) %*% var1 %*% v1 #+ t(v2) %*% var2 %*% v2

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
        # term2 <- tau_c_2 * (f / ((phi - 2*c*f) * (tau_c_2 - c*(c^2-varsigma_c_2)*f)))^2 * t(2*c*w - tau_c_2*zeta_c_minus) %*%
        #   Sigma_half %*% Mxx_tilde_inv %*% Sigma_half %*% (2*c*w - tau_c_2*zeta_c_minus)

        avar <- term1 #+ term2

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
        # Omega <- parameters$params$Omega
        # w <- Omega * tau_c_2
        # zeta_c_minus <- 2 * Omega * c
        # Sigma_half <- parameters$params$Sigma_half
        # Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

        # calculate asymptotic variance
        term1 <- gamma * (1 - gamma)
        term2 <- (c * f)^2 * (tau_4 - 1) *
          ((split^2 / (1-split)) + ((1-split)^2 / split))
        term3 <- -2 * c * f * (1 - gamma - tau_c_2)
        # term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*%
        #   Mxx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus) *
        #   ((split^2 / (1-split)) + ((1-split)^2 / split))
        avar <- term1 + term2 + term3 #+ term4

      } else { # m >= 1

        stop(strwrap("No theory for m >= 1 if saturated initial estimator is not
                     split in half", prefix = " ", initial = ""))

      } # end m >= 1
    } # end normal
  } # end saturated

  return(avar)

}




#' Asymptotic covariance of gauge
#'
#' \code{gauge_covar} calculates the asymptotic covariance between two FODRs
#' with different cut-off values s and t for a given iteration using a given set
#' of parameters (true or estimated).
#'
#' @param ref_dist A character vector that specifies the reference distribution
#' against which observations are classified as outliers. \code{"normal"} refers
#' to the normal distribution.
#' @param sign_level1 A numeric value between 0 and 1 that determines the first
#'   cutoff in the reference distribution against which observations are judged
#'   as outliers or not.
#' @param sign_level2 A numeric value between 0 and 1 that determines the second
#'   cutoff in the reference distribution against which observations are judged
#'   as outliers or not.
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
#'   \link{estimate_param_null} that stores the parameters (true or estimated).
#'   \code{NULL} permitted if \code{ref_dist == "normal"}.
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split. Can be \code{NULL} if
#' \code{initial_est == "robustified"}.
#'
#' @return \code{gauge_covar} returns a numeric value.
#'
#' @export

gauge_covar <- function(ref_dist = c("normal"), sign_level1, sign_level2,
                       initial_est = c("robustified", "saturated"),
                       iteration, parameters, split) {

  if (!is.numeric(sign_level1) | !identical(length(sign_level1), 1L)) {
    stop(strwrap("Argument 'sign_level1' must be a numeric vector of length 1",
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(sign_level2) | !identical(length(sign_level2), 1L)) {
    stop(strwrap("Argument 'sign_level2' must be a numeric vector of length 1",
                 prefix = " ", initial = ""))
  }
  if (!(sign_level1 > 0) | !(sign_level1 < 1)) {
    stop(strwrap("Argument 'sign_level1' must lie strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }
  if (!(sign_level2 > 0) | !(sign_level2 < 1)) {
    stop(strwrap("Argument 'sign_level2' must lie strictly between 0 and 1",
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
  if (is.null(split) & !identical(initial_est, "robustified")) {
    stop("Argument 'split' cannot be NULL unless initial estimator is 'robustified'")
  }
  if (!is.null(split)) { # only need to check these when is not NULL
    if (!is.numeric(split) | !identical(length(split), 1L)) {
      stop(strwrap("Argument 'split' must be a numeric vector of length 1",
                   prefix = " ", initial = ""))
    }
    if (!(split > 0) | !(split < 1)) {
      stop(strwrap("Argument 'split' must lie strictly between 0 and 1",
                   prefix = " ", initial = ""))
    }
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

  # choose convention that s <= t (as in paper); so gamma_s >= gamma_t

  # robustified and (saturated + split half) have identical expansion for m >= 0
  # therefore if this is the case, can use the existing formulas for robustified
  sat_split_half <- (initial_est == "saturated" && split == 0.5)

  if (initial_est == "robustified" | sat_split_half) {
    if (ref_dist == "normal") {

      # NOTE: under normality, general formula simplifies a lot
      # comment out the obsolete parts but keep code for other distributions in future

      # create parameters needed for calculating asymptotic covariance
      sign_levels <- sort(c(sign_level1, sign_level2), decreasing = FALSE)
      gamma_s <- sign_levels[2] # larger level, smaller cutoff
      gamma_t <- sign_levels[1] # smaller level, larger cutoff
      s <- stats::qnorm(gamma_s/2, mean=0, sd=1, lower.tail = FALSE)
      t <- stats::qnorm(gamma_t/2, mean=0, sd=1, lower.tail = FALSE)
      phi_s <- 1 - gamma_s
      phi_t <- 1 - gamma_t
      f_s <- stats::dnorm(s, mean=0, sd=1)
      f_t <- stats::dnorm(t, mean=0, sd=1)
      tau_s_2 <- phi_s - 2 * s * f_s
      tau_t_2 <- phi_t - 2 * t * f_t
      tau_s_4 <- 3 * phi_s - 2 * s * (s^2 + 3) * f_s
      tau_t_4 <- 3 * phi_t - 2 * t * (t^2 + 3) * f_t
      tau_2 <- 1
      tau_4 <- 3
      varsigma_s_2 <- tau_s_2 / phi_s
      varsigma_t_2 <- tau_t_2 / phi_t
      # sigma <- parameters$params$sigma
      # Omega <- parameters$params$Omega
      # w_s <- Omega * tau_s_2
      # w_t <- Omega * tau_t_2
      # zeta_s_minus <- 2 * Omega * s
      # zeta_t_minus <- 2 * Omega * t
      # Sigma_half <- parameters$params$Sigma_half
      # Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

      if (iteration == 0) {

        # calculate asymptotic covariance
        term1 <- gamma_t * (1 - gamma_s)
        term2 <- -s * f_s * (1 - gamma_t - tau_t_2)
        term3 <- -t * f_t * (1 - gamma_s - tau_s_2)
        term4 <- s * t * f_s * f_t * (tau_4 - 1)
        # term5 <- f_s * f_t * t(zeta_s_minus - 2 * s * Omega) %*% Sigma_half %*%
        #   Mxx_tilde_inv %*% Sigma_half %*% (zeta_t_minus - 2 * t * Omega)
        covar <- term1 + term2 + term3 + term4 #+ term5

      } else if (is.numeric(iteration)) { # m >= 1

        # calculate varrho parameters, already for iteration
        m <- iteration
        v_s <- varrho(sign_level = gamma_s, ref_dist = "normal",
                      iteration = iteration)
        vbb_s <- v_s$c$vbb
        vss_s <- v_s$c$vss
        vbxu_s <- v_s$c$vbxu
        vsuu_s <- v_s$c$vsuu
        vsb_s <- v_s$c$vsb
        vsxu_s <- v_s$c$vsxu
        v_t <- varrho(sign_level = gamma_t, ref_dist = "normal",
                      iteration = iteration)
        vbb_t <- v_t$c$vbb
        vss_t <- v_t$c$vss
        vbxu_t <- v_t$c$vbxu
        vsuu_t <- v_t$c$vsuu
        vsb_t <- v_t$c$vsb
        vsxu_t <- v_t$c$vsxu

        # calculate asymptotic covariance
        # to avoid mistakes, I don't implement the formula explicitly
        # instead, use vector notation of gauge and get covar by inner product

        # vector with scalars
        v1_s <- rbind(1, -s*f_s*vss_s, -s*f_s*vsuu_s)
        v1_t <- rbind(1, -t*f_t*vss_t, -t*f_t*vsuu_t)
        # vector with vectors
        # v21_s <- t(vbb_s*zeta_s_minus + 2*s*f_s/tau_s_2*vsb_s*((s^2-varsigma_s_2)/2*zeta_s_minus - 2*s/phi_s*w_s) - 2*s*vss_s*Omega) %*% Sigma_half %*% Mxx_tilde_inv
        # v21_t <- t(vbb_t*zeta_t_minus + 2*t*f_t/tau_t_2*vsb_t*((t^2-varsigma_t_2)/2*zeta_t_minus - 2*t/phi_t*w_t) - 2*t*vss_t*Omega) %*% Sigma_half %*% Mxx_tilde_inv
        # v22_s <- t(vbxu_s*zeta_s_minus + 2*s*(vsxu_s*((s^2-varsigma_s_2)/2*zeta_s_minus - 2*s/phi_s*w_s) - vsuu_s/phi_s*w_s)) %*% Sigma_half %*% Mxx_tilde_inv
        # v22_t <- t(vbxu_t*zeta_t_minus + 2*t*(vsxu_t*((t^2-varsigma_t_2)/2*zeta_t_minus - 2*t/phi_t*w_t) - vsuu_t/phi_t*w_t)) %*% Sigma_half %*% Mxx_tilde_inv
        # v2_s <- -f_s/sigma * t(cbind(v21_s, v22_s))
        # v2_t <- -f_t/sigma * t(cbind(v21_t, v22_t))

        # var-cov matrices
        var1 <- cbind(gamma_t*phi_s, phi_s-tau_s_2, tau_t_2*phi_s/phi_t-tau_s_2)
        var1 <- rbind(var1, cbind(phi_t-tau_t_2, tau_4-1, tau_t_4-tau_t_2*varsigma_t_2))
        var1 <- rbind(var1, cbind(0, tau_s_4-tau_s_2*varsigma_s_2, tau_s_4-tau_s_2*varsigma_s_2))
        # var2 <- cbind(Mxx_tilde_inv*sigma^2, Mxx_tilde_inv*sigma^2*tau_t_2)
        # var2 <- rbind(var2, cbind(Mxx_tilde_inv*sigma^2*tau_s_2, Mxx_tilde_inv*sigma^2*tau_s_2))

        # calculate covar
        covar <- t(v1_s) %*% var1 %*% v1_t #+ t(v2_s) %*% var2 %*% v2_t

      } else { # "convergence"

        # varrho parameters
        # varrho always also gives fixed point parameters so "iteration" does not matter
        # might change in future if varrho() takes also "convergence" as value
        v_s <- varrho(sign_level = gamma_s, ref_dist = "normal", iteration = 1)
        v_t <- varrho(sign_level = gamma_t, ref_dist = "normal", iteration = 1)
        vbb_s <- v_s$fp$vbb
        vss_s <- v_s$fp$vss
        vbxu_s <- v_s$fp$vbxu
        vsuu_s <- v_s$fp$vsuu
        vsb_s <- v_s$fp$vsb
        vsxu_s <- v_s$fp$vsxu
        vbb_t <- v_t$fp$vbb
        vss_t <- v_t$fp$vss
        vbxu_t <- v_t$fp$vbxu
        vsuu_t <- v_t$fp$vsuu
        vsb_t <- v_t$fp$vsb
        vsxu_t <- v_t$fp$vsxu

        # for fixed point, again do not use the varrho terms
        term1 <- gamma_t * (1 - gamma_s)
        term2 <- s*t*f_s*f_t*(tau_s_4 - tau_s_2*varsigma_s_2) / ((tau_s_2-s*(s^2-varsigma_s_2)*f_s) * (tau_t_2-t*(t^2-varsigma_t_2)*f_t))
        term3 <- -t*f_t*(tau_t_2*phi_s/phi_t - tau_s_2) / (tau_t_2-t*(t^2-varsigma_t_2)*f_t)
        # term4 <- tau_s_2*f_s*f_t / ((phi_s-2*s*f_s)*(phi_t-2*t*f_t)*(tau_s_2-s*(s^2-varsigma_s_2)*f_s)*(tau_t_2*(t^2-varsigma_t_2)*f_t)) * t(2*s*w_s - tau_s_2*zeta_s_minus) %*% Sigma_half %*% Mxx_tilde_inv %*% Sigma_half %*% (2*t*w_t - tau_t_2*zeta_t_minus)
        covar <- term1 + term2 + term3 #+ term4

      }
    } # end normal
  } else if (initial_est == "saturated") { # end robustified
    if (ref_dist == "normal") {

      # NOTE: under normality, general formula simplifies a lot
      # comment out the obsolete parts but keep code for other distributions in future

      if (iteration == 0) {

        # create parameters needed for calculating asymptotic covariance
        sign_levels <- sort(c(sign_level1, sign_level2), decreasing = FALSE)
        gamma_s <- sign_levels[2] # larger level, smaller cutoff
        gamma_t <- sign_levels[1] # smaller level, larger cutoff
        s <- stats::qnorm(gamma_s/2, mean=0, sd=1, lower.tail = FALSE)
        t <- stats::qnorm(gamma_t/2, mean=0, sd=1, lower.tail = FALSE)
        phi_s <- 1 - gamma_s
        phi_t <- 1 - gamma_t
        f_s <- stats::dnorm(s, mean=0, sd=1)
        f_t <- stats::dnorm(t, mean=0, sd=1)
        tau_s_2 <- phi_s - 2 * s * f_s
        tau_t_2 <- phi_t - 2 * t * f_t
        tau_s_4 <- 3 * phi_s - 2 * s * (s^2 + 3) * f_s
        tau_t_4 <- 3 * phi_t - 2 * t * (t^2 + 3) * f_t
        tau_2 <- 1
        tau_4 <- 3
        varsigma_s_2 <- tau_s_2 / phi_s
        varsigma_t_2 <- tau_t_2 / phi_t
        # sigma <- parameters$params$sigma
        # Omega <- parameters$params$Omega
        # w_s <- Omega * tau_s_2
        # w_t <- Omega * tau_t_2
        # zeta_s_minus <- 2 * Omega * s
        # zeta_t_minus <- 2 * Omega * t
        # Sigma_half <- parameters$params$Sigma_half
        # Mxx_tilde_inv <- parameters$params$Mxx_tilde_inv

        # calculate asymptotic variance
        term1 <- gamma_t * (1 - gamma_s)
        term2 <- -s*f_s*(phi_t - tau_t_2) - t*f_t*(phi_s - tau_s_2)
        # term3 <- f_s*f_t*(s*t*(tau_4-1) + t(zeta_s_minus-2*s*Omega) %*% Sigma_half %*% Mxx_tilde_inv %*% Sigma_half %*% (zeta_t_minus-2*t*Omega)) * (split^3 + (1-split)^3) / (split*(1-split))
        covar <- term1 + term2 #+ term3

      } else { # m >= 1

        stop(strwrap("No theory for m >= 1 if saturated initial estimator is not
                     split in half", prefix = " ", initial = ""))

      } # end m >= 1
    } # end normal
  } # end saturated

  return(covar)

}
