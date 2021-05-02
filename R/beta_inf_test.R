# used to store the functions for
# a) valid inference for the structural beta parameters
# b) Hausman and t-test for difference between robust and full sample estimates
# applicable under H0 of no outliers

#' Calculates the correction factor for inference under H0 of no outliers
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer > 0 specifying the iteration step for which
#' parameters to calculate corrected standard errors.
#' @param exact A logical value indicating whether the actually detected share
#' of outliers (TRUE) or the theoretical share (FALSE) should be used.
#' @param fp A logical value whether the fixed point standard error correction
#' (TRUE) or the exact iteration correction should be computed (FALSE).
#'
#' @details
#' Argument \code{iteration} specifies which iteration of the robust structural
#' parameter estimates should be calculated. Iteration \code{1} refers to the
#' first robust estimate. Iteration \code{0} is not a valid argument since it
#' is the baseline estimate, which is not robust.
#'
#' The parameter \code{exact} does not matter much under the null hypothesis of
#' no outliers since the detected share will converge to the theoretical share.
#' Under the alternative, this function should not be used.
#'
#' Argument \code{fp} determines whether the fixed point standard error
#' correction should be computed. This argument is only respected if the
#' specified \code{iteration} is one of the iterations after the algorithm
#' converged.
#'
#' @return \code{beta_inf_correction} returns the numeric correction factor.
#'
#' @export

beta_inf_correction <- function(robust2sls_object, iteration = 1,
                                exact = FALSE, fp = FALSE) {

  # test inputs correct
  validate_robust2sls(robust2sls_object)

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(iteration), 1L)) {
    stop(strwrap("The argument `iteration` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!((iteration %% 1) == 0)) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("The argument `iteration` must be >= 1", prefix = " ",
                 initial = ""))
  }
  if (!(iteration <= robust2sls_object$cons$iterations$actual)) {
    stop(strwrap("The argument `iteration` specifies a higher iteration
                 than was actually done", prefix = " ",
                 initial = ""))
  }
  if (!is.logical(exact)) {
    stop(strwrap("The argument `exact` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(exact), 1L)) {
    stop(strwrap("The argument `exact` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!is.logical(fp)) {
    stop(strwrap("The argument `fp` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(fp), 1L)) {
    stop(strwrap("The argument `fp` has to be length 1", prefix = " ",
                 initial = ""))
  }
  # checking inputs end

  m <- iteration
  fml <- extract_formula(robust2sls_object$cons$formula)
  dx <- length(fml$x1_var) + length(fml$x2_var)
  psi_asymp <- robust2sls_object$cons$psi
  psi_exact <- 1 - outliers_prop(robust2sls_object = robust2sls_object,
                                 iteration = (iteration-1))
  # note: element "iteration" is m-1 because we start counting at 0 (base model)
  # desired: e.g. for iteration = 1, want to use v(m-1) = v(0) here

  sigma_correction <- robust2sls_object$cons$bias_corr # varsigma_{c}^{-2}

  v <- varrho(sign_level = robust2sls_object$cons$sign_level,
                   ref_dist = robust2sls_object$cons$reference,
                   iteration = m)

  # which parameters to use: has algorithm converged at the given iteration?
  iter_converged <- robust2sls_object$cons$convergence$iter
  converged_yn <- robust2sls_object$cons$convergence$converged
  if (is.null(converged_yn)) { converged_yn <- FALSE }
  # number doesn't matter bc then also converged_yn = FALSE
  if (is.null(iter_converged)) { iter_converged <- 1 }
  # check that converged and we are analysing an iteration after convergence
  if (fp & (converged_yn == TRUE) & (iter_converged <= iteration)) {
    vbb <- v$fp$vbb
    vbxu <- v$fp$vbxu
    which_corr <- "fixed point"
  } else {
    vbb <- v$c$vbb
    vbxu <- v$c$vbxu
    which_corr <- paste("iteration m = ", iteration, sep = "")
  }

  vartheta <- (vbb^2 + 2*v$aux$tauc2*vbb*vbxu + v$aux$tauc2*vbxu^2)^(-1)
  # note here defined without multiplied by identity matrix
  # iota <- (vartheta * v$aux$varsigmac2)^-1 * diag(nrow = dx) # iota(m)
  iota <- (vartheta * v$aux$varsigmac2)^-1

  if (exact == TRUE) {
    correction <- psi_exact * iota
  } else {
    correction <- psi_asymp * iota
  }

  attr(correction, which = "type of correction") <- which_corr
  return(correction) # returns correction factor

}

#' Calculates valid se for coefficients under H0 of no outliers
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer > 0 specifying the iteration step for which
#' parameters to calculate corrected standard errors.
#' @param exact A logical value indicating whether the actually detected share
#' of outliers (TRUE) or the theoretical share (FALSE) should be used.
#' @param fp A logical value whether the fixed point standard error correction
#' (TRUE) or the exact iteration correction should be computed (FALSE).
#'
#' @details
#' Argument \code{iteration} specifies which iteration of the robust structural
#' parameter estimates should be calculated. Iteration \code{1} refers to the
#' first robust estimate. Iteration \code{0} is not a valid argument since it
#' is the baseline estimate, which is not robust.
#'
#' The parameter \code{exact} does not matter much under the null hypothesis of
#' no outliers since the detected share will converge to the theoretical share.
#' Under the alternative, this function should not be used.
#'
#' Argument \code{fp} determines whether the fixed point standard error
#' correction should be computed. This argument is only respected if the
#' specified \code{iteration} is one of the iterations after the algorithm
#' converged.
#'
#' @return \code{beta_inf} returns the corrected standard errors for the
#' structural parameters. These are valid under the null hypothesis of no
#' outliers in the sample. For comparison, the uncorrected standard errors are
#' also reported.
#'
#' @export

beta_inf <- function(robust2sls_object, iteration = 1, exact = FALSE,
                     fp = FALSE) {

  # test inputs correct
  validate_robust2sls(robust2sls_object)

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(iteration), 1L)) {
    stop(strwrap("The argument `iteration` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!((iteration %% 1) == 0)) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("The argument `iteration` must be >= 1", prefix = " ",
                 initial = ""))
  }
  if (!(iteration <= robust2sls_object$cons$iterations$actual)) {
    stop(strwrap("The argument `iteration` specifies a higher iteration
                 than was actually done", prefix = " ",
                 initial = ""))
  }
  if (!is.logical(exact)) {
    stop(strwrap("The argument `exact` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(exact), 1L)) {
    stop(strwrap("The argument `exact` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!is.logical(fp)) {
    stop(strwrap("The argument `fp` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(fp), 1L)) {
    stop(strwrap("The argument `fp` has to be length 1", prefix = " ",
                 initial = ""))
  }
  # checking inputs end

  # extract usual standard errors (+1 because first element is m0)
  rmodel <- robust2sls_object$model[[iteration+1]]
  beta_vcov <- (rmodel$sigma)^2 * rmodel$cov.unscaled # already divided by n
  beta_se <- sqrt(diag(beta_vcov))

  # create corrected standard errors
  correction <- beta_inf_correction(robust2sls_object = robust2sls_object,
                                    iteration = iteration, exact = exact,
                                    fp = fp)
  beta_vcov_corr <- correction * beta_vcov
  beta_se_corr <- sqrt(diag(beta_vcov_corr))

  # extract coefficient estimates
  coeff <- rmodel$coefficients
  # delete any NAs (multicollinearity)
  coeff <- coeff[!is.na(coeff)]

  # calculate t values
  t <- coeff/beta_se
  t_corr <- coeff/beta_se_corr

  # calculate p values (two-sided)
  pn <- stats::pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE)
  pn_corr <- 2 * stats::pnorm(abs(t_corr), mean = 0, sd = 1, lower.tail = FALSE)

  # calculate p values (two-sided) based on t-distribution
  df <- rmodel$df.residual
  pt <- 2 * stats::pt(abs(t), df = df, lower.tail = FALSE)
  pt_corr <- 2 * stats::pt(abs(t_corr), df = df, lower.tail = FALSE)

  out <- cbind(coeff, beta_se, beta_se_corr, t, t_corr, pt, pt_corr, pn, pn_corr)

  outnames <- c("Estimate", "Std. Error", "H0Std. Error", "t value",
                "H0t value", "Pr(>|t|) t dist", "H0Pr(>|t|) t dist",
                "Pr(>|t|) normal", "H0Pr(>|t|) normal")
  colnames(out) <- outnames
  attr(out, "iteration") <- iteration
  attr(out, "type of correction") <- attr(correction, "type of correction")

  return(out)

}

#' Calculates the asymptotic variance of the difference between robust and full
#' sample estimators of the structural parameters
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer > 0 specifying the iteration step for which
#' parameters to calculate corrected standard errors.
#' @param fp A logical value whether the fixed point asymptotic variance
#' (TRUE) or the exact iteration asymptotic variance should be computed (FALSE).
#'
#' @return \code{beta_test_avar} returns a dx by dx variance-covariance matrix of
#' the difference between the robust and full sample strucutral parameter
#' estimates of the 2SLS model.
#'
#' @details
#' Argument \code{fp} determines whether the fixed point asymptotic variance
#' should be computed. This argument is only respected if the specified
#' \code{iteration} is one of the iterations after the algorithm converged.
#'
#' @export

beta_test_avar <- function(robust2sls_object, iteration, fp = FALSE) {

  # test inputs correct
  validate_robust2sls(robust2sls_object)

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(iteration), 1L)) {
    stop(strwrap("The argument `iteration` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!((iteration %% 1) == 0)) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("The argument `iteration` must be >= 1", prefix = " ",
                 initial = ""))
  }
  if (!(iteration <= robust2sls_object$cons$iterations$actual)) {
    stop(strwrap("The argument `iteration` specifies a higher iteration
                 than was actually done", prefix = " ",
                 initial = ""))
  }
  if (!is.logical(fp)) {
    stop(strwrap("The argument `fp` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(fp), 1L)) {
    stop(strwrap("The argument `fp` has to be length 1", prefix = " ",
                 initial = ""))
  }
  # checking inputs end

  m <- iteration
  rmodel <- robust2sls_object$model[[iteration+1]] # 1st element is m0

  # could extract dx from formula but problematic if have collinearity
  # under collinearity the dimension of the var-cov matrix will be smaller
  ### fml <- extract_formula(robust2sls_object$cons$formula)
  ### dx <- length(fml$x1_var) + length(fml$x2_var)

  v <- varrho(sign_level = robust2sls_object$cons$sign_level,
              ref_dist = robust2sls_object$cons$reference,
              iteration = m)

  # which parameters to use: has algorithm converged at the given iteration?
  iter_converged <- robust2sls_object$cons$convergence$iter
  converged_yn <- robust2sls_object$cons$convergence$converged
  if (is.null(converged_yn)) { converged_yn <- FALSE }
  # number doesn't matter bc then also converged_yn = FALSE
  if (is.null(iter_converged)) { iter_converged <- 1 }
  # check that converged and we are analysing an iteration after convergence
  if (fp & (converged_yn == TRUE) & (iter_converged <= iteration)) {
    vbb <- v$fp$vbb
    vbxu <- v$fp$vbxu
    which_corr <- "fixed point"
  } else {
    vbb <- v$c$vbb
    vbxu <- v$c$vbxu
    which_corr <- paste("iteration m = ", iteration, sep = "")
  }

  factor <- (vbb-1)^2 + 2*v$aux$tauc2*(vbb-1)*vbxu + v$aux$tauc2*vbxu^2

  sigma_correction <- robust2sls_object$cons$bias_corr # varsigma_{c}^{-2}
  # sigma2 <- (rmodel$sigma)^2 * rmodel$df.residual / rmodel$nobs # reverse df
  sigma2 <- (rmodel$sigma)^2
  sigma2_robust <- sigma2 * sigma_correction

  Mxx_tilde_inv_robust <- rmodel$cov.unscaled * rmodel$nobs

  avar <- factor * sigma2_robust * Mxx_tilde_inv_robust
  attr(avar, which = "type of avar") <- which_corr

  return(avar)

}

#' Conducts a t-test on the difference between robust and full sample estimates
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer > 0 specifying the iteration step for which
#' parameters to calculate corrected standard errors.
#' @param element An index or a string to select the coefficient which is to be
#' tested. The index should refer to the index of coefficients in the
#' \code{"ivreg"} model object, i.e. \code{$coefficients}.
#' @param fp A logical value whether the fixed point asymptotic variance
#' (TRUE) or the exact iteration asymptotic variance should be used (FALSE).
#'
#' @return \code{beta_t} returns a matrix with the robust and full sample
#' estimates of beta, the t statistic on their difference, the standard error of
#' the difference, and three p-values (two-sided, both one-sided alternatives).
#'
#' @details
#' Argument \code{fp} determines whether the fixed point asymptotic variance
#' should be used. This argument is only respected if the specified
#' \code{iteration} is one of the iterations after the algorithm converged.
#'
#' @export

beta_t <- function(robust2sls_object, iteration, element, fp = FALSE) {

  # test inputs correct
  validate_robust2sls(robust2sls_object)

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(iteration), 1L)) {
    stop(strwrap("The argument `iteration` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!((iteration %% 1) == 0)) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("The argument `iteration` must be >= 1", prefix = " ",
                 initial = ""))
  }
  if (!(iteration <= robust2sls_object$cons$iterations$actual)) {
    stop(strwrap("The argument `iteration` specifies a higher iteration
                 than was actually done", prefix = " ",
                 initial = ""))
  }
  if (!(is.numeric(element) | is.character(element))) {
    stop(strwrap("The argument `element` must be numeric or a string",
                 prefix = " ", initial = ""))
  }
  if (is.numeric(element)) {
    if (any(!(element %% 1 == 0))) {
      stop(strwrap("The argument `element` must only contain integers
                   if numeric", prefix = " ", initial = ""))
    }
    n.coef <- length(robust2sls_object$model[[iteration + 1]]$coefficients)
    if (any(element > n.coef)) {
      stop(strwrap("The argument `element` contains indices larger than the
                   number of coefficients in the model", prefix = " ",
                   initial = ""))
    }
    if (any(element <= 0)) {
      stop(strwrap("The argument `element` contains indices < 1",
                   prefix = " ", initial = ""))
    }
  } # end if element numeric
  if (is.character(element)) {
    names.coef <- names(robust2sls_object$model[[iteration + 1]]$coefficients)
    if (any(!(element %in% names.coef))) {
      stop(strwrap("The argument `element` contains names that are not among the
                   coefficients in the model", prefix = " ", initial = ""))
    }
  } # end if element character
  if (!is.logical(fp)) {
    stop(strwrap("The argument `fp` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(fp), 1L)) {
    stop(strwrap("The argument `fp` has to be length 1", prefix = " ",
                 initial = ""))
  }
  # checking inputs end

  # obtain asymptotic variance of whole beta vector
  avar <- beta_test_avar(robust2sls_object = robust2sls_object,
                         iteration = iteration, fp = fp)
  # estimate full sample
  full <- AER::ivreg(formula = robust2sls_object$cons$formula,
                data = robust2sls_object$cons$data)

  # extract vector of parameter estimates
  coef_robust <- robust2sls_object$model[[iteration + 1]]$coefficients
  coef_full <- full$coefficients

  # extract single element
  if (is.numeric(element)) {

    beta_robust <- coef_robust[element]
    beta_full <- coef_full[element]

    if (!identical(names(beta_robust), names(beta_full))) {
      stop(strwrap("The index selects different coefficients in the robust and full sample specification.",
                   prefix = " ", initial = ""))
    }

    if (any(is.na(beta_robust)) | any(is.na(beta_full))) {
      stop(strwrap("At least one of the coefficients is NA. Check elements.",
                   prefix = " ", initial = ""))
    }

  } else { # element is string

    beta_robust <- coef_robust[element]
    beta_full <- coef_full[element]

    if (any(is.na(beta_robust)) | any(is.na(beta_full))) {
      stop(strwrap("At least one of the coefficients is NA. Check elements.",
                   prefix = " ", initial = ""))
    }

    } # end extract coefficients

    # $coefficients can still contain NA values but M_xx_tilde_inv does not
    # so the index may potentially differ between the two
    # select by name instead, is safer
    name <- names(beta_robust) # this should be = element if it was a string
    betadiff_avar <- avar[name, name] # if several elements, is a matrix
    # extract the diagonal elements
    # need to ensure it is a matrix
    betadiff_avar <- as.matrix(betadiff_avar)
    betadiff_avar <- diag(betadiff_avar)
    n <- full$nobs
    betadiff_se <- sqrt(betadiff_avar / n)

    t <- (beta_robust - beta_full) / betadiff_se
    p_two <- 2 * stats::pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE)
    p_lower <- stats::pnorm(t, mean = 0, sd = 1, lower.tail = TRUE)
    p_higher <- stats::pnorm(t, mean = 0, sd = 1, lower.tail = FALSE)

    out <- cbind(beta_robust, beta_full, betadiff_se, t, p_two, p_higher,
                 p_lower)
    r <- paste("robust m =", iteration, collapse = "")
    cnames <- c(r, "full", "se diff", "t value", "Pr(>|z|)", "Pr(>z)", "Pr(<z)")
    colnames(out) <- cnames
    attr(out, "type of avar") <- attr(avar, "type of avar")

    return(out)

}

#' Calculates a Hausman test on the difference between robust and full sample
#' estimates
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer > 0 specifying the iteration step for which
#' parameters to calculate corrected standard errors.
#' @param subset A vector of numeric indices or strings indicating which
#' coefficients to include in the Hausman test. \code{NULL} uses the whole
#' vector of coefficients.
#' @param fp A logical value whether the fixed point asymptotic variance
#' (TRUE) or the exact iteration asymptotic variance should be used (FALSE).
#'
#' @return \code{beta_hausman} returns a matrix with the value of the Hausman
#' test statistic and its corresponding p-value. The attribute
#' \code{"type of avar"} records which asymptotic variance has been used (the
#' specific iteration or the fixed point). The attribute \code{"coefficients"}
#' stores the names of the coefficients that were included in the Hausman test.
#'
#' @details
#' Argument \code{fp} determines whether the fixed point asymptotic variance
#' should be used. This argument is only respected if the specified
#' \code{iteration} is one of the iterations after the algorithm converged.
#'
#' @export

beta_hausman <- function(robust2sls_object, iteration, subset = NULL,
                         fp = FALSE) {

  # test inputs correct
  validate_robust2sls(robust2sls_object)

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(iteration), 1L)) {
    stop(strwrap("The argument `iteration` has to be length 1", prefix = " ",
                 initial = ""))
  }
  if (!((iteration %% 1) == 0)) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("The argument `iteration` must be >= 1", prefix = " ",
                 initial = ""))
  }
  if (!(iteration <= robust2sls_object$cons$iterations$actual)) {
    stop(strwrap("The argument `iteration` specifies a higher iteration
                 than was actually done", prefix = " ",
                 initial = ""))
  }
  if (!(is.null(subset) || is.numeric(subset) || is.character(subset))) {
    stop(strwrap("The argument `subset` must be NULL or numeric or a string",
                 prefix = " ", initial = ""))
  }
  if (!is.null(subset)) { # otherwise cannot evaluate others
    if (is.numeric(subset)) {
      if (any(!(subset %% 1 == 0))) {
        stop(strwrap("The argument `subset` must only contain integers
                    if numeric", prefix = " ", initial = ""))
      }
      n.coef <- length(robust2sls_object$model[[iteration + 1]]$coefficients)
      if (any(subset > n.coef)) {
        stop(strwrap("The argument `subset` contains indices larger than the
                    number of coefficients in the model", prefix = " ",
                    initial = ""))
      }
      if (any(subset <= 0)) {
        stop(strwrap("The argument `subset` contains indices < 1",
                    prefix = " ", initial = ""))
      }
    } # end if subset numeric
    if (is.character(subset)) {
      names.coef <- names(robust2sls_object$model[[iteration + 1]]$coefficients)
      if (any(!(subset %in% names.coef))) {
        stop(strwrap("The argument `subset` contains names that are not among the
                    coefficients in the model", prefix = " ", initial = ""))
      }
    } # end if subset character
  } # end if subset not null
  if (!is.logical(fp)) {
    stop(strwrap("The argument `fp` has to be a logical value", prefix = " ",
                 initial = ""))
  }
  if (!identical(length(fp), 1L)) {
    stop(strwrap("The argument `fp` has to be length 1", prefix = " ",
                 initial = ""))
  }
  # checking inputs end

  # obtain asymptotic variance of whole beta vector
  avar <- beta_test_avar(robust2sls_object = robust2sls_object,
                         iteration = iteration, fp = fp)
  # estimate full sample
  full <- AER::ivreg(formula = robust2sls_object$cons$formula,
                data = robust2sls_object$cons$data)

  # extract vector of parameter estimates
  coef_robust <- robust2sls_object$model[[iteration + 1]]$coefficients
  coef_full <- full$coefficients

  if (is.null(subset)) { # then test whole vector, so only need to remove NAs

    # could contain NA values
    coef_robust <- coef_robust[!is.na(coef_robust)]
    coef_full <- coef_full[!is.na(coef_full)]

  }

  if (is.numeric(subset)) { # select subset based on original indices (w/ NA)

    coef_robust <- coef_robust[subset]
    coef_full <- coef_full[subset]

    if (!identical(names(coef_robust), names(coef_full))) {
      stop(strwrap("The index selects different coefficients in the robust and full sample specification.",
                   prefix = " ", initial = ""))
    }

    if (any(is.na(coef_robust)) | any(is.na(coef_full))) {
      stop(strwrap("At least one of the coefficients is NA. Check elements.",
                   prefix = " ", initial = ""))
    }

  } else if (is.character(subset)) { # select based on names

    coef_robust <- coef_robust[subset]
    coef_full <- coef_full[subset]

    if (any(is.na(coef_robust)) | any(is.na(coef_full))) {
      stop(strwrap("At least one of the coefficients is NA. Check elements.",
                   prefix = " ", initial = ""))
    }

  } # end selecting coefficient subset

  # select avar for subset

  name <- names(coef_robust) # this should be = subset if it was a string
  betadiff_avar <- avar[name, name] # if several elements, is a matrix
  # need to ensure it is a matrix
  # note: if subset = NULL then identical(betadiff_avar, avar) is TRUE
  betadiff_avar <- as.matrix(betadiff_avar)

  # aux
  n <- full$nobs
  coef_diff <- coef_robust - coef_full
  dx <- length(coef_diff)

  # Hausman test
  h <- n * t(coef_diff) %*% solve(betadiff_avar) %*% coef_diff
  p <- stats::pchisq(h, df = dx, lower.tail = FALSE)

  out <- cbind(h, p)
  cnames <- c("Hausman test", "p value")
  colnames(out) <- cnames
  attr(out, "type of avar") <- attr(avar, "type of avar")
  attr(out, "coefficients") <- name

  return(out)

}


