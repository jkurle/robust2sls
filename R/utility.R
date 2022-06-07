#' Extract the elements of ivreg formula
#'
#' \code{extract_formula} takes a formula object for \code{\link[AER]{ivreg}},
#' i.e. in a format of \code{y ~ x1 + x2 | x1 + z2} and extracts the different
#' elements in a list. Each element is a character vector storing the different
#' types of regressors. Element \code{y_var} refers to the dependent variable,
#' \code{x1_var} to the exogenous regressors, \code{x2_var} to the endogenous
#' regressors, \code{z1_var} to the exogenous regressors (which have to be
#' included again as instruments and hence coincide with \code{x1_var}), and
#' \code{z2_var} refers to the outside instruments.
#'
#' @param formula A formula for the \code{\link[AER]{ivreg}} function, i.e. in
#' format \code{y ~ x1 + x2 | z1 + z2}.
#'
#' @return \code{extract_formula} returns a list with five named components,
#' each of which is a character vector: \code{$y_var} refers to the dependent
#' variable, \code{$x1_var} to the exogenous regressors, \code{$x2_var} to the
#' endogenous regressors, \code{$z1_var} to the exogenous regressors (which have
#' to be included again as instruments and hence coincide with \code{$x1_var}),
#' and \code{$z2_var} refers to the outside instruments.
#'
#' @keywords internal
#' @export

extract_formula <- function(formula) {

  # convert formula to character vector of length 1
  # use Reduce() to avoid length > 1 if formula is too long
  fml <- Reduce(paste, deparse(formula))

  # check that formula contains both "~" and "|" symbols
  if (!(grepl("~", fml) && grepl("\\|", fml))) {
    stop(strwrap("The `formula` is not of the required format since it does not
          include both symbols `~` and `|`", prefix = " ", initial = ""))
  }

  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")

  # ensure that the formula contains three parts
  if (length(fml_split[[1]]) != 3) {
    stop(strwrap("The `formula` does not consist of three parts as in
         y ~ x1 + x2 | x1 + z2", prefix = " ", initial = ""))
  }

  # delete symbols and leading & trailing spaces, collect in character vector
  y_var <- trimws(fml_split[[1]][1])
  x_var <- fml_split[[1]][2]
  x_var <- trimws(strsplit(x_var, "\\+|\\*")[[1]])
  z_var <- fml_split[[1]][3]
  z_var <- trimws(strsplit(z_var, "\\+|\\*")[[1]])

  if (y_var == "") {
    stop(strwrap("The `formula` does not specify any dependent variable",
         prefix = " ", initial = ""))
  }

  x2_var <- setdiff(x_var, z_var) # endogenous regressors
  x1_var <- setdiff(x_var, x2_var) # exogenous regressors
  z1_var <- x1_var # included instruments
  z2_var <- setdiff(z_var, z1_var) # outside instruments

  if (length(x2_var) > length(z2_var)) {
    stop(strwrap("The specified formula does not fulfill the order condition
      (for 2SLS, the number of outside instruments must be weakly larger than
      the number of endogenous regressors", prefix = " ", initial = ""))
  }

  vars <- list(y_var = y_var, x1_var = x1_var, x2_var = x2_var, z1_var = z1_var,
               z2_var = z2_var)
  return(vars)

}

#' Create selection (non-outlying) vector from model
#'
#' \code{selection} uses the data and model objects to create a list with five
#' elements that are used to determine whether the observations are judged as
#' outliers or not.
#'
#' @param data A dataframe.
#' @param yvar A character vector of length 1 that refers to the name of the
#' dependent variable in the data set.
#' @param model A model object of \link{class} \code{\link[AER]{ivreg}} whose
#' parameters are used to calculate the residuals.
#' @param cutoff A numeric cutoff value used to judge whether an observation
#' is an outlier or not. If its absolute value is larger than the cutoff value,
#' the observations is classified as being an outlier.
#' @param bias_correction A numeric factor used to correct the estimate of
#' sigma under the null hypothesis of no outliers or \code{NULL} if no
#' correction should be done.
#'
#' @return A list with five elements. The first four are vectors whose length
#' equals the number of observations in the data set. Unlike the residuals
#' stored in a model object (usually accessible via \code{model$residuals}), it
#' does not ignore observations where any of y, x or z are missing. It instead
#' sets their values to \code{NA}.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with \code{TRUE} if
#' the observation is judged as not outlying, \code{FALSE} if it is an outlier,
#' and \code{NA} if any of y, x, or z are missing. The fourth element of the
#' list is an integer vector with three values: 1 if the observations is judged
#' to be an outlier, 0 if not, and -1 if missing. The fifth and last element
#' stores the \code{\link[AER]{ivreg}} model object based on which the four
#' vectors were calculated.
#'
#' @section Warning:
#' Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), this function returns vectors of the same length as
#' the original data set even if any of the y, x, or z variables are missing.
#' The residuals for those observations are set to \code{NA}.
#'
#' @keywords internal

selection <- function(data, yvar, model, cutoff, bias_correction = NULL) {

  # cannot simply extract residuals or fitted values from model object because
  # it omits all observations where x, z, or y is missing
  # dta <- mtcars
  # dta[1, "mpg"] <- NA
  # dta[2, "cyl"] <- NA
  # dta[3, "wt"] <- NA
  # test <- ivreg(mpg ~ cyl + disp | cyl + wt, data = dta)
  # NROW(mtcars) # 32 observations
  # NROW(test$residuals) # 29 observations due to NA values in y, x, z
  # NROW(dta[, "mpg"] - predict(test, dta)) # 32 when calculating manually, NA

  if (typeof(yvar) != "character") {
    stop(strwrap("The argument `yvar` is not a character vector", prefix = " ",
      initial = ""))
  }

  if (!(yvar %in% colnames(data))) {
    stop(strwrap("The argument `yvar` is not a variable in the dataframe or
      matrix `data`", prefix = " ", initial = ""))
  }

  if (class(model) != "ivreg") {
    stop(strwrap("The `model` is not of class `ivreg`", prefix = " ",
      initial = ""))
  }

  # calculate the residuals, will be NA when either y or x missing
  res <- data[, yvar] - stats::predict(model, data)
  # replace value by NA for observations where only z missing
  nonmiss <- nonmissing(data = data, formula = model$formula)
  res[!nonmiss] <- NA

  # calculate the standardised residuals
  # reverse df correction and potentially bias correction
  if (is.null(bias_correction)) {
    sigma <- model$sigma * sqrt(model$df.residual / model$nobs)
  } else {
    sigma <- model$sigma * sqrt(model$df.residual / model$nobs) *
      sqrt(bias_correction)
  }

  # calculate standardised residuals, also missing if any of y, x, or z missing
  stdres <- res / sigma

  # create selection vector, if missing before then still missing now
  sel <- (abs(stdres) <= cutoff)
  sel[is.na(sel)] <- FALSE # manually exclude all observations that had NA
  type <- as.numeric(nonmiss) + as.numeric(sel) - 1
  type <- as.integer(type)

  # if observations are named, should give type these
  names(type) <- names(sel)

  return(list(res = res, stdres = stdres, sel = sel, type = type,
              model = model))

}

#' Determine which observations can be used for estimation
#'
#' \code{nonmissing} takes a dataframe and a formula and determines which
#' observations can principally be used for the estimation of the 2SLS model
#' that is specified by the formula. Observations where any of the y, x, or z
#' variables are missing will be set to FALSE. While technically, fitted values
#' and residuals could be calculated for observations where only any of the
#' outside instruments is missing, this is often not desirable. This would cause
#' the sample on which the model is estimated to be different from the sample
#' on which the outliers are determined.
#'
#' @param data A dataframe.
#' @inheritParams extract_formula
#'
#' @return Returns a logical vector with the same length as the number of
#' observations in the data set that specifies whether an observation has any
#' missing values in any of y, x, or z variables. TRUE means not missing, FALSE
#' means at least one of these variables necessary for estimation is missing.
#'
#' @keywords internal
#' @export

nonmissing <- function(data, formula) {

  if (!is.data.frame(data)) {
    stop(strwrap("The argument 'data' must be a dataframe", prefix = " ",
                 initial = ""))
  }

  vars <- extract_formula(formula = formula)
  all_vars <- union(union(union(union(vars$y_var, vars$x1_var), vars$x2_var),
                          vars$z1_var), vars$z2_var)

  # if formula contains "0", "-1", or "1", don't need to check
  all_vars <- setdiff(all_vars, c("0", "-1", "1"))

  # initialise logical vector with all TRUE
  non_missing <- !logical(length = NROW(data))

  for (i in seq_along(all_vars)) {
    # keep as TRUE if not missing in any of the vars used for estimation
    non_missing <- (non_missing & !is.na(data[, all_vars[i]]))
  }

  # should never trigger but as a fail-safe
  if (length(non_missing)!= NROW(data)) { # nocov start
    stop(strwrap("The returned vector does not have the same length as the
                 data set", prefix = " ", initial = ""))
  } # nocov end

  return(non_missing)

}

#' Calculate constants across estimation
#'
#' \code{constants} calculates various values that do not change across the
#' estimation and records them in a list.
#'
#' @param call A record of the original function call.
#' @param formula The regression formula specified in the function call.
#' @param data The dataframe used in the function call.
#' @param reference A character vector of length 1 that denotes a valid
#' reference distribution.
#' @param sign_level A numeric value between 0 and 1 that determines the cutoff
#' in the reference distribution against which observations are judged as
#' outliers or not.
#' @param estimator A character vector specifying which initial estimator was
#' used.
#' @param split A numeric value strictly between 0 and 1 that specifies how the
#' sample is split in case of saturated 2SLS. \code{NULL} otherwise.
#' @param shuffle A logical value whether the sample is re-arranged in random
#' order before splitting the sample in case of saturated 2SLS. \code{NULL}
#' otherwise.
#' @param shuffle_seed A numeric value setting the seed for the shuffling of the
#' sample. Only used if \code{shuffle == TRUE}. NULL otherwise.
#' @param iter An integer value setting the number of iterations of the
#' outlier-detection algorithm.
#' @param criterion A numeric value that determines when the iterated
#' outlier-detection algorithm stops by comparing it to the sum of squared
#' differences between the m- and (m-1)-step parameter estimates. NULL if
#' convergence criterion should not be used.
#' @param max_iter A numeric value that determines after which iteration the
#' algorithm stops in case it does not converge.
#' @param user_model A model object of \link{class} \link[AER]{ivreg}. Only
#' required if argument \code{initial_est} is set to \code{"user"}, otherwise
#' \code{NULL}.
#' @param verbose A logical value whether progress during estimation should be
#' reported.
#'
#' @return Returns a list that stores values that are constant across the
#' estimation. It is used to fill parts of the \code{"robust2sls"} class object,
#' which is returned by \link{outlier_detection}.
#' \describe{
#'   \item{\code{$call}}{The captured function call.}
#'   \item{\code{$verbose}}{The verbose argument (TRUE/FALSE).}
#'   \item{\code{$formula}}{The formula argument.}
#'   \item{\code{$data}}{The original data set.}
#'   \item{\code{$reference}}{The chosen reference distribution to classify
#'   outliers.}
#'   \item{\code{$sign_level}}{The significance level determining the cutoff.}
#'   \item{\code{$psi}}{The probability that an observation is not classified as
#'   an outlier under the null hypothesis of no outliers.}
#'   \item{\code{$cutoff}}{The cutoff used to classify outliers if
#'   their standardised residuals are larger than that value.}
#'   \item{\code{$bias_corr}}{A numeric bias correction factor to account for
#'   potential false positives (observations classified as outliers even though
#'   they are not).}
#'   \item{\code{$initial}}{A list storing settings about the initial estimator:
#'   \code{$estimator} is the type of the initial estimator (e.g. robustified or
#'   saturated), \code{$split} how the sample is split (\code{NULL} if argument
#'   not used), \code{$shuffle} whether the sample is shuffled before splitting
#'   (\code{NULL} if argument not used), \code{$shuffle_seed} the value of the
#'   random seed (\code{NULL} if argument not used), \code{$user} the
#'   user-specified initial model (\code{NULL} if not used).}
#'   \item{\code{$convergence}}{A list storing information about the convergence
#'   of the outlier-detection algorithm: \code{$criterion} is the user-specified
#'   convergence criterion (\code{NULL} if argument not used),
#'   \code{$difference} is initialised as \code{NULL}. \code{$converged} is
#'   initialised as \code{NULL}. \code{$iter} is initialised as \code{NULL}.
#'   \code{$max_iter} the maximum number of iterations if does not converge
#'   (\code{NULL} if not used or applicable).}
#'   \item{\code{$iterations}}{A list storing information about the iterations
#'   of the algorithm. \code{$setting} stores the user-specified
#'   \code{iterations} argument. \code{$actual} is initialised as \code{NULL}
#'   and will store the actual number of iterations done.}
#' }
#'
#' @keywords internal

constants <- function(call, formula, data, reference = c("normal"), sign_level,
                      estimator, split, shuffle, shuffle_seed, iter,
                      criterion, max_iter, user_model, verbose) {

  ref <- match.arg(reference) # throws error if not in selection

  if (sign_level >= 1 | sign_level <= 0) {
    stop(strwrap("The argument `sign_level` has to be > 0 and < 1",
                 prefix = " ", initial = ""))
  }

  if (identical(ref, "normal")) {
    psi <- 1 - sign_level
    cutoff <- stats::qnorm(p = 1-(sign_level/2), mean = 0, sd = 1)
    bias_corr <- 1/(((1-sign_level)-2*cutoff*stats::dnorm(cutoff,mean=0,sd=1))/
                      (1-sign_level))
  }

  if (!identical(estimator, "saturated")) { # args not used if not "saturated"
    split <- shuffle <- shuffle_seed <- NULL
  }

  initial <- list(estimator = estimator, split = split, shuffle = shuffle,
                  shuffle_seed = NULL, user = user_model)
  convergence <- list(criterion = criterion, difference = NULL,
                      converged = NULL, iter = NULL, max_iter = max_iter)
  iterations <- list(setting = iter, actual = NULL)

  if (identical(estimator, "saturated") & identical(shuffle, TRUE)) {
    initial$shuffle_seed <- shuffle_seed
  }

  cons <- list(call = call, verbose = FALSE, formula = formula, data = data,
               reference = ref, sign_level = sign_level, psi = psi,
               cutoff = cutoff, bias_corr = bias_corr, initial = initial,
               convergence = convergence, iterations = iterations)

  return(cons)

}


#' Append new iteration results to \code{"robust2sls"} object
#'
#' \code{update_list} takes an existing \code{"robust2sls"} object and appends
#' the estimation results (\link[AER]{ivreg} model object, residuals,
#' standardised residuals, selection and type vectors) of a new iteration.
#'
#' @param current_list A list object of class \code{"robust2sls"}.
#' @param new_info A list with named components \code{$model}, \code{$res},
#' \code{$stdres}, \code{$sel}, and \code{$type}.
#' @param name A character vector of length one naming the appended iteration
#' results. Convention is \code{"m0"}, \code{"m1"}, \code{"m2"} etc. for
#' iterations 0 (initial), 1, 3...
#'
#' @return An object of class \code{"robust2sls"} whose components
#' \code{$model}, \code{$res}, \code{$stdres}, \code{$sel}, and \code{$type} are
#' now appended with the new iteration results.
#'
#' @keywords internal

update_list <- function(current_list, new_info, name) {

  current_list$model[[name]] <- new_info$model
  current_list$res[[name]] <- new_info$res
  current_list$stdres[[name]] <- new_info$stdres
  current_list$sel[[name]] <- new_info$sel
  current_list$type[[name]] <- new_info$type

  return(current_list)

}

#' L2 norm between two most recent estimates
#'
#' \code{conv_diff} uses an object of class \code{"robust2sls"} to calculate the
#' L2 norm (sum of squared differences) between the most recent outlier-robust
#' iteration and the previous iteration estimates.
#'
#' @param current A list object of class \code{"robust2sls"}.
#' @param counter An integer denoting the number of the current iteration.
#'
#' @return \code{conv_diff} returns a numeric value, which is the L2 norm
#' of the difference between the most recent and the previous parameter
#' estimates. The L2 norm is the sum of squared differences of the estimates.
#'
#' @keywords internal

conv_diff <- function(current, counter) {

  if (current$cons$iterations$setting == 0) { # cannot calculate comparison

  } else { # can calculate

    if (current$cons$initial$estimator == "saturated" && counter == 1) {
      # we now have two initial estimates, one for each split

      coef_current <- current$model[[2]]$coefficients
      coef_before1 <- current$model[[1]][[1]]$coefficients
      coef_before2 <- current$model[[1]][[2]]$coefficients

      # check for NA coefficients (due to collinearity)
      if (any(is.na(coef_current)) | any(is.na(coef_before1))
          | any(is.na(coef_before2))) {

        # need to get rid of these missing values
        coef_current <- coef_current[!is.na(coef_current)]
        coef_before1 <- coef_before1[!is.na(coef_before1)]
        coef_before2 <- coef_before2[!is.na(coef_before2)]

        # check that they are still lined up in the same way
        if ((length(coef_current) != length(coef_before1)) |
            (length(coef_current) != length(coef_before2))) {
          stop(strwrap("Iterations have a different number of coefficients. This should only happen when the change in the subsample causes a change in perfect collinearity.",
                       prefix = " ", initial = ""))
        }
        varnames1 <- names(coef_current)
        varnames2 <- names(coef_before1)
        varnames3 <- names(coef_before2)
        if (!identical(varnames1, varnames2) |
            !identical(varnames1, varnames3)) {
          stop(strwrap("Iterations have different regressors or ordering. Cannot calculate the difference.",
                       prefix = " ", initial = ""))
        } # if not same variables or ordering
      } # close if any missing

      # define the difference as the larger difference of the two estimates
      diff1 <- sum((coef_current - coef_before1)^2) # diff 1st split
      diff2 <- sum((coef_current - coef_before2)^2) # diff 2nd split
      diff <- max(diff1, diff2) # take the maximum of the two differences

    } else { # not (first iteration and saturated)

      coef_current <- current$model[[length(current$model)]]$coefficients
      coef_before <- current$model[[length(current$model)-1]]$coefficients

      # check for NA coefficients (due to collinearity)
      if (any(is.na(coef_current)) | any(is.na(coef_before))) {

        # need to get rid of these missing values
        coef_current <- coef_current[!is.na(coef_current)]
        coef_before <- coef_before[!is.na(coef_before)]

        # check that they are still lined up in the same way
        if (length(coef_current) != length(coef_before)) {
          stop(strwrap("Iterations have a different number of coefficients. This should only happen when the change in the subsample causes a change in perfect collinearity.",
                       prefix = " ", initial = ""))
        }
        varnames1 <- names(coef_current)
        varnames2 <- names(coef_before)
        if (!identical(varnames1, varnames2)) {
          stop(strwrap("Iterations have different regressors or ordering. Cannot calculate the difference.",
                       prefix = " ", initial = ""))
        } # if not same variables or ordering
      } # close if any missing

      diff <- sum((coef_current - coef_before)^2) # L2 norm

    }

    return(diff)

  } # end can calculate

}


#' Calculate varrho coefficients
#'
#' \code{varrho} calculates the coefficients for the asymptotic variance of the
#' gauge (false outlier detection rate) for a specific iteration m >= 1.
#'
#' @param sign_level A numeric value between 0 and 1 that determines the cutoff
#' in the reference distribution against which observations are judged as
#' outliers or not.
#' @param ref_dist A character vector that specifies the reference distribution
#' against which observations are classified as outliers. \code{"normal"} refers
#' to the normal distribution.
#' @param iteration An integer >= 1 that specifies the iteration of the outlier
#' detection algorithm.
#'
#' @return \code{varrho} returns a list with four components, all of which are
#' lists themselves. \code{$setting} stores the arguments with which the
#' function was called. \code{$c} stores the values of the six different
#' coefficients for the specified iteration. \code{$fp} contains the fixed point
#' versions of the six coefficients. \code{$aux} stores intermediate values
#' required for calculating the coefficients.
#'
#' @export

varrho <- function(sign_level, ref_dist = c("normal"), iteration) {

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
  if (!is.numeric(iteration) | !identical(length(iteration), 1L)) {
    stop(strwrap("Argument 'iteration' must be a numeric vector of length 1",
                 prefix = " ", initial = ""))
  }
  if (!(iteration %% 1 == 0)) {
    stop(strwrap("Argument 'iteration' must be an integer", prefix = " ",
                 initial = ""))
  }
  if (!(iteration >= 1)) {
    stop(strwrap("Argument 'iteration' must be weakly larger than 1",
                 prefix = " ", initial = ""))
  }

  if (ref_dist == "normal") {

    gamma <- sign_level
    c <- stats::qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE)
    phi <- 1 - gamma
    f <- stats::dnorm(c, mean=0, sd=1)
    tau_c_2 <- phi - 2 * c * f
    tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f
    tau_2 <- 1
    tau_4 <- 3
    varsigma_c_2 <- tau_c_2 / phi
    m <- iteration

    # varrho beta beta
    vbb <- (2 * c * f / phi)^m
    # varrho sigma sigma
    vss <- (c * (c^2 - varsigma_c_2) * f / tau_c_2)^m
    # varrho beta tildex u
    vbxu <- (phi^m - (2 * c * f)^m) / (phi^m * (phi - 2 * c * f))
    # varrho sigma u u
    vsuu <- (tau_c_2^m - (c * (c^2 - varsigma_c_2) * f)^m) /
      (tau_c_2^m * (tau_c_2 - c * (c^2 - varsigma_c_2) * f))
    # varrho sigma beta
    vsb_fun <- function(m, l, c, f, phi, varsigma_c_2, tau_c_2) {
      ele <- (2*c*f/phi)^(m-l-1) * ((c*(c^2 - varsigma_c_2)*f) / tau_c_2)^l
      return(ele)
    }
    vsb_parts <- vapply(X = 0:(m-1), FUN = vsb_fun, FUN.VALUE = double(1),
                        m = m, c = c, f = f, phi = phi, varsigma_c_2 = varsigma_c_2,
                        tau_c_2 = tau_c_2)
    vsb <- sum(vsb_parts)
    # varrho sigma tildex u
    vsxu <- (((tau_c_2^m - (c * (c^2 - varsigma_c_2) * f)^m) /
                (tau_c_2^(m-1) * (tau_c_2 - c * (c^2 - varsigma_c_2) * f))) -
               vsb) * f / (tau_c_2 * (phi - 2 * c * f))

    # also provide fixed point elements
    vbb_fp <- 0
    vbxu_fp <- 1 / (phi - 2 * c * f)
    vss_fp <- 0
    vsuu_fp <- 1 / (tau_c_2 - c * (c^2 - varsigma_c_2) * f)
    vsb_fp <- 0
    vsxu_fp <- f / ((phi - 2 * c * f) * (tau_c_2 - c * (c^2 - varsigma_c_2) * f))

  } # end normal

  set <- list(sign_level = sign_level, ref_dist = ref_dist, m = iteration)
  coeff <- list(vbb = vbb, vss = vss, vbxu = vbxu, vsuu = vsuu, vsb = vsb,
                vsxu = vsxu)
  fp <- list(vbb = vbb_fp, vss = vss_fp, vbxu = vbxu_fp, vsuu = vsuu_fp,
             vsb = vsb_fp, vsxu = vsxu_fp)
  auxiliary <- list(f = f, tau2 = tau_2, tau4 = tau_4, tauc2 = tau_c_2,
                    tauc4 = tau_c_4, varsigmac2 = varsigma_c_2)
  out <- list(setting = set, c = coeff, fp = fp, aux = auxiliary)

  return(out)

}

#' Estimation of moments of the data
#'
#' NOTE (12 Apr 2022): probably superseded by estimate_param_null() function
#' taken out of testing
#'
#' DO NOT USE YET!
#' \code{estimate_param} can be used to estimate certain moments of the data
#' that are required for calculating the asymptotic variance of the gauge. Such
#' moments are the covariance between the standardised first stage errors and
#' the structural error \eqn{\Omega}, the covariance matrix of the first stage
#' errors \eqn{\Sigma}, the first stage parameter matrix \eqn{\Pi}, and more.
#'
#' @param robust2SLS_object An object of class \code{"robust2sls"} for which
#' the moments will be calculated.
#' @param iteration An integer >= 0 specifying based on which model iteration
#' the moments should be estimated. The model iteration affects which
#' observations are determined to be outliers and these observations will hence
#' be excluded during the estimation of the moments.
#'
#' @return \code{estimate_param} returns a list with a similar structure as the
#' output of the Monte Carlo functionality \link{generate_param}. Hence, the
#' resulting list can be given to the function \link{gauge_avar} as argument
#' \code{parameters} to return an estimate of the asymptotic variance of the
#' gauge.
#'
#' @section Warning:
#' The function is not yet fully developed. The estimators of the moments are
#' at the moment not guaranteed to be consistent for the population moments. DO
#' NOT USE!
#'
#' @export

estimate_param <- function(robust2SLS_object, iteration) {

  # extract the dataset
  data <- robust2SLS_object$cons$data

  # how to call the variable in the data set that stores the subset selection
  # don't overwrite an existing variable so create name that does not yet exist
  i <- 1
  selection_name <- "selection"
  while (selection_name %in% colnames(data)) {
    selection_name <- paste("selection_", i, sep = "")
    i <- i + 1
  }
  data[[selection_name]] <- robust2SLS_object$sel[[iteration + 1]]

  # calculate first stage linear projections
  fml <- extract_formula(robust2SLS_object$cons$formula)
  dx1 <- length(fml$x1_var)
  dx2 <- length(fml$x2_var)
  dz1 <- length(fml$z1_var)
  dz2 <- length(fml$z2_var)

  Pi_hat <- NULL
  R2_hat <- NULL

  # create the first stage formulas
  z <- union(fml$z1_var, fml$z2_var)
  part2 <- paste(z, collapse = " + ")
  part2 <- paste("0 + ", part2, sep = "") # take out intercept
  for (i in seq_along(fml$x2_var)) {
    depvar <- fml$x2_var[[i]]
    formula1 <- paste(depvar, part2, sep = " ~ ")
    model1 <- NULL
    # run first stages
    command <- paste("model1 <- stats::lm(formula = as.formula(formula1),
                     data = data, subset = ", selection_name, ")")
    expr <- parse(text = command)
    eval(expr)
    pi_hat <- as.matrix(model1$coefficients, (dz1+dz2), 1) # into column vector
    colnames(pi_hat) <- depvar
    Pi_hat <- cbind(Pi_hat, pi_hat)

    r2_hat <- data[, fml$x2_var[[i]]] - stats::predict(model1, newdata = data)
    r2_hat <- r2_hat[data[[selection_name]]]
    R2_hat <- cbind(R2_hat, r2_hat)

  }

  dimnames(R2_hat) <- NULL

  # alternative to get R2_hat via ivreg model object
  # other <- data[data[[selection_name]], fml$x2_var] -
  #   stats::model.matrix(robust2SLS_object$model[[iteration + 2]],
  #                       component = c("projected"))[, fml$x2_var]

  # pad the matrix to account for perfectly fit exogenous regressors
  Pi_hat <- cbind(rbind(diag(dx1), matrix(0,dz2,dx1)), Pi_hat)

  # estimate Mzz = Ezz' = Var(z) + Ez Ez'
  Z <- data[, z]
  Z <- Z[robust2SLS_object$sel[[iteration + 1]], ]
  ZZ <- t(as.matrix(Z)) %*% as.matrix(Z)
  Mzz_hat <- ZZ / NROW(Z)

  # estimate Mxx_tilde_inv
  Mxx_tilde_hat <- t(Pi_hat) %*% Mzz_hat %*% Pi_hat
  Mxx_tilde_inv_hat <- pracma::inv(Mxx_tilde_hat)

  # estimate Sigma2 = E(r2i r2i')
  Sigma2_hat <- stats::cov(R2_hat)
  Sigma2_half_hat <- pracma::sqrtm(Sigma2_hat)$B
  Sigma2_half_inv_hat <- pracma::inv(Sigma2_half_hat)

  # pad the matrix to get estimate of Sigma
  Sigma_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                     cbind(matrix(0, dx2, dx1), Sigma2_hat))
  Sigma_half_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                     cbind(matrix(0, dx2, dx1), Sigma2_half_hat))
  Sigma_half_inv_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                     cbind(matrix(0, dx2, dx1), Sigma2_half_inv_hat))

  # estimate E(r2i ui)
  u_std_hat <- robust2SLS_object$stdres[[iteration + 2]][robust2SLS_object$sel[[iteration + 1]]]
  MRu_hat <- colMeans(R2_hat * u_std_hat)

  # estimate Omega2
  Omega2_hat <- Sigma2_half_inv_hat %*% MRu_hat

  # pad the vector to get estimate of Omega
  Omega_hat <- rbind(matrix(0, dx1, 1), Omega2_hat)

  # try alternative and see whether works better; get virtually the same
  # probably only different because below didn't reverse the df correction
  # u_hat <- robust2SLS_object$model$m1$residuals
  # MRu <- colMeans(R2_hat * u_hat)
  # sigma <- robust2SLS_object$model$m1$sigma * sqrt(robust2SLS_object$cons$bias_corr)
  # Omega2_hat <- Sigma2_half_inv_hat %*% MRu / sigma

  # the problem is with R2u_hat because we are using a truncated sample
  # so probably need a correction factor to account for falsely detected outl.
  # could also estimate on full sample but the problem would be under altern.
  # of contaminated sample

  # for now, return results in a list similar to that created by generate_param

  set <- list(call = robust2SLS_object$cons$call,
              formula = robust2SLS_object$cons$formula, dx1 = dx1, dx2 = dx2,
              dz2 = dz2)
  nam <- list(y = fml$y_var, x1 = fml$x1_var, x2 = fml$x2_var, z2 = fml$z2_var)
  par <- list(Omega = Omega_hat, Sigma_half = Sigma_half_hat,
              Mxx_tilde_inv = Mxx_tilde_inv_hat)
  out <- list(params = par, setting = set, names = nam)
  return(out)

}


#' Multivariate normal supremum simulation
#'
#' \code{mvn_sup} simulates the distribution of the supremum of the specified
#' multivariate normal distribution by drawing repeatedly from the multivariate
#' normal distribution and calculating the maximum of each vector.
#'
#' @param n An integer determining the number of draws from the multivariate
#' normal distribution.
#' @param mu A numeric vector representing the mean of the multivariate normal
#' distribution.
#' @param Sigma A numeric matrix representing the variance-covariance matrix of
#' the mutlivariate normal distribution.
#' @param seed An integer setting the random seed or \code{NULL} if it should
#' not be set.
#'
#' @return \code{mvn_sup} returns a vector of suprema of length \code{n}.
#' @export

mvn_sup <- function(n, mu, Sigma, seed = NULL) {

  if (!is.numeric(n) || !(n %% 1 == 0)) {
    stop("Argument 'n' is not an integer.")
  }

  if (!is.numeric(mu)) {
    stop("Argument 'mu' is not a numeric vector.")
  }

  if (!is.numeric(Sigma) | !is.matrix(Sigma)) {
    stop("Argument 'Sigma' is not a numeric matrix.")
  }

  if (!identical(length(mu), NROW(Sigma)) | !identical(length(mu), NCOL(Sigma))) {
    stop("Vector 'mu' is not compatible with var-cov matrix 'Sigma'.")
  }

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  sim <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  # sim returns an (n) by (length of mvn vector) matrix
  # to get supremum (discrete = maximum) take maximum across rows

  sup <- apply(X = abs(sim), MARGIN = 1, FUN = max)


  return(sup)

}










