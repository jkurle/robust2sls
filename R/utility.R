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

extract_formula <- function(formula) {

  # convert formula to character vector of length 1
  # use Reduce() to avoid length > 1 if formula is too long
  fml <- Reduce(paste, deparse(formula))

  # check that formula contains both "~" and "|" symbols
  if (!(grepl("~", fml) && grepl("|", fml))) {
    stop(strwrap("The `formula` is not of the required format since it does not
          include both symbols `~` and `|`", prefix = " ", initial = ""))
  }

  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")

  # ensure that the formula contains three parts
  if (length(fml_split[[1]]) != 3) {
    stop(strwrap("The `formula` does not consist of three parts as in
         y ~ x1 + x2 | z1 + z2", prefix = " ", initial = ""))
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
#' \code{selection} uses the data and model objects to create a list with four
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
#'
#' @return A list with five elements. The first four are vectors whose length
#' equals the number of observations in the data set. Unlike the residuals
#' stored in a model object (usually accessible via \code{model$residuals}), it
#' does not ignore observations where any of y, x or z are missing. It instead
#' sets their values to NA.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with TRUE if the
#' observation is judged as not outlying, FALSE if it is an outlier, and NA if
#' any of y, x, or z are missing. The fourth element of the list is an integer
#' vector with three values: 1 if the observations is judged to be an outlier,
#' 0 if not, and -1 if missing. The fifth and last element stores the
#' \code{\link[AER]{ivreg}} model object based on which the four vectors were
#' calculated.
#'
#' @section Warning:
#' Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), this function returns vectors of the same length as
#' the original data set even if any of the y, x, or z variables are missing.
#' The residuals for those observations are set to NA.
#'
#' @keywords internal

selection <- function(data, yvar, model, cutoff) {

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

  if (!(yvar %in% colnames(mtcars))) {
    stop(strwrap("The argument `yvar` is not a variable in the dataframe or
      matrix `data`", prefix = " ", initial = ""))
  }

  if (class(model) != "ivreg") {
    stop(strwrap("The `model` is not of class `ivreg`", prefix = " ",
      initial = ""))
  }

  # calculate the residuals, will be NA when either y or x missing
  res <- data[, yvar] - predict(model, data)
  # replace value by NA for observations where only z missing
  nonmiss <- nonmissing(data = data, formula = model$formula)
  res[!nonmiss] <- NA

  # calculate the standardised residuals
  # reverse df correction
  sigma <- model$sigma * sqrt(model$df.residual / model$nobs)
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
#' \code{nonmissing} takes a data set and a formula and determines which
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

nonmissing <- function(data, formula) {

  vars <- extract_formula(formula = formula)
  all_vars <- union(union(union(union(vars$y_var, vars$x1_var), vars$x2_var),
                          vars$z1_var), vars$z2_var)

  # initialise logical vector with all TRUE
  non_missing <- !logical(length = NROW(data))

  for (i in seq_along(all_vars)) {
    # keep as TRUE if not missing in any of the vars used for estimation
    non_missing <- (non_missing & !is.na(data[, all_vars[i]]))
  }

  # should never trigger but as a fail-safe
  if (length(non_missing)!= NROW(data)) {
    stop(strwrap("The returned vector does not have the same length as the
                 data set", prefix = " ", initial = ""))
  }

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
#'
#' @return Returns a list that stores values that are constant across the
#' estimation. It is used to fill parts of the \code{"robust2sls"} class object,
#' which is returned by \link{outlier_detection}.
#' \describe{
#'   \item{\code{$call}}{The captured function call.}
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
#'   random seed (\code{NULL} if argument not used).}
#'   \item{\code{$convergence}}{A list storing information about the convergence
#'   of the outlier-detection algorithm: \code{$criterion} is the user-specified
#'   convergence criterion (\code{NULL} if argument not used),
#'   \code{$difference} is initialised as \code{NULL}. \code{$converged} is
#'   initialised as \code{NULL}.}
#'   \item{\code{$iterations}}{A list storing information about the iterations
#'   of the algorithm. \code{$setting} stores the user-specified
#'   \code{iterations} argument. \code{$actual} is initialised as \code{NULL}
#'   and will store the actual number of iterations done.}
#' }

constants <- function(call, formula, data, reference = c("normal"), sign_level,
                      estimator, split, shuffle, shuffle_seed, iter,
                      criterion) {

  ref <- match.arg(reference) # throws error if not in selection

  if (sign_level >= 1 | sign_level <= 0) {
    stop(strwrap("The argument `sign_level` has to be > 0 and < 1",
                 prefix = " ", initial = ""))
  }

  if (ref == "normal") {
    psi <- 1 - sign_level
    cutoff <- qnorm(p = 1-(sign_level/2), mean = 0, sd = 1)
    bias_corr <- 1/(((1-sign_level)-2*cutoff*dnorm(cutoff,mean=0,sd=1))/
                      (1-sign_level))
  }

  if (estimator != "saturated") { # following args not used if not "saturated"
    split <- shuffle <- shuffle_seed <- NULL
  }

  initial <- list(estimator = estimator, split = split, shuffle = shuffle,
                  shuffle_seed = shuffle_seed)
  convergence <- list(criterion = criterion, difference = NULL,
                      converged = NULL)
  iterations <- list(setting = iter, actual = NULL)

  cons <- list(call = call, formula = formula, data = data, reference = ref,
               sign_level = sign_level, psi = psi, cutoff = cutoff,
               bias_corr = bias_corr, initial = initial, convergence =
                 convergence, iterations = iterations)

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
#' no appended with the new iteration results.

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

conv_diff <- function(current, counter) {

  if (current$cons$iterations$setting == 0) { # cannot calculate comparison

  } else { # can calculate

    if (current$cons$initial$estimator == "saturated" & counter == 1) {
      # we now have two initial estimates, one for each split
      # define the difference as the larger difference of the two estimates
      diff1 <- sum((current$model[[2]]$coefficients -
                      current$model[[1]][[1]]$coefficients)^2) # diff 1st split
      diff2 <- sum((current$model[[2]]$coefficients -
                      current$model[[1]][[2]]$coefficients)^2) # diff 2nd split
      diff <- max(diff1, diff2) # take the maximum of the two differences
    } else {
      diff <- sum((current$model[[length(current$model)]]$coefficients -
                     current$model[[length(current$model)-1]]$coefficients)^2)
    }

    return(diff)

  } # end can calculate

}




