#' Extract the elements of ivreg formula
#'
#' \code{extract_formula} takes a formula object for \code{ivreg}, i.e. in a
#' format of \code{y ~ x1 + x2 | z1 + z2} and extracts the different elements
#' in a list. Each element is a character vector storing the different types of
#' regressors. Element \code{y_var} refers to the dependent variable,
#' \code{x1_var} to the exogenous regressors, \code{x2_var} to the endogenous
#' regressors, \code{z1_var} to the exogenous regressors (which have to be
#' included again as instruments and hence coincide with \code{x1_var}), and
#' \code{z2_var} refers to the outside instruments.
#'
#' @param formula A formula for the \code{\link[AER]{ivreg}} function, i.e. in
#' format \code{y ~ x | z}.
#'
#' @examples
#' extract_formula(y ~ x1 + x2 + x3 | x1 + x2 + z3)

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
#' @param data A dataframe that contains the dependent variable \code{yvar} for
#' which the residuals will be calculated.
#' @param yvar A character vector of length 1 that refers to the name of the
#' dependent variable in the data set.
#' @param model A model object of \link{class} \link[AER]{ivreg} whose
#' parameters are used to calculate the residuals.
#' @param cutoff A numeric cutoff value used to judge whether an observation
#' is an outlier or not. If its absolute value is larger than the cutoff value,
#' the observations is classified as being an outlier.
#'
#' @return A list with five elements. The first four are vectors whose length
#' equals the number of observations in the data set. The first element is a
#' double vector containing the residuals for each observation based on the
#' model estimates. Unlike \code{model$residuals}, it does not ignore
#' observations where any of y, x or z are missing. It instead sets their values
#' to NA.
#'
#' The second element contains the standardised residuals, the third one a
#' logical vector with TRUE if the observation is judged as not outlying,
#' FALSE if it is an outlier, and NA if any of y, x, or z are missing. The
#' fourth element of the list is an integer vector with three values: 1 if the
#' observations is judged to be an outlier, 0 if not, and -1 if missing.
#'
#' The fifth and last element stores the \link[AER]{ivreg} model object based on
#' which the four vectors were calculated.
#'
#' @section Warning:
#' Unlike the object \code{model$residuals}, this function returns vectors
#' of the same length as the original data set even if any of the y, x,
#' or z variables are missing. The residuals for those observations are set to
#' NA.

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
#' estimation to be accessed throughout the calculations.
#' The following elements are stored (in order): the significance level to
#' determine the cutoff value, the reference distribution as a character vector,
#' the numeric cutoff value to determine outliers, the bias correction factor
#' to account for the fact that even under the null hypothesis of no outliers,
#' there will be some false positives that incorrectly classified as outliers.


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
