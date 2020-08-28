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

#' Create residuals from model
#'
#' \code{res_all} creates a vector of residuals from the data and model object.
#' Unlike the residuals from the model object (\code{model$residuals}), it does
#' not exclude observations where any of y, x, or z are missing. Instead, it
#' includes these observations with a missing value \code{NA} as residual.
#'
#' @param data A dataset that contains the dependent variable \code{yvar} for
#' which the residuals will be calculated.
#' @param yvar A character vector of length 1 that refers to the name of the
#' dependent variable in the data set.
#' @param model A model object of \link{class} \link[AER]{ivreg} whose
#' parameters are used to calculate the residuals.
#'
#' @section Warning:
#' Unlike the object \code{model$residuals}, this function returns a
#' vector of the same length as the original data set even if any of the y, x,
#' or z variables are missing. The residuals for those observations are set to
#' NA.

res_all <- function(data, yvar, model) {

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

  return(res)

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
