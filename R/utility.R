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

  res <- data[, yvar] - predict(model, data)

  return(res)

}


