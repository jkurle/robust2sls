#' Extract the elements of ivreg formula
#'
#' \code{extract_formula} takes a formula object for \code{ivreg}, i.e. in a
#' format of \code{y ~ x | z} and extracts the different elements in a list.
#' Each element is a character vector storing the different types of regressors.
#' Element \code{y_var} refers to the dependent variable, \code{x1_var} to the
#' endogenous regressors, \code{x2_var} and \code{z1_var} to the exogenous
#' regressors (which have to be included as instruments), and \code{z2_var} to
#' the outside instruments.
#'
#' @param formula A formula for the \code{\link[AER]{ivreg}} function, i.e. in
#' format \code{y ~ x | z}.
#'
#' @examples
#' extract_formula(y ~ x1 + x2 + x3 | x2 + x3 + z4)

extract_formula <- function(formula) {

  # convert formula to character vector of length 1
  # use Reduce() to avoid length > 1 if formula is too long
  fml <- Reduce(paste, deparse(formula))

  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")

  # ensure that the formula contains three parts
  if (length(fml_split[[1]]) != 3) {
    stop("The `formula` does not consist of three parts as in
         y ~ x1 + x2 | x2 + z3")
  }

  # delete symbols and leading & trailing spaces, collect in character vector
  y_var <- trimws(fml_split[[1]][1])
  x_var <- fml_split[[1]][2]
  x_var <- trimws(strsplit(x_var, "\\+")[[1]])
  z_var <- fml_split[[1]][3]
  z_var <- trimws(strsplit(z_var, "\\+")[[1]])

  x1_var <- setdiff(x_var, z_var) # endogenous regressors
  x2_var <- setdiff(x_var, x1_var) # exogenous regressors
  z1_var <- x2_var # included instruments
  z2_var <- setdiff(z_var, z1_var) # outside instruments

  vars <- list(y_var = y_var, x1_var = x1_var, x2_var = x2_var, z1_var = z1_var,
               z2_var = z2_var)
  return(vars)

}
