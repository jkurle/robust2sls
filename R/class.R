#' Constructor of r2sls class
#'
#' \code{new_r2sls} turns a list into an object of \link{class} "\code{r2sls}"
#'
#' @section Warning:
#' Only checks that the input is a list but not that its components match the
#' requirements of the "\code{r2sls}" class. Use the validator function
#' \code{validate_r2sls} for that purpose.
#'
#' @param x A list with components of the "\code{r2sls}" class.
#'
#' @examples
#' \dontrun{
#' new_r2sls(1:10)
#' }

new_r2sls <- function(x = list()) {

  stopifnot(is.list(x))
  structure(x, class = "r2sls")

}

#' Validator of r2sls class
#'
#' \code{validate_2rsls} checks that the input is a valid object of \link{class}
#' "\code{r2sls}"
#'
#' @param x An object whose validity of class "\code{r2sls}" is tested.

validate_r2sls <- function(x) {

  values <- unclass(x)

  if (!is.data.frame(x[["data"]]) | !is.matrix(x[["data"]])) {
    stop("Component `data` must either be a dataframe or matrix", call. = FALSE)
  }
  if (!(x[["gamma"]] > 0 & x[["gamma"]] < 1)) {
    stop("Component `gamma` must be between 0 and 1", call. = FALSE)
  }
  if (!is.numeric(x[["coefficients"]])) {
    stop("Component `coefficients` must be numeric", call. = FALSE)
  }
  if (!is.logical(x[["converged"]]) | !is.null(x[["converged"]])) {
    stop("Component `coefficients` must be logical or NULL", call. = FALSE)
  }
  if(!is.integer(x[["iterations"]]) | x[["integers"]] < 0) {
    stop("Component `iterations`must be integer >= 0", call. = FALSE)
  }

  x

}
