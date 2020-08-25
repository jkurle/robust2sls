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

#' Helper of r2sls class
#'
#' \code{r2sls} allows the user to create an object of \link{class}
#' "\code{r2sls}" by specifying the different components of the list. The
#' validator function \code{validate_r2sls} is called at the end to ensure that
#' the resulting object is a valid object of \link{class} "\code{r2sls}".
#'
#' @param data A dataframe or matrix containing the original data used in the
#' estimation.
#' @param coefficients A numeric vector of the second stage coefficients.
#' @param gamma A numeric scalar between 0 and 1 representing the tail quantiles
#' that determine the cutoff value for judging outliers.
#' @param iterations Determines the number of iterations of the outlier
#' detection algorithm. Either an integer >= 0 or a character \code{"inf"}
#' representing iteration until convergence.
#' @param converged A logical value whether the algorithm has converged.


r2sls <- function(data, coefficients, gamma, iterations, converged) {

  x <- list(data = data, coefficients = coefficients, gamma = gamma,
            iterations = iterations, converged = converged)
  x <- new_r2sls(x)
  validate_r2sls(x)

}
