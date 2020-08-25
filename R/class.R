#' Constructor of robust2sls class
#'
#' \code{new_robust2sls} turns a list into an object of \link{class}
#' "\code{robust2sls}"
#'
#' @section Warning:
#' Only checks that the input is a list but not that its components match the
#' requirements of the "\code{robust2sls}" class. Use the validator function
#' \code{validate_robust2sls} for that purpose.
#'
#' @param x A list with components of the "\code{robust2sls}" class.
#'
#' @examples
#' \dontrun{
#' new_robust2sls(1:10)
#' }

new_robust2sls <- function(x = list()) {

  stopifnot(is.list(x))
  structure(x, class = "robust2sls")

}

#' Validator of robust2sls class
#'
#' \code{validate_2robustsls} checks that the input is a valid object of
#' \link{class} "\code{robust2sls}"
#'
#' @param x An object whose validity of class "\code{robust2sls}" is tested.

validate_robust2sls <- function(x) {

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

#' Helper of robust2sls class
#'
#' \code{robust2sls} allows the user to create an object of \link{class}
#' "\code{robust2sls}" by specifying the different components of the list. The
#' validator function \code{validate_robust2sls} is called at the end to ensure
#' that the resulting object is a valid object of \link{class}
#' "\code{robust2sls}".
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


robust2sls <- function(data, coefficients, gamma, iterations, converged) {

  x <- list(data = data, coefficients = coefficients, gamma = gamma,
            iterations = iterations, converged = converged)
  x <- new_robust2sls(x)
  validate_robust2sls(x)

}
