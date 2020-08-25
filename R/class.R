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
