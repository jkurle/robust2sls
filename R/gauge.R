
#' Number of outliers
#'
#' \code{outliers} calculates the number of outliers from a \code{"robust2sls"}
#' object for a given iteration.
#'
#' @param robust2sls_object An object of class \code{"robust2sls"}.
#' @param iteration An integer >= 0 representing the iteration for which the
#' outliers are calculated.
#'
#' @return \code{outliers} returns the number of outliers  for a given iteration
#' as determined by the outlier-detection algorithm.

outliers <- function(robust2sls_object, iteration) {

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }

  if (iteration %% 1 != 0) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }

  if (iteration < 0) {
    stop(strwrap("The argument `iteration` has to be >= 0", prefix = " ",
                 initial = ""))
  }

  if (iteration > robust2sls_object$cons$iterations$actual) {
    stop(strwrap("The 'robust2sls' object has fewer iterations than argument
                 `iteration` specifies.", prefix = " ", initial = ""))
  }

  if (class(robust2sls_object) != "robust2sls") {
    stop(strwrap("The argument `robust2sls_object` does not have the correct
                 class", prefix = " ", initial = ""))
  }

  # add check that "robust2sls" object is valid? validator returns object though

  accessor <- paste("m", iteration, sep = "")
  num <- sum(robust2sls_object$type[[accessor]] == 0)
  return(num)

}

#' Proportion of outliers
#'
#' \code{outliers_prop} calculates the proportion of outliers relative to all
#' non-missing observations in the full sample from a \code{"robust2sls"} obejct
#' for a given iteration.
#'
#' @inheritParams outliers
#'
#' @return \code{outliers_prop} returns the proportion of outliers for a given
#' iteration as determined by the outlier-detection algorithm.

outliers_prop <- function(robust2sls_object, iteration) {

  if (!is.numeric(iteration)) {
    stop(strwrap("The argument `iteration` has to be numeric", prefix = " ",
                 initial = ""))
  }

  if (iteration %% 1 != 0) {
    stop(strwrap("The argument `iteration` has to be an integer", prefix = " ",
                 initial = ""))
  }

  if (iteration > robust2sls_object$cons$iterations$actual) {
    stop(strwrap("The 'robust2sls' object has fewer iterations than argument
                 `iteration` specifies.", prefix = " ", initial = ""))
  }

  if (class(robust2sls_object) != "robust2sls") {
    stop(strwrap("The argument `robust2sls_object` does not have the correct
                 class", prefix = " ", initial = ""))
  }

  accessor <- paste("m", iteration, sep = "")
  num_out <- outliers(robust2sls_object = robust2sls_object,
                      iteration = iteration)
  num_nonmiss <- (NROW(robust2sls_object$cons$data) -
                    sum(robust2sls_object$type[[accessor]] == -1))
  prop <- num_out / num_nonmiss
  return(prop)

}

#' Outlier history of single observation
#'
#' \code{outlier} takes a \code{"robust2sls"} object and the index of a specific
#' observation and returns its history of classification across the different
#' iterations contained in the \code{"robust2sls"} object.
#'
#' @inheritParams outliers
#' @param obs An index (row number) of an observation
#'

outlier <- function(robust2sls_object, obs) {

  iterations <- robust2sls_object$cons$iterations$actual
  hist <- numeric(length = 0)
  coln <- character(length = 0)

  for (i in seq_len(iterations + 1)) {

    acc <- paste("m", i-1, sep = "")
    hist <- c(hist, robust2sls_object$type[[acc]][[obs]])
    coln <- c(coln, acc)

  }

  hist <- t(as.matrix(hist, nrow = 1))
  rownames(hist) <- rownames(robust2sls_object$cons$data)[[obs]]
  colnames(hist) <- coln

  return(hist)

}


