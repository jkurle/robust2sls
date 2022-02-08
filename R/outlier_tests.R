#' Critical and p-value for test statistic relative to simulated distribution
#'
#' \code{test_cpv} returns the critical value corresponding to a given
#' quantile of the simulated distribution and the p-value of the test statistic.
#'
#' @param dist A numeric vector of simulated values approximating the
#' distribution of the test statistic, e.g. generated as in \code{mvn_sup()}.
#' @param teststat A numeric value of the test statistic.
#' @param p A numeric vector of probabilities with values in [0,1] for which the
#' corresponding quantiles are calculated.
#'
#' @return A list with two named entries. \code{$pval} is the p-value of the
#' test statistic with respect to the distribution \code{dist}. \code{$q} is the
#' vector of sample quantiles in the distribution \code{dist} corresponding to
#' the probabilities specified in \code{p}.
#'
#' @export

test_cpv <- function(dist, teststat, p) {

  if (!is.numeric(dist) | !is.vector(dist)) {
    stop("Argument 'dist' must be a numeric vector.")
  }
  if (!is.numeric(teststat) | !identical(length(teststat), 1L)) {
    stop("Argument 'teststat' must be a single numeric value.")
  }
  if (!is.numeric(p) | !is.vector(p)) {
    stop("Argument 'p' must be a numeric vector.")
  }
  if (!all(p >= 0 & p <= 1)) {
    stop("Elements of 'p' must lie between 0 and 1.")
  }

  # find the p-value of the value of the test statistic
  pval <- mean(dist > teststat)

  # find the critical value corresponding to q (can be vector)
  dist <- sort(dist, decreasing = FALSE)
  sampleq <- quantile(x = dist, probs = p, type = 8)

  return(list(pval = pval, critical = sampleq))

}





