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

#' Simes (1986) procedure for multiple testing
#'
#' \code{simes()} takes a vector of p-values corresponding to individual null
#' hypotheses and performs the Simes (1986) procedure for the global null
#' hypothesis. The global null hypothesis is the intersection of all individual
#' null hypotheses.
#'
#' @param pvals A numeric vector of p-values corresponding to the p-values of
#' the individual null hypotheses.
#' @param alpha A numeric value representing the global significance level.
#'
#' @return \code{simes()} returns a list with three named elements.
#' \code{$reject} stores a logical value whether the global null hypothesis has
#' been rejected. \code{$alpha} stores the significance level that was chosen.
#' \code{$details} stores a matrix of the individual null hypothesis p-values,
#' the adjusted significance level according to Simes' procedure, and the
#' rejection decision for each individual hypothesis test.
#'
#' @details See
#' \href{https://academic.oup.com/biomet/article/73/3/751/250538}{Simes (1986)}.

simes <- function(pvals, alpha) {

  if(!is.numeric(pvals) | !is.vector(pvals)) {
    stop("Argument 'pvals' must be a numeric vector.")
  }
  if(!all(pvals >= 0 & pvals <= 1)) {
    stop("Elements of 'pvals' must lie between 0 and 1.")
  }
  if(!is.numeric(alpha) | !identical(length(alpha), 1L)) {
    stop("Argument 'alpha' must be a single numeric value.")
  }
  if(!(alpha >= 0 & alpha <= 1)) {
    stop("Argument 'alpha' must lie between 0 and 1.")
  }

  pvals <- sort(pvals, decreasing = FALSE)
  alpha_adj <- seq(1:length(pvals)) * alpha / length(pvals)
  comp <- pvals <= alpha_adj
  reject <- any(comp)

  details <- data.frame(pvals = pvals, alpha_adj = alpha_adj, reject = comp)
  out <- list(reject = reject, alpha = alpha, details = details)

}



