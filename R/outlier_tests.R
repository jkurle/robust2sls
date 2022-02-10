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
#'
#' @export

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


#' Multiple models, varying cut-off
#'
#' \code{multi_cutoff()} runs several outlier detection algorithms that differ
#' in the value of the cut-off that determines whether an observation is
#' classified as an outlier or not.
#'
#' @param gamma A numeric vector representing the probability of falsely
#' classifying an observation as an outlier. One setting of the algorithm per
#' element of \code{gamma} is being run.
#' @param ... Arguments for specifying the other settings of the outlier
#' detection algorithm, \code{\link{outlier_detection}}.
#'
#' @details \code{mutli_cutoff} uses the
#'   \code{\link[foreach:foreach-package]{foreach}} and
#'   \code{\link[future:future]{future}} packages to run several models at the
#'   same time in parallel. This means the user has to register a backend and
#'   thereby determine how the code should be executed. The default is
#'   sequential, i.e. not in parallel. See
#'   \code{\link[future:plan]{future::plan()}} for details.
#'
#' @return A list containing the \code{robust2sls} objects, one per setting of
#' \code{gamma}. The length of the list therefore corresponds to the length of
#' the vector \code{gamma}.
#'
#' @importFrom foreach %dopar% %do%
#' @export

multi_cutoff <- function(gamma, ...) {

  if (!is.numeric(gamma) | !is.vector(gamma)) {
    stop("Argument 'gamma' must be a numeric vector.")
  }
  if (!all(gamma >= 0 & gamma <= 1)) {
    stop("Argument 'gamma' must lie between 0 and 1.")
  }

  results <- foreach::foreach(i = seq_along(gamma)) %dopar% {
    g <- gamma[i]
    outlier_detection(..., sign_level = g)
  }

  # name the elements: "gamma(gamma_i)"
  elenames <- paste("gamma", as.character(gamma), sep = "")
  names(results) <- elenames

  return(results)

}


#' Proportion test
#'
#' \code{proptest()} conducts a test whether the false outlier detection rate
#' (FODR) in the sample deviates significantly from its expected value
#' (population FODR) under the null hypothesis that there are no outliers in the
#' sample.
#'
#' @param robust2sls_object An object of class \code{"robust2sls"} or a
#' list of such objects.
#' @param alpha A numeric value between 0 and 1 representing the significance
#' level of the test.
#' @param iteration An integer >= 0 or the character "convergence" that
#' determines which iteration is used for the test.
#' @param one_sided A logical value whether a two-sided test (\code{FALSE})
#'   should be conducted or a one-sided test (\code{TRUE}) that rejects only
#'   when the false outlier detection rate is above its expected value.
#'
#' @details See \code{\link[=outlier_detection]{outlier_detection()}} and
#' \code{\link[=multi_cutoff]{multi_cutoff()}} for creating an object of class
#' \code{"robust2sls"} or a list thereof.
#'
#' @return \code{proptest()} returns a data frame with the iteration (m) to be
#' tested, the actual iteration that was tested (generally coincides with the
#' iteration that was specified to be tested but is the convergent iteration if
#' the fixed point is tested), the setting of the probability of exceeding the
#' cut-off (gamma), the type of t-test (one- or two-sided), the value of the
#' test statistic, its p-value, the significance level \code{alpha}, and the
#' decision. The number of rows of the data frame corresponds to the length of
#' the argument \code{robust2sls_object}.
#'
#' @export

proptest <- function(robust2sls_object, alpha, iteration, one_sided = FALSE) {

  # check input values
  if (!(identical(class(robust2sls_object), "robust2sls") | identical(class(robust2sls_object), "list"))) {
    stop("Argument 'robust2sls_object' must be of class 'robust2sls' or a list of such objects.")
  }
  classes <- sapply(X = robust2sls_object, FUN = class)
  if (identical(class(robust2sls_object), "list") && !all(classes == "robust2sls")) {
    stop("Argument 'robust2sls_object' is a list but not all elements have class 'robust2sls'.")
  }
  if (!is.numeric(alpha) | !identical(length(alpha), 1L)) {
    stop("Argument 'alpha' must be a numeric value of length one.")
  }
  if (!(alpha >= 0 & alpha <= 1)) {
    stop("Argument 'alpha' must be between 0 and 1.")
  }
  if (!(is.numeric(iteration) | identical(iteration, "convergence"))) {
    stop("Argument 'iteration' must either be a numeric or the string 'convergence'.")
  }
  if (is.numeric(iteration) & !identical(length(iteration), 1L)) {
    stop("Argument 'iteration' must be of length one.")
  }
  if (is.numeric(iteration) && !((iteration %% 1) == 0)) {
    stop("Argument 'iteration' must be an integer.")
  }
  if (!is.logical(one_sided) | !identical(length(one_sided), 1L)) {
    stop("Argument 'one_sided' must be a logical value of length one.")
  }

  # define auxiliary function
  proptest_fun <- function(r, iter) {
    if (iter == "convergence") {
      final_iter <- r$cons$iterations$actual
      actual_prop <- outliers_prop(robust2sls_object = r, iteration = final_iter)
    } else {
      actual_prop <- outliers_prop(robust2sls_object = r, iteration = iter)
    }
    expected_prop <- r$cons$sign_level
    n <- sum(nonmissing(data = r$cons$data, formula = r$cons$formula))
    est_param <- estimate_param_null(robust2SLS_object = r)
    est_avar <- gauge_avar(ref_dist = r$cons$reference,
                           sign_level = expected_prop,
                           initial_est = r$cons$initial$estimator,
                           iteration = iter,
                           parameters = est_param,
                           split = r$cons$initial$split)
    t <- sqrt(n)*(actual_prop - expected_prop)/sqrt(est_avar)
    return(t)
  }

  # check that selected iteration is actually available in robust2sls_object(s)
  if (identical(class(robust2sls_object), "robust2sls")) {
    iter_set <- robust2sls_object$cons$iterations$setting
    if (identical(iteration, "convergence")) {
      iter_act <- robust2sls_object$cons$iterations$actual
    } else {
      iter_act <- iteration
    }
    iter_max <- robust2sls_object$cons$convergence$max_iter
    gamma <- robust2sls_object$cons$sign_level
    t <- proptest_fun(r = robust2sls_object, iter = iteration)
    if (isTRUE(one_sided)) {
      type <- "one-sided"
      pval <- pnorm(q = t, mean = 0, sd = 1, lower.tail = FALSE)
    } else {
      type <- "two-sided"
      pval <- 2*pnorm(q = abs(t), mean = 0, sd = 1, lower.tail = FALSE)
    }
  } else if (identical(class(robust2sls_object), "list")) {
    # return list because could be different types (numeric or character for convergence)
    iter_set <- sapply(X = robust2sls_object, FUN = function(x) x$cons$iterations$setting)
    if (identical(iteration, "convergence")) {
      iter_act <- sapply(X = robust2sls_object, FUN = function(x) x$cons$iterations$actual)
    } else {
      iter_act <- iteration
    }
    iter_max <- sapply(X = robust2sls_object, FUN = function(x) x$cons$convergence$max_iter)
    # return vector because know it must be numeric
    gamma <- sapply(X = robust2sls_object, FUN = function(x) x$cons$sign_level)
    t <- sapply(X = robust2sls_object, FUN = proptest_fun, iter = iteration)
    if (isTRUE(one_sided)) {
      type <- "one-sided"
      pval <- sapply(X = t, FUN = function(x) pnorm(q = x, mean = 0, sd = 1, lower.tail = FALSE))
    } else {
      type <- "two-sided"
      pval <- sapply(X = t, FUN = function(x) 2*pnorm(q = abs(x), mean = 0, sd = 1, lower.tail = FALSE))
    }
  } else {
    stop("Argument 'robust2sls_object' invalid input type.")
  }

  out <- data.frame(iter_test = iteration, iter_act = iter_act, gamma = gamma, t = t, type = type,
                    pval = pval, alpha = alpha, reject = (pval <= alpha))

  return(out)

}

#' Count test
#'
#' \code{counttest()} conducts a test whether the number of detected outliers
#' deviates significantly from the expected number of outliers under the null
#' hypothesis that there are no outliers in the sample.
#' @inheritParams proptest
#' @param one_sided A logical value whether a two-sided test (\code{FALSE})
#'   should be conducted or a one-sided test (\code{TRUE}) that rejects only
#'   when the number of detected outliers is above the expected number.
#'
#' @details See \code{\link[=outlier_detection]{outlier_detection()}} and
#' \code{\link[=multi_cutoff]{multi_cutoff()}} for creating an object of class
#' \code{"robust2sls"} or a list thereof.
#'
#' @return \code{proptest()} returns a data frame with the iteration (m) to be
#' tested, the actual iteration that was tested (generally coincides with the
#' iteration that was specified to be tested but is the convergent iteration if
#' the fixed point is tested), the setting of the probability of exceeding the
#' cut-off (gamma), the number of detected outliers, the expected number of
#' outliers under the null hypothesis that there are no outliers, the type of
#' test (one- or two-sided), the p-value, the significance level \code{alpha},
#' and the decision. The number of rows of the data frame corresponds to the
#' length of the argument \code{robust2sls_object}.
#'
#' @export

counttest <- function(robust2sls_object, alpha, iteration, one_sided = FALSE) {

  # check input values
  if (!(identical(class(robust2sls_object), "robust2sls") | identical(class(robust2sls_object), "list"))) {
    stop("Argument 'robust2sls_object' must be of class 'robust2sls' or a list of such objects.")
  }
  classes <- sapply(X = robust2sls_object, FUN = class)
  if (identical(class(robust2sls_object), "list") && !all(classes == "robust2sls")) {
    stop("Argument 'robust2sls_object' is a list but not all elements have class 'robust2sls'.")
  }
  if (!is.numeric(alpha) | !identical(length(alpha), 1L)) {
    stop("Argument 'alpha' must be a numeric value of length one.")
  }
  if (!(alpha >= 0 & alpha <= 1)) {
    stop("Argument 'alpha' must be between 0 and 1.")
  }
  if (!(is.numeric(iteration) | identical(iteration, "convergence"))) {
    stop("Argument 'iteration' must either be a numeric or the string 'convergence'.")
  }
  if (is.numeric(iteration) & !identical(length(iteration), 1L)) {
    stop("Argument 'iteration' must be of length one.")
  }
  if (is.numeric(iteration) && !((iteration %% 1) == 0)) {
    stop("Argument 'iteration' must be an integer.")
  }
  if (!is.logical(one_sided) | !identical(length(one_sided), 1L)) {
    stop("Argument 'one_sided' must be a logical value of length one.")
  }

  # define auxiliary function
  counttest_fun <- function(r, iter, type) {
    if (isTRUE(type)) {
      alt <- "greater"
    } else {
      alt <- "two.sided"
    }
    if (iter == "convergence") {
      iter_act <- r$cons$iterations$actual
    } else {
      iter_act <- iter
    }
    num_act <- outliers(robust2sls_object = r, iteration = iter_act)
    n <- sum(nonmissing(data = r$cons$data, formula = r$cons$formula))
    gamma <- r$cons$sign_level
    num_exp <- gamma * n
    pvalue <- stats::poisson.test(x = num_act, r = num_exp,
                                  alternative = alt)$p.value

    if (identical(alt, "greater")) {
      alt2 <- "one-sided"
    } else {
      alt2 <- "two-sided"
    }

    out <- c(iter, iter_act, gamma, num_act, num_exp, alt2, pvalue)
    out <- data.frame(iter_test = iter, iter_act = iter_act, gamma = gamma,
                      num_act = num_act, num_exp = num_exp, type = alt2,
                      pval = pvalue)
    return(out)

  }

  if (identical(class(robust2sls_object), "robust2sls")) {
    result <- counttest_fun(r = robust2sls_object, iter = iteration, type = one_sided)
  } else if (identical(class(robust2sls_object), "list")) {
    iter_max <- sapply(X = robust2sls_object, FUN = function(x) x$cons$convergence$max_iter)
    result <- do.call(rbind, lapply(X = robust2sls_object, FUN = counttest_fun, iter = iteration, type = one_sided))
  } else {
    stop("Argument 'robust2sls_object' invalid input type.")
  }

  colnames(result) <- c("iter_test", "iter_act", "gamma", "num_act", "num_exp",
                        "type", "pval")
  result <- as.data.frame(result)
  result <- cbind(result, data.frame(alpha = alpha))
  result <- cbind(result, reject = (result$pval <= result$alpha))
  return(result)

}

#' Creates a vector of the centered FODR across different cut-offs
#'
#' \code{multi_cutoff_to_fodr_vec()} takes a list of \code{"robust2sls"} objects
#' and returns a vector of the centered FODR (sample - expected) for different
#' values of the cut-off c (equivalently gamma):
#' \loadmathjax
#' \mjdeqn{ \sqrt{n}(\widehat{\gamma_{c}} - \gamma_{c}) }{sqrt(n)(gamma_hat - gamma)}
#'
#' @param r A list of \code{"robust2sls"} objects.
#'
#' @details See \code{\link[=outlier_detection]{outlier_detection()}} and
#' \code{\link[=multi_cutoff]{multi_cutoff()}} for creating an object of class
#' \code{"robust2sls"} or a list thereof.
#'
#' @return A numeric vector of the centered FODR values.

multi_cutoff_to_fodr_vec <- function(r, iteration) {

  centered_fodr <- function(robust2sls_object, iteration) {

  }

}
