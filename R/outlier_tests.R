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
#' @keywords internal

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
  sampleq <- stats::quantile(x = dist, probs = p, type = 8)

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
#' @keywords internal

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

  details <- data.frame(pvals = pvals, alpha_adj = alpha_adj, reject_adj = comp)
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

  # binding for i
  i <- NULL

  results <- foreach::foreach(i = (1:length(gamma))) %dopar% {
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
      pval <- stats::pnorm(q = t, mean = 0, sd = 1, lower.tail = FALSE)
    } else {
      type <- "two-sided"
      pval <- 2 * stats::pnorm(q = abs(t), mean = 0, sd = 1, lower.tail = FALSE)
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
      pval <- sapply(X = t, FUN = function(x) stats::pnorm(q = x, mean = 0, sd = 1, lower.tail = FALSE))
    } else {
      type <- "two-sided"
      pval <- sapply(X = t, FUN = function(x) 2 * stats::pnorm(q = abs(x), mean = 0, sd = 1, lower.tail = FALSE))
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
#' @param robust2sls_object A list of \code{"robust2sls"} objects.
#' @inheritParams proptest
#'
#' @details See \code{\link[=outlier_detection]{outlier_detection()}} and
#' \code{\link[=multi_cutoff]{multi_cutoff()}} for creating an object of class
#' \code{"robust2sls"} or a list thereof.
#'
#' @return A numeric vector of the centered FODR values.
#'
#' @keywords internal

multi_cutoff_to_fodr_vec <- function(robust2sls_object, iteration) {

  if (!identical(class(robust2sls_object), "list")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  classes <- sapply(X = robust2sls_object, FUN = class)
  if (is.list(robust2sls_object) && !all(classes == "robust2sls")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  if (!(is.numeric(iteration) | identical(iteration, "convergence"))) {
    stop("Argument 'iteration' must be numeric or the string 'convergence'.")
  }
  if (is.numeric(iteration)) {
    if (!((iteration %% 1) == 0) | iteration < 0) {
      stop("Argument 'iteration' must be an integer >= 0 if numeric.")
    }
  }

  centered_fodr <- function(r, iteration) {
    if (identical(iteration, "convergence")) {
      iter_act <- r$cons$iterations$actual
    } else {
      iter_act <- iteration
    }
    n <- sum(nonmissing(data = r$cons$data, formula = r$cons$formula))
    gamma_hat <- outliers_prop(robust2sls_object = r, iteration = iter_act)
    gamma <- r$cons$sign_level
    out <- sqrt(n) * (gamma_hat - gamma)
    return(out)
  }

  vec <- sapply(X = robust2sls_object, FUN = centered_fodr, iteration = iteration)
  return(vec)

}


#' Scaling sum proportion test across different cut-offs
#'
#' \code{sumtest()} uses the estimations across several cut-offs to test whether
#' the sum of the deviations between sample and population FODR differ
#' significantly from its expected value.
#' \loadmathjax
#' \mjdeqn{ \sum_{k = 1}^{K} \sqrt{n}(\widehat{\gamma}_{c_{k}} - \gamma_{c_{k}}) }{}
#'
#' @param robust2sls_object A list of \code{"robust2sls"} objects.
#' @inheritParams proptest
#'
#' @return \code{sumtest()} returns a data frame with one row storing the
#' iteration that was tested, the value of the test statistic (t-test), the
#' type of the test (one- or two-sided), the corresponding p-value, the
#' significance level, and whether the null hypothesis is rejected. The data
#' frame also contains an attribute named \code{"gammas"} that records which
#' gammas determining the different cut-offs were used in the scaling sum test.
#'
#' @export

sumtest <- function(robust2sls_object, alpha, iteration, one_sided = FALSE) {

  # input checks
  if (!identical(class(robust2sls_object), "list")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  classes <- sapply(X = robust2sls_object, FUN = class)
  if (is.list(robust2sls_object) && !all(classes == "robust2sls")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  if (identical(length(robust2sls_object), 1L)) {
    stop("sumtest() requires several different cutoffs. See proptest() and counttest() for single-value tests.")
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

  # extract settings of the outlier detection
  ref <- robust2sls_object[[1]]$cons$reference
  init <- robust2sls_object[[1]]$cons$initial$estimator
  split <- robust2sls_object[[1]]$cons$initial$split # could be NULL
  param_est <- estimate_param_null(robust2SLS_object = robust2sls_object[[1]])

  # extract which gammas are in the models
  gammas <- sapply(X = robust2sls_object, FUN = function(x) x$cons$sign_level)

  # turn list of robust2sls_objects to vector of the scaled FODR deviations
  vec <- multi_cutoff_to_fodr_vec(robust2sls_object = robust2sls_object,
                                  iteration = iteration)

  # create value of the test statistic
  test_value <- sum(vec)

  # calculate the asymptotic variance of the test statistic
  # technically can rely completely on gauge_covar and sum all entries
  # as internal check, also calculate explicitly with gauge_avar
  # variance part
  var_part <- sapply(X = gammas, FUN = gauge_avar, ref_dist = ref,
                     initial_est = init, iteration = iteration,
                     parameters = param_est, split = split)
  covar_part <- sapply(X = gammas, FUN = function(x) sapply(X = gammas,
                                                            FUN = gauge_covar,
                                                            ref_dist = "normal",
                                                            initial_est = init,
                                                            iteration = iteration,
                                                            parameters = param_est,
                                                            split = split,
                                                            sign_level1 = x))

  # covariance matrix should be symmetric, so implement an internal check
  if (!isSymmetric.matrix(covar_part)) {
    stop("Internal error. Covariance matrix of vector should be symmetric.")
  }

  test_var1 <- sum(var_part) + 2 * sum(covar_part[upper.tri(covar_part,
                                                            diag = FALSE)])
  test_var2 <- sum(covar_part)

  if (!isTRUE(all.equal(test_var1, test_var2))) {
    stop("Internal error. Two ways of calculating avar should coincide.")
  }

  # calculate test statistic and other outputs to be returned
  t <- test_value / sqrt(test_var2)
  if (isTRUE(one_sided)) {
    type <- "one-sided"
    pval <- stats::pnorm(q = t, mean = 0, sd = 1, lower.tail = FALSE)
  } else {
    type <- "two-sided"
    pval <- 2 * stats::pnorm(q = abs(t), mean = 0, sd = 1, lower.tail = FALSE)
  }

  out <- data.frame(iter_test = iteration, t = t, type = type,
                    pval = pval, alpha = alpha, reject = (pval <= alpha))
  names(gammas) <- NULL
  attr(x = out, which = "gammas") <- gammas

  return(out)

}

#' Supremum proportion test across different cut-offs
#'
#' \code{suptest()} uses the estimations across several cut-offs to test whether
#' the supremum/maximum of the deviations between sample and population FODR
#' differs significantly from its expected value.
#' \loadmathjax
#' \mjdeqn{ \sup_{c} |\sqrt{n}(\widehat{\gamma}_{c} - \gamma_{c})| }{}
#'
#' @inheritParams sumtest
#' @inheritParams test_cpv
#' @param R An integer specifying the number of replications for simulating the
#' distribution of the test statistic.
#'
#' @return \code{suptest()} returns a data frame with one row storing the
#' iteration that was tested, the value of the test statistic, the corresponding
#' p-value, the significance level, and whether the null hypothesis is rejected.
#' The data frame also contains two named attributes. The first attribute is
#' named \code{"gammas"} and records which gammas determining the different
#' cut-offs were used in the scaling sup test. The second attribute is named
#' \code{"critical"} and records the critical values corresponding to the
#' different quantiles in the limiting distribution that were specified in
#' \code{p}.
#'
#' @export

suptest <- function(robust2sls_object, alpha, iteration, p = c(0.9, 0.95, 0.99),
                    R = 50000) {

  # input checks
  if (!identical(class(robust2sls_object), "list")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  classes <- sapply(X = robust2sls_object, FUN = class)
  if (is.list(robust2sls_object) && !all(classes == "robust2sls")) {
    stop("Argument 'robust2sls_object' must be a list of 'robust2sls' objects.")
  }
  if (identical(length(robust2sls_object), 1L)) {
    stop("suptest() requires several different cutoffs. See proptest() and counttest() for single-value tests.")
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
  if (!is.numeric(p)) {
    stop("Argument 'p' must be a numeric vector.")
  }
  if (!all(p >= 0 & p <= 1)) {
    stop("Argument 'p' can only contain elements between 0 and 1.")
  }

  # extract settings of the outlier detection
  ref <- robust2sls_object[[1]]$cons$reference
  init <- robust2sls_object[[1]]$cons$initial$estimator
  split <- robust2sls_object[[1]]$cons$initial$split # could be NULL
  param_est <- estimate_param_null(robust2SLS_object = robust2sls_object[[1]])

  # extract which gammas are in the models
  gammas <- sapply(X = robust2sls_object, FUN = function(x) x$cons$sign_level)

  # turn list of robust2sls_objects to vector of the scaled FODR deviations
  vec <- multi_cutoff_to_fodr_vec(robust2sls_object = robust2sls_object,
                                  iteration = iteration)

  # create value of the test statistic
  test_value <- max(abs(vec))

  # for critical value / p-value need to simulate the distribution of the sup
  # create var-cov matrix of the Gaussian process
  varcov <- sapply(X = gammas, FUN = function(x) sapply(X = gammas,
                                                        FUN = gauge_covar,
                                                        ref_dist = "normal",
                                                        initial_est = init,
                                                        iteration = iteration,
                                                        parameters = param_est,
                                                        split = split,
                                                        sign_level1 = x))

  sup_distr <- mvn_sup(n = R, mu = rep(0, length(gammas)), Sigma = varcov)

  res <- test_cpv(dist = sup_distr, teststat = test_value,
                  p = p)
  pvalue <- res$pval
  criticalvalues <- res$critical
  rej <- pvalue <= alpha

  out <- data.frame(iter_test = iteration, test_value = test_value,
                    pval = pvalue, alpha = alpha, reject = rej)
  names(gammas) <- NULL
  attr(x = out, which = "gammas") <- gammas
  attr(x = out, which = "critical") <- criticalvalues

  return(out)

}

#' Global test correcting for multiple hypothesis testing
#'
#' \code{globaltest()} uses several proportion or count tests with different
#' cut-offs to test a global hypothesis of no outliers using the Simes (1986)
#' procedure to account for multiple testing.
#'
#' @param tests A data frame that contains a column named \code{$pval}
#' containing the different p-values for different hypothesis tests, each
#' stored in a row.
#' @param global_alpha A numeric value representing the global significance
#' level.
#'
#' @seealso [proptest()], [counttest()]
#'
#' @details See
#' \href{https://academic.oup.com/biomet/article/73/3/751/250538}{Simes (1986)}.
#'
#' @return A list with three entries. The first entry named \code{$reject}
#'   contains the global rejection decision. The second entry named
#'   \code{$global_alpha} stores the global significance level. The third entry
#'   named \code{$tests} returns the input data frame \code{tests}, appended
#'   with two columns containing the adjusted significance level and respective
#'   rejection decision.
#'
#' @export

globaltest <- function(tests, global_alpha) {

  if (!is.data.frame(tests)) {
    stop("Argument 'tests' must be a data frame.")
  }
  if (!("pval" %in% colnames(tests))) {
    stop("Argument 'tests' must contain a column named 'pval' containing the p-values of the individual tests.")
  }
  if (!(is.numeric(global_alpha)) | !identical(length(global_alpha), 1L)) {
    stop("Argument 'global_alpha' must be a numeric value of length one.")
  }
  if (!(global_alpha >= 0 & global_alpha <= 1)) {
    stop("Argument 'global_alpha' must be between 0 and 1.")
  }

  # calculate decisions based on Simes procedure
  pvalues <- tests$pval
  simes_res <- simes(pvals = pvalues, alpha = global_alpha)

  # add column that saves the original order
  colno <- NCOL(tests)
  tests$id <- 1:NROW(tests)
  # sort "tests" by p-value, as the simes() function does
  tests <- tests[order(tests$pval), ]
  tests$alpha_adj <- simes_res$details$alpha_adj
  tests$reject_adj <- simes_res$details$reject_adj
  tests <- tests[order(tests$id), ]
  tests <- tests[, -(colno+1)]

  out <- list(reject = simes_res$reject, global_alpha = global_alpha,
              tests = tests)
  return(out)

}

