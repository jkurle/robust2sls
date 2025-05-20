#' robust2sls: A package for outlier robust 2SLS inference and testing
#'
#' @description
#' The robust2sls package provides two main functionalities. First, it
#' implements an algorithm for determining whether an observations is an outlier
#' based on its standardized residual and re-estimation based on the sub-sample
#' excluding all outliers. This procedure is often used in empirical research to
#' show that the results are not driven by outliers. This package has
#' implemented the algorithm in various forms and the user can select between
#' different initial estimators and how often the algorithm is iterated. The
#' statistical inference is adapted to account for potential false positives
#' (classifying observations as outliers even though they are not).
#'
#' Second, the robust2sls package provides easy-to-use statistical tests on
#' whether the difference between the original and the outlier-robust estimates
#' is statistically significant. Furthermore, several different statistical
#' tests are implemented to test whether the sample actually contains outliers.
#'
#' @name robust2sls-package
"_PACKAGE"

