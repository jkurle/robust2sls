#' Outlier detection algorithms
#'
#' \code{outlier_detection} provides different types of outlier detection
#' algorithms depending on the arguments provided. The decision whether to
#' classify an observations as an outlier or not is based on its standardised
#' residual in comparison to some user-specified reference distribution. \cr
#' The algorithms differ mainly in two ways. First, they can differ by the use
#' of initial estimator, i.e. the estimator based on which the first
#' classification as outliers is made. Second, the algorithm can either be
#' iterated a fixed number of times or until the difference in coefficient
#' estimates between the most recent model and the previous one is smaller than
#' some user-specified convergence criterion. The difference is measured by
#' the L2 norm.
#'
#' @param data A dataframe.
#' @param formula A formula for the \code{\link[AER]{ivreg}} function, i.e. in
#' the format \code{y ~ x1 + x2 | x1 + z2} where \code{y} is the dependent
#' variable, \code{x1} are the exogenous regressors, \code{x2} the endogenous
#' regressors, and \code{z2} the outside instruments.
#' @param ref_dist A character vector that specifies the reference distribution
#' against which observations are classified as outliers. \code{"normal"} refers
#' to the normal distribution.
#' @param sign_level A numeric value between 0 and 1 that determines the cutoff
#' in the reference distribution against which observations are judged as
#' outliers or not.
#' @param initial_est A character vector that specifies the initial estimator
#' for the outlier detection algorithm. \code{"robustified"} means that the full
#' sample 2SLS is used as initial estimator. \code{"saturated"} splits the
#' sample into two parts and estimates a 2SLS on each subsample. The
#' coefficients of one subsample are used to calculate residuals and determine
#' outliers in the other subsample. \code{"user"} allows the user to specify a
#' model based on which observations are classified as outliers. See section
#' "Warning" for more information and conditions.
#' @param user_model A model object of \link{class} \link[AER]{ivreg}. Only
#' required if argument \code{initial_est} is set to \code{"user"}, otherwise
#' \code{NULL}.
#' @param iterations Either an integer >= 0 that specifies how often the outlier
#' detection algorithm is iterated, or the character vector
#' \code{"convergence"}. In the former case, the value \code{0} means that only
#' outlier classification based on the initial estimator is done. In the latter,
#' the alfgorithm is iterated until it converges, i.e. when the difference in
#' coefficient estimates between the most recent model and the previous one is
#' smaller than some user-specified convergence criterion.
#' @param convergence_criterion A numeric value or NULL. The algorithm stops as
#' soon as the difference in coefficient estimates between the most recent model
#' and the previous one is smaller than \code{convergence_criterion}. The
#' difference is measured by the L2 norm. If the argument is set to a numeric
#' value but \code{iterations} is an integer > 0 then the algorithm stops either
#' when it converged or when \code{iterations} is reached.
#' @param shuffle A logical value or \code{NULL}. Only used if
#' c. If \code{TRUE} then the sample is shuffled
#' before creating the subsamples.
#' @param shuffle_seed An integer value that will set the seed for shuffling the
#' sample or \code{NULL}. Only used if \code{initial_est == "saturated"} and
#' \code{shuffle == TRUE}.
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split.
#'
#' @return \code{outlier_detection} returns an object of class
#' \code{"robust2sls"}, which is a list with the following components:
#' \describe{
#'   \item{\code{$cons}}{A list which stores high-level information about the
#'   function call and some results. \code{$call} is the captured function call,
#'   \code{$formula} the formula argument, \code{$data} the original data set,
#'   \code{$reference} the chosen reference distribution to classify outliers,
#'   \code{$sign_level} the significance level, \code{$psi} the probability that
#'   an observation is not classified as an outlier under the null hypothesis
#'   of no outliers, \code{$cutoff} the cutoff used to classify outliers if
#'   their standardised residuals are larger than that value, \code{$bias_corr}
#'   a bias correction factor to account for potential false positives
#'   (observations classified as outliers even though they are not). There are
#'   three further elements that are lists themselves. \cr \cr
#'   \code{$initial} stores settings about the initial estimator:
#'   \code{$estimator} is the type of the initial estimator (e.g. robustified or
#'   saturated), \code{$split} how the sample is split (\code{NULL} if argument
#'   not used), \code{$shuffle} whether the sample is shuffled before splitting
#'   (\code{NULL} if argument not used), \code{$shuffle_seed} the value of the
#'   random seed (\code{NULL} if argument not used). \cr \cr
#'   \code{$convergence} stores information about the convergence of the
#'   outlier-detection algorithm:
#'   \code{$criterion} is the user-specified convergence criterion (\code{NULL}
#'   if argument not used), \code{$difference} is the L2 norm between the last
#'   coefficient estimates and the previous ones (\code{NULL} if argument not
#'   used or only initial estimator calculated). \code{$converged} is a logical
#'   value indicating whether the algorithm has converged, i.e. whether the
#'   difference is smaller than the convergence criterion (\code{NULL} if
#'   argument not used). \cr \cr
#'   \code{$iterations} contains information about the user-specified iterations
#'   argument (\code{$setting}) and the actual number of iterations that were
#'   done (\code{$actual}). The actual number can be lower if the algorithm
#'   converged already before the user-specified number of iterations were
#'   reached.}
#'   \item{\code{$model}}{A list storing the model objects of class
#'   \link[AER]{ivreg} for each iteration. Each model is stored under
#'   \code{$m0}, \code{$m1}, ...}
#'   \item{\code{$res}}{A list storing the residuals of all observations for
#'   each iteration. Residuals of observations where any of the y, x, or z
#'   variables used in the 2SLS model are missing are set to NA. Each vector is
#'   stored under \code{$m0}, \code{$m1}, ...}
#'   \item{\code{$stdres}}{A list storing the standardised residuals of all
#'   observations for each iteration. Standardised residuals of observations
#'   where any of the y, x, or z variables used in the 2SLS model are missing
#'   are set to NA. Standardisation is done by dividing by sigma, which is not
#'   adjusted for degrees of freedom. Each vector is stored under \code{$m0},
#'   \code{$m1}, ...}
#'   \item{\code{$sel}}{A list of logical vectors storing whether an observation
#'   is included in the estimation or not. Observations are excluded (FALSE) if
#'   they either have missing values in any of the x, y, or z variables needed
#'   in the model or when they are classified as outliers based on the model.
#'   Each vector is stored under \code{$m0}, \code{$m1}, ...}
#'   \item{\code{$type}}{A list of integer vectors indicating whether an
#'   observation has any missing values in x, y, or z (\code{-1}), whether it is
#'   classified as an outlier (\code{0}) or not (\code{1}). Each vector is
#'   stored under \code{$m0}, \code{$m1}, ...}
#' }
#'
#' @section Warning:
#' Check REFERENCE TO PAPER about conditions on the initial estimator that
#' should be satisfied for the initial estimator (e.g. they have to be Op(1)).
#'
#' @export

outlier_detection <- function(data, formula, ref_dist = c("normal"), sign_level,
  initial_est = c("robustified", "saturated", "user"), user_model = NULL,
  iterations = 1, convergence_criterion = NULL, shuffle = FALSE, shuffle_seed,
  split = 0.5) {

  # capture the original function call
  cll <- sys.call()

  # initialise the robust2sls (class) object
  out <- list(cons = list(), model = list(), res = list(), stdres = list(),
              sel = list(), type = list())

  # calculate and save the constant values
  constant <- constants(call = cll, formula = formula, data = data,
                        reference = ref_dist, sign_level = sign_level,
                        estimator = initial_est, split = split,
                        shuffle = shuffle, shuffle_seed = shuffle_seed,
                        iter = iterations, criterion = convergence_criterion)
  out$cons <- constant
  out <- new_robust2sls(x = out) # turn into object of class "robust2sls"
  cutoff <- constant$cutoff

  # extract dependent variable
  vars <- extract_formula(formula)
  y_var <- vars$y_var

  cat("Estimating iteration: 0")

  # initial estimation
  if (initial_est == "robustified") {
    initial <- robustified_init(data = data, formula = formula,
                                cutoff = out$cons$cutoff)
  } else if (initial_est == "saturated") {
    initial <- saturated_init(data = data, formula = formula,
                              cutoff = out$cons$cutoff, shuffle = shuffle,
                              shuffle_seed = shuffle_seed, split = split)
  } else if (initial_est == "user") {
    initial <- user_init(data = data, formula = formula,
                         cutoff = out$cons$cutoff, user_model = user_model)
  } else {
    stop(strwrap("Unknown `initial_est` argument", prefix = " ", initial = ""))
  }

  # add initial estimation results to out list
  iter_name <- "m0"
  out <- update_list(current_list = out, new_info = initial, name = iter_name)

  # how to call the variable in the data set that stores the subset selection
  # don't overwrite an existing variable so create name that does not yet exist
  i <- 1
  selection_name <- "selection"
  while (selection_name %in% colnames(data)) {
    selection_name <- paste("selection_", i, sep = "")
    i <- i + 1
  }
  remove(i)

  # initialise counter: how many interations were done?
  counter <- 1

  if (iterations == "convergence") {

    # initialise difference > convergence_criterion so that while loop starts
    difference <- convergence_criterion + 1

    while (difference > convergence_criterion) {

      # print progress
      cat(", ", counter, sep = "")

      # add latest selection vector as new variable to dataframe
      data[[selection_name]] <- out$sel[[length(out$sel)]]

      # new model of this iteration
      new <- NULL
      command <- paste("new <- AER::ivreg(formula = formula, data = data,
                    model = TRUE, y = TRUE, subset = ", selection_name, ")")
      expr <- parse(text = command)
      eval(expr)

      # calculate the res, stdres, sel, type & return model
      update_info <- selection(data = data, yvar = y_var, model = new,
                               cutoff = cutoff)

      # create new name and store results under this name
      iter_name <- paste("m", counter, sep = "")
      out <- update_list(current_list = out, new_info = update_info,
                         name = iter_name)

      # delete the selection variable, will be re-created in the next iteration
      to_drop <- selection_name # variable to drop
      data <- data[, !names(data) %in% to_drop] # only delete the selection var

      # calculate difference
      difference <- conv_diff(current = out, counter = counter)

      if (difference <= convergence_criterion) {
        cat("\n Algorithm converged successfully. Exit iterations.")
      }

      # update counter
      counter <- counter + 1

    } # end while convergence

  } else if (is.numeric(iterations)) { # end convergence, else numeric iter

    for (i in seq_len(iterations)) {

      # print progress
      cat(", ", counter, sep = "")

      # add latest selection vector as new variable to dataframe
      data[[selection_name]] <- out$sel[[length(out$sel)]]

      # new model of this iteration
      new <- NULL
      command <- paste("new <- AER::ivreg(formula = formula, data = data,
                    model = TRUE, y = TRUE, subset = ", selection_name, ")")
      expr <- parse(text = command)
      eval(expr)

      # calculate the res, stdres, sel, type & return model
      update_info <- selection(data = data, yvar = y_var, model = new,
                               cutoff = cutoff)

      # create new name and store results under this name
      iter_name <- paste("m", counter, sep = "")
      out <- update_list(current_list = out, new_info = update_info,
                         name = iter_name)

      # delete the selection variable, will be re-created in the next iteration
      to_drop <- selection_name # variable to drop
      data <- data[, !names(data) %in% to_drop] # only delete the selection var

      # calculate difference
      difference <- conv_diff(current = out, counter = counter)

      # update counter
      counter <- counter + 1

      # despite choosing fixed number of iterations, can end early if converged
      # this is only done when a convergence_criterion is specified (not NULL)
      if (!is.null(convergence_criterion)) {
        if (difference <= convergence_criterion) {
          cat("\n Algorithm converged successfully. Exit iterations.")
          break
        }
      } # end if break

    } # end for numeric

  } # end if numeric iterations

  out$cons$iterations$actual <- (length(out$type) - 1)

  if (!is.null(out$cons$convergence$criterion)) {
  out$cons$convergence$difference <- conv_diff(current = out, counter = counter)
  out$cons$convergence$converged <- (out$cons$convergence$difference
                                <= convergence_criterion)
  }

  return(out)

}
