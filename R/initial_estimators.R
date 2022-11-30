
#' Robustified 2SLS (full sample initial estimator)
#'
#' \code{robustified_init} estimates the full sample 2SLS model, which is used
#' as the initial estimator for the iterative procedure.
#'
#' @param data A dataframe.
#' @param formula A formula in the format \code{y ~ x1 + x2 | x1 + z2} where
#' \code{y} is the dependent variable, \code{x1} are the exogenous regressors,
#' \code{x2} the endogenous regressors, and \code{z2} the outside instruments.
#' @param cutoff A numeric cutoff value used to judge whether an observation
#' is an outlier or not. If its absolute value is larger than the cutoff value,
#' the observations is classified as an outlier.
#'
#' @return \code{robustified_init} returns a list with five elements. The first
#' four are vectors whose length equals the number of observations in the data
#' set. Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), it does not ignore observations where any of y, x
#' or z are missing. It instead sets their values to \code{NA}.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with \code{TRUE} if
#' the observation is judged as not outlying, \code{FALSE} if it is an outlier,
#' and \code{NA} if any of y, x, or z are missing. The fourth element of the
#' list is an integer vector with three values: 0 if the observations is judged
#' to be an outlier, 1 if not, and -1 if missing. The fifth and last element
#' stores the \code{\link[ivreg]{ivreg}} model object based on which the four
#' vectors were calculated.
#'
#' @export

robustified_init <- function(data, formula, cutoff) {

  full <- ivreg::ivreg(formula = formula, data = data, model = TRUE, y = TRUE)

  # extract all variables appearing in the regression formula
  vars <- extract_formula(formula)
  y_var <- vars$y_var

  # calculate residuals, standardised residuals, selection and type vectors
  update_info <- selection(data = data, yvar = y_var, model = full,
                           cutoff = cutoff, bias_correction = NULL)

}

#' User-specified initial estimator
#'
#' \code{user_init} uses a model supplied by the user as the initial estimator.
#' Based on this estimator, observations are classified as outliers or not.
#'
#' @inheritParams robustified_init
#' @param user_model A model object of \link{class} \link[ivreg]{ivreg} whose
#' parameters are used to calculate the residuals.
#'
#' @section Warning:
#' Check \href{https://drive.google.com/file/d/1qPxDJnLlzLqdk94X9wwVASptf1MPpI2w/view}{Jiao (2019)}
#' about conditions on the initial estimator that should be satisfied for the
#' initial estimator (e.g. they have to be Op(1)).
#'
#' @return \code{user_init} returns a list with five elements. The first
#' four are vectors whose length equals the number of observations in the data
#' set. Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), it does not ignore observations where any of y, x
#' or z are missing. It instead sets their values to \code{NA}.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with \code{TRUE} if
#' the observation is judged as not outlying, \code{FALSE} if it is an outlier,
#' and \code{NA} if any of y, x, or z are missing. The fourth element of the
#' list is an integer vector with three values: 0 if the observations is judged
#' to be an outlier, 1 if not, and -1 if missing. The fifth and last element
#' stores the \code{\link[ivreg]{ivreg}} user-specified model object based on
#' which the four vectors were calculated.
#'
#' @export

user_init <- function(data, formula, cutoff, user_model) {

  if (!inherits(user_model, "ivreg")) {
    stop(strwrap("The argument `user_model` is not of class `ivreg`, the model
                 object class for 2SLS models from package `ivreg`",
                 initial = "", prefix = " "))
  }

  # when given a model, could also extract formula by user_model$formula
  # vars <- extract_formula(user_model$formula)

  # extract all variables appearing in the regression formula
  vars <- extract_formula(formula)
  y_var <- vars$y_var

  # calculate residuals, standardised residuals, selection and type vectors
  update_info <- selection(data = data, yvar = y_var, model = user_model,
                           cutoff = cutoff, bias_correction = NULL)

}

#' Saturated 2SLS (split-sample initial estimator)
#'
#' \code{saturated_init} splits the sample into two sub-samples. The 2SLS model
#' is estimated on both sub-samples and the estimates of one sub-sample are
#' used to calculate the residuals and hence outliers from the other sub-sample.
#'
#' @inheritParams robustified_init
#' @param shuffle A logical value (\code{TRUE} or \code{FALSE}) whether the
#' sample should be split into sub-samples randomly. If \code{FALSE}, the sample
#' is simply cut into two parts using the original order of the supplied data
#' set.
#' @param shuffle_seed A numeric value that sets the seed for shuffling the
#' data set before splitting it. Only used if \code{shuffle == TRUE}.
#' @param split A numeric value strictly between 0 and 1 that determines
#' in which proportions the sample will be split.
#'
#' @section Warning:
#' The estimator may have bad properties if the \code{split} is too unequal and
#' the sample size is not large enough.
#'
#' @return \code{saturated_init} returns a list with five elements. The first
#' four are vectors whose length equals the number of observations in the data
#' set. Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), it does not ignore observations where any of y, x
#' or z are missing. It instead sets their values to \code{NA}.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with \code{TRUE} if
#' the observation is judged as not outlying, \code{FALSE} if it is an outlier,
#' and \code{NA} if any of y, x, or z are missing. The fourth element of the
#' list is an integer vector with three values: 0 if the observations is judged
#' to be an outlier, 1 if not, and -1 if missing. The fifth and last element
#' is a list with the two initial \code{\link[ivreg]{ivreg}} model objects based
#' on the two different sub-samples.
#'
#' @export

saturated_init <- function(data, formula, cutoff, shuffle, shuffle_seed,
                           split = 0.5) {

  if (!is.numeric(split)) {
    stop(strwrap("The argument `split` has to be numeric", prefix = " ",
                 initial = ""))
  }

  if (split <= 0 | split >= 1) {
    stop(strwrap("The argument `split` has to lie strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }

  if (split < 0.25 | split > 0.75) {
    warning(strwrap("Very unequal `split`. May have bad properties if the sample
                    size is not large enough."))
  }

  if (!is.logical(shuffle)) {
    stop(strwrap("The argument `shuffle` has to be TRUE or FALSE.",
                 prefix = " ", initial = ""))
  }

  # extract all variables appearing in the regression formula
  vars <- extract_formula(formula)
  y_var <- vars$y_var

  # extract the number of observations that can be used in estimation
  # i.e. no y, x, z missing

  non_missing <- nonmissing(data = data, formula = formula)
  num_obs <- sum(non_missing)

  # extract index variable as vector
  ind <- 1:NROW(data)

  if (shuffle == TRUE) {

    # want to draw indices in random order and base the split on random order
    # since there can be observations with missing y, x, z need to exclude them
    # create vector with weights 0 if missing, 1 if not missing
    wght <- as.numeric(non_missing)
    set.seed(seed = shuffle_seed)
    ind_nonmissing <- sample(x = ind, size = num_obs, replace = FALSE,
                             prob = wght)

  } else {

    # keep all indices that correspond to non missing observations
    ind_nonmissing <- ind[non_missing]

  } # end shuffle

  # splitting the sample
  # splitpoint is the number where the sample is split
  # rounding to nearest integer smaller than num_obs * split
  splitpoint <- floor(num_obs * split)

  split1 <- ind_nonmissing[1:splitpoint] # selection vector for first split
  split2 <- ind_nonmissing[(splitpoint + 1):num_obs] # vector for second split

  # create two new variables in data set to indicate the subset to be used
  # TRUE/1 if used in that split, FALSE/0 if not
  # if these variables exist already in the data set, create new name
  # split1_1, split1_2, split1_3 etc. and split2_1, split2_2, ...

  i <- 1
  split1_name <- "split1"
  while (split1_name %in% colnames(data)) {
    split1_name <- paste("split1_", i, sep = "")
    i <- i + 1
  }

  i <- 1
  split2_name <- "split2"
  while (split2_name %in% colnames(data)) {
    split2_name <- paste("split2_", i, sep = "")
    i <- i + 1
  }

  data[[split1_name]] <- FALSE
  data[[split1_name]][split1] <- TRUE
  data[[split2_name]] <- FALSE
  data[[split2_name]][split2] <- TRUE

  # calculating separate models for each split
  # use the subset argument of ivreg(), which specifies which obs to use
  # problem: command looks for a variable in dataset to use for selection
  # need to use the new variables we created with names split1/2_name
  # cannot access via [[split1_name]] or data$split1_name
  # have to create the command as a string, then parse it to make it an
  # expression and then evaluate it
  # this way, we can use the contents of split1/2_name to refer to the var
  model_split1 <- NULL
  model_split2 <- NULL
  command1 <- paste("model_split1 <- ivreg::ivreg(formula = formula, data = data,
                    model = TRUE, y = TRUE, subset = ", split1_name, ")")
  expr1 <- parse(text = command1)
  eval(expr1)

  command2 <- paste("model_split2 <- ivreg::ivreg(formula = formula, data = data,
                    model = TRUE, y = TRUE, subset = ", split2_name, ")")
  expr2 <- parse(text = command2)
  eval(expr2)

  # following code is slightly different from first version
  # first version created more variables in original data set, which is prone
  # to errors; try to avoid now except for split1_name, split2_name
  # now rely on selection() function to get the residuals, selection and type
  # have checked that results are identical to first version

  update_info1 <- selection(data = data[split1,], yvar = y_var,
                            model = model_split2, cutoff = cutoff,
                            bias_correction = NULL)
  update_info2 <- selection(data = data[split2,], yvar = y_var,
                            model = model_split1, cutoff = cutoff,
                            bias_correction = NULL)

  # put the results together in the original order of the observations
  res <- rep(NA, times = NROW(data))
  res[split1] <- update_info1$res
  res[split2] <- update_info2$res
  names(res) <- rownames(data)

  stdres <- rep(NA, times = NROW(data))
  stdres[split1] <- update_info1$stdres
  stdres[split2] <- update_info2$stdres
  names(stdres) <- rownames(data)

  sel <- logical(length = NROW(data))
  sel[split1] <- (abs(stdres[split1]) <= cutoff)
  sel[split2] <- (abs(stdres[split2]) <= cutoff)
  names(sel) <- rownames(data)

  type <- as.numeric(non_missing) + as.numeric(sel) - 1
  type <- as.integer(type)
  names(type) <- rownames(data)

  model <- list(split1 = update_info1$model, split2 = update_info2$model)

  update_info <- list(res = res, stdres = stdres, sel = sel, type = type,
                      model = model)

  # remove columns created for indexing and splitting
  # seems to be unnecessary because of copy-on-modify behaviour
  # keep <- setdiff(colnames(data), c(split1_name, split2_name))
  # data <- data[, keep]

}

#' Impulse Indicator Saturation (IIS initial estimator)
#'
#' @inheritParams robustified_init
#' @inheritParams ivgets::ivisat
#' @param gamma A numeric value between 0 and 1 representing the significance
#'   level used for two-sided significance t-test on the impulse indicators.
#'   Corresponds to the probability of falsely classifying an observation as an
#'   outlier.
#' @param t.pval A numeric value between 0 and 1 representing the significance
#'   level for the Parsimonious Encompassing Test (PET).
#'
#' @return \code{iis_init} returns a list with five elements. The first
#' four are vectors whose length equals the number of observations in the data
#' set. Unlike the residuals stored in a model object (usually accessible via
#' \code{model$residuals}), it does not ignore observations where any of y, x
#' or z are missing. It instead sets their values to \code{NA}.
#'
#' The first element is a double vector containing the residuals for each
#' observation based on the model estimates. The second element contains the
#' standardised residuals, the third one a logical vector with \code{TRUE} if
#' the observation is judged as not outlying, \code{FALSE} if it is an outlier,
#' and \code{NA} if any of y, x, or z are missing. The fourth element of the
#' list is an integer vector with three values: 0 if the observations is judged
#' to be an outlier, 1 if not, and -1 if missing. The fifth and last element
#' stores the \code{\link[ivreg]{ivreg}} model object based on which the four
#' vectors were calculated.
#'
#' @section Note:
#' IIS runs multiple models, similar to \code{\link{saturated_init}} but with
#' multiple block search. These intermediate models are not recorded. For
#' simplicity, the element \code{$model} of the returned list stores the full
#' sample model result, identical to \code{\link{robustified_init}}.
#'
#' @export

iis_init <- function(data, formula, gamma, t.pval = gamma, do.pet = FALSE,
                     normality.JarqueB = NULL, turbo = FALSE, overid = NULL,
                     weak = NULL, parallel.options = parallel.options) {

  # can only use this function when "ivgets" is installed
  if (!requireNamespace("ivgets", quietly = TRUE)) { # nocov start
    stop("Package 'ivgets' must be installed to use this function.", .call = FALSE)
  } # nocov end

  # run full model as reference
  full <- ivreg::ivreg(formula = formula, data = data, model = TRUE, y = TRUE)

  # problems with ivregFun in combination with getsFun when have missings
  # reason: getsFun deletes trailing and leading NAs in x or y but not z (b/c treated separately)
  # causes error in ivregFun when binding y, x, and z because different length
  # need to ensure that run ivisat only on the subset of nonmissing observations
  # still keep track of all to get complete classification vector in the end

  # need to match indicators found in complete subset later to original obs
  rownames.orig <- rownames(data)
  rownames(data) <- as.character(1:NROW(data))

  # check which observations are complete
  complete <- nonmissing(data = data, formula = formula)
  complete_data <- data[complete, ]

  iismodel <- ivgets::ivisat(formula = formula, data = complete_data, iis = TRUE,
                             sis = FALSE, tis = FALSE, uis = FALSE,
                             blocks = NULL, ratio.threshold = 0.8,
                             max.block.size = 30, t.pval = gamma,
                             wald.pval = t.pval, do.pet = do.pet,
                             # don't need these two tests b/c iid model
                             ar.LjungB = NULL, arch.LjungB = NULL,
                             normality.JarqueB = normality.JarqueB,
                             info.method = "sc", include.1cut = FALSE,
                             include.empty = FALSE, max.paths = NULL,
                             parallel.options = parallel.options, turbo = turbo,
                             tol = 1e-07, max.regs = NULL,
                             print.searchinfo = FALSE, plot = NULL,
                             alarm = FALSE, overid = overid, weak = weak)

  if (is.null(iismodel$final)) {
    stop("IIS final model is NULL. See warning.")
  }

  vars <- extract_formula(formula)
  y_var <- vars$y_var

  update_info <- selection_iis(x = iismodel, data = data, yvar = y_var,
                               complete = complete, rownames_orig = rownames.orig,
                               refmodel = full)

}
