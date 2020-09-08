#' Constructor of robust2sls class
#'
#' \code{new_robust2sls} turns a list into an object of \link{class}
#' \code{"robust2sls"}
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
#' \code{validate_robust2sls} checks that the input is a valid object of
#' \link{class} \code{"robust2sls"}
#'
#' @param x An object whose validity of class "\code{robust2sls}" is tested.
#'
#' @export

validate_robust2sls <- function(x) {

  values <- unclass(x)
  if (!identical(class(x), "robust2sls")) {
    stop(strwrap("Object x is not of class 'robust2sls'", prefix = " ",
                 initial = ""))
  }

  # test that it is a list
  if (!is.list(x)) {
    stop(strwrap("Object must be a list", prefix = " ", initial = ""))
  }
  # test that list has length 6
  if (!identical(length(x), 6L)) {
    stop(strwrap("Object must be a list with 6 elements", prefix = " ",
                 initial = ""))
  }
  # test that list has the correct named components
  name <- c("cons", "model", "res", "stdres", "sel", "type")
  if (!identical(names(x), name)) {
    stop(strwrap(paste(c("Object must have 6 named components:", name),
                       collapse = " ")), prefix = " ", initial = "")
  }
  # test that component $cons is a list
  if (!is.list(x$cons)) {
    stop(strwrap("Component $cons must be a list", prefix = " ", initial = ""))
  }
  # test that sublist $cons has length 11
  if (!identical(length(x$cons), 11L)) {
    stop(strwrap("Component $cons must be a list with 11 elements",
                 prefix = " ", initial = ""))
  }
  # test that sublist $cons has the correct named components
  cons_name <- c("call", "formula", "data", "reference", "sign_level", "psi",
                 "cutoff", "bias_corr", "initial", "convergence", "iterations")
  if (!identical(names(x$cons), cons_name)) {
    stop(strwrap(paste(c("Component $cons must have 11 named components:",
                      cons_name), collapse = " ")), prefix = " ", initial = "")
  }
  # test that subsublist $cons$initial is a list
  if (!is.list(x$cons$initial)) {
    stop(strwrap("Component $cons$initial must be a list", prefix = " ",
                 initial = ""))
  }
  # test that subsublist $cons$initial has length 4
  if (!identical(length(x$cons$initial), 4L)) {
    stop(strwrap("Component $cons$initial must be a list with 4 elements",
                 prefix = " ", initial = ""))
  }
  # test that subsublist $cons$initial has the correct named components
  cons_initial_name <- c("estimator", "split", "shuffle", "shuffle_seed")
  if (!identical(names(x$cons$initial), cons_initial_name)) {
    stop(strwrap(paste(c("Component $cons$initial must have 4 named
                    components:", cons_initial_name), collapse = " ")),
                    prefix = " ", initial = "")
  }
  # test that subsublist $cons$convergence is a list
  if (!is.list(x$cons$convergence)) {
    stop(strwrap("Component $cons$convergence must be a list", prefix = " ",
                 initial = ""))
  }
  # test that subsublist $cons$convergence has length 3
  if (!identical(length(x$cons$convergence), 3L)) {
    stop(strwrap("Component $cons$convergence must be a list with 3 elements",
                 prefix = " ", initial = ""))
  }
  # test that subsublist $cons$convergence has the correct named components
  cons_convergence_name <- c("criterion", "difference", "converged")
  if (!identical(names(x$cons$convergence), cons_convergence_name)) {
    stop(strwrap(paste(c("Component $cons$convergence must have 3 named
                    components:", cons_convergence_name), collapse = " ")),
         prefix = " ", initial = "")
  }
  # test that subsublist $cons$iterations is a list
  if (!is.list(x$cons$iterations)) {
    stop(strwrap("Component $cons$iterations must be a list", prefix = " ",
                 initial = ""))
  }
  # test that subsublist $cons$iterations has length 2
  if (!identical(length(x$cons$iterations), 2L)) {
    stop(strwrap("Component $cons$iterations must be a list with 2 elements",
                 prefix = " ", initial = ""))
  }
  # test that subsublist $cons$iterations has the correct named components
  cons_iterations_name <- c("setting", "actual")
  if (!identical(names(x$cons$iterations), cons_iterations_name)) {
    stop(strwrap(paste(c("Component $cons$iterations must have 2 named
                    components:", cons_iterations_name), collapse = " ")),
         prefix = " ", initial = "")
  }
  # test that sublist $model is a list
  if (!is.list(x$model)) {
    stop(strwrap("Component $model must be a list", prefix = " ",
                 initial = ""))
  }
  # test that sublist $res is a list
  if (!is.list(x$res)) {
    stop(strwrap("Component $res must be a list", prefix = " ",
                 initial = ""))
  }
  # test that sublist $stdres is a list
  if (!is.list(x$stdres)) {
    stop(strwrap("Component $stdres must be a list", prefix = " ",
                 initial = ""))
  }
  # test that sublist $sel is a list
  if (!is.list(x$sel)) {
    stop(strwrap("Component $sel must be a list", prefix = " ",
                 initial = ""))
  }
  # test that sublist $type is a list
  if (!is.list(x$type)) {
    stop(strwrap("Component $type must be a list", prefix = " ",
                 initial = ""))
  }
  # test that sublists $model, $res, $stdres, $sel, $type have same length
  if (!(identical(length(x$model), length(x$res)) &
      identical(length(x$res), length(x$stdres)) &
      identical(length(x$stdres), length(x$sel)) &
      identical(length(x$sel), length(x$type)))) {
    stop(strwrap("The length of components $model, $res, $stdres, $sel, $type
                 must be the same", prefix = " ", initial = ""))
  }
  # if robust2sls has been estimated (not only initialised) then can check
  # for naming of the components of $model, $res etc.
  if (x$cons$iterations$actual >= 0) {
    max.iter <- x$cons$iterations$actual
    iter.names <- character(length = 0)
    for (i in seq_len(max.iter + 1)) {
      new <- paste("m", i-1, sep = "")
      iter.names <- c(iter.names, new)
    }
    # test that subsublists $model, $res etc. have correct length
    if (!identical(length(x$type), as.integer(max.iter + 1))) {
      stop(strwrap("Number of actual iterations and number of elements in
                   $model, $res etc. not consistent", prefix = " ",
                   initial = ""))
    }
    # test that subsublists $model, $res etc. have correct names
    if (!(identical(names(x$model), iter.names) &
        identical(names(x$res), iter.names) &
        identical(names(x$stdres), iter.names) &
        identical(names(x$sel), iter.names) &
        identical(names(x$type), iter.names))) {
      stop(strwrap("Components $model, $res, $stdres, $sel, and $type should
                   have named components called 'm0', 'm1' etc.", prefix = " ",
                   initial = ""))
    }
  } # end if axctual iterations >= 0

  # test that the elements are of the correct type or class
  if (!identical(typeof(x$cons$call), "language") &
      !identical(class(x$cons$call), "call")) {
    stop(strwrap("Component $cons$call must be a valid function call (class
                 'call' and type 'language')", prefix = " ", initial = ""))
  }
  if (!identical(typeof(x$cons$formula), "language") &
      !identical(class(x$cons$formula), "formula")) {
    stop(strwrap("Component $cons$formula must be a valid formula (class
                 'formula' and type 'language')", prefix = " ", initial = ""))
  }
  if (!is.data.frame(x$cons$data)) {
    stop(strwrap("Component $cons$data must be a data frame", prefix = " ",
                 initial = ""))
  }
  if (!is.character(x$cons$reference) &
      !identical(length(x$cons$reference), 1L)) {
    stop(strwrap("Component $cons$reference must be a character vector of length
                 1", prefix = " ", initial = ""))
  }
  # available reference distributions (so far only "normal"):
  ref_dist <- c("normal")
  if (!(x$cons$reference %in% ref_dist)) {
    stop(strwrap(paste(c("Component $cons$reference must be one of the available
                 reference distributions:", ref_dist), collapse = " "),
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(x$cons$sign_level)) {
    stop(strwrap("Component $cons$sign_level must be numeric", prefix = " ",
                 initial = ""))
  }
  if (!(x$cons$sign_level > 0 & x$cons$sign_level < 1)) {
    stop(strwrap("Component $cons$sign_level must be strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(x$cons$psi)) {
    stop(strwrap("Component $cons$psi must be numeric", prefix = " ",
                 initial = ""))
  }
  if (!(x$cons$psi > 0 & x$cons$psi < 1)) {
    stop(strwrap("Component $cons$psi must be strictly between 0 and 1",
                 prefix = " ", initial = ""))
  }
  if (!is.numeric(x$cons$cutoff)) {
    stop(strwrap("Component $cons$cutoff must be numeric", prefix = " ",
                 initial = ""))
  }
  if (!is.numeric(x$cons$bias_corr)) {
    stop(strwrap("Component $cons$bias_corr must be numeric", prefix = " ",
                 initial = ""))
  }
  if (!(x$cons$bias_corr > 1)) { # I think this should always be the case
    stop(strwrap("Component $cons$bias_corr must be > 1", prefix = " ",
                 initial = ""))
  }
  if (!is.character(x$cons$initial$estimator) |
      !identical(length(x$cons$initial$estimator), 1L)) {
    stop(strwrap("Component $cons$initial$estimator must be a character vector
                 of length 1", prefix = " ", initial = ""))
  }
  initials <- c("robustified", "saturated", "user")
  if (!(x$cons$initial$estimator %in% initials)) {
    stop(strwrap(paste(c("Component $cons$initial$estimator must be one of the
          available initial estimators:", initials), collapse = " "),
          prefix = " ", initial = ""))
  }
  if (!identical(x$cons$initial$estimator, "saturated")) { # NOT 'saturated'
    if (!is.null(x$cons$initial$split)) {
      stop(strwrap("Component $cons$initial$split should be NULL when the
                   initial estimator is NOT 'saturated'", prefix = " ",
                   initial = ""))
    }
    if (!is.null(x$cons$initial$shuffle)) {
      stop(strwrap("Component $cons$initial$shuffle should be NULL when the
                   initial estimator is NOT 'saturated'", prefix = " ",
                   initial = ""))
    }
    if (!is.null(x$cons$initial$shuffle_seed)) {
      stop(strwrap("Component $cons$initial$shuffle_seed should be NULL when the
                   initial estimator is NOT 'saturated'", prefix = " ",
                   initial = ""))
    }
  } else { # when 'saturated'
    if (!is.numeric(x$cons$initial$split)) {
      stop(strwrap("Component $cons$initial$split must be numeric when the
              initial estimator is 'saturated'", prefix = " ", initial = ""))
    }
    if (!(x$cons$initial$split > 0 & x$cons$initial$split < 1)) {
      stop(strwrap("Component $cons$initial$split must be strictly between 0 and
                   1 when the initial estimator is 'saturated'", prefix = " ",
                   initial = ""))
    }
    if (!is.logical(x$cons$initial$shuffle) |
        !identical(length(x$cons$initial$shuffle), 1L)) {
      stop(strwrap("Component $cons$initial$shuffle must be a numeric vector of
                   length 1, i.e. TRUE or FALSE, when the initial estimator is
                   'saturated'", prefix = " ", initial = ""))
    }
    if (identical(x$cons$initial$shuffle, TRUE)) { # when shuffle == TRUE
      if (!is.numeric(x$cons$initial$shuffle_seed) |
          !identical(length(x$cons$initial$shuffle_seed), 1L)) {
        stop(strwrap("Component $cons$initial$shuffle_seed must be a numeric
                     vector of length 1 when the initial estimator is
                     'saturated' and when 'shuffle' is TRUE", prefix = " ",
                     initial = ""))
      }
    } else { # when shuffle != TRUE
      if(!is.null(x$cons$initial$shuffle_seed)) {
        stop(strwrap("Component $cons$initial$shuffle_seed must be NULL when
                     'shuffle' is FALSE", prefix = " ", initial = ""))
      }
    } # end shuffle
  } # end saturated
  if (!is.null(x$cons$convergence$criterion) &
      !is.numeric(x$cons$convergence$criterion)) {
    stop(strwrap("Component $cons$convergence$criterion must either be NULL or
                 numeric", prefix = " ", initial = ""))
  }
  if (identical(x$cons$iterations$setting, "convergence")) {
    # when iterations == "convergence" then need to specify conv criterion
    if (!is.numeric(x$cons$convergence$criterion) |
        !(x$cons$convergence$criterion >= 0)) {
      stop(strwrap("Component $cons$convergence$criterion must be numeric >= 0",
                   prefix = " ", initial = ""))
    }
  } # end if iterations == "convergence"
  if (is.numeric(x$cons$convergence$criterion)) {
    # if a convergence criterion has been specified (irrespective of whether
    # iterations == "convergence" or some numeric) then need a calculated
    # value for $difference and a logical value for $converged
    if (!is.numeric(x$cons$convergence$difference) |
        !(x$cons$convergence$difference >= 0)) {
      stop(strwrap("Component $cons$convergence$difference must be a numeric
                   value >= 0 when a convergence criterion has been specified",
                   prefix = " ", initial = ""))
    }
    if (!is.logical(x$cons$convergence$converged) |
        !identical(length(x$cons$convergence$converged), 1L)) {
      stop(strwrap("Component $cons$convergence$converged must be a logical
                   vector of length 1, i.e. TRUE or FALSE, when a convergence
                   criterion has been specified", prefix = " ", initial = ""))
    }
  } else { # convergence criterion is NULL
    if (!is.null(x$cons$convergence$difference)) {
      stop(strwrap("Component $cons$convergence$difference must be NULL when
                   no convergence criterion has been specified", prefix = " ",
                   initial = ""))
    }
    if (!is.null(x$cons$convergence$converged)) {
      stop(strwrap("Component $cons$convergence$converged must be NULL when no
                   convergence criterion has been specified", prefix = " ",
                   initial = ""))
    }
  } # end convergence criterion
  if (!identical(x$cons$iterations$setting, "convergence") &
      !is.numeric(x$cons$iterations$setting)) {
    stop(strwrap("Component x$cons$iterations$setting must either be numeric or
                 the character 'convergence'", prefix = " ", initial = ""))
  }
  if (!is.numeric(x$cons$iterations$actual) |
      !identical(length(x$cons$iterations$actual), 1L) |
      !(x$cons$iterations$actual >= 0)) {
    stop(strwrap("Component x$cons$iterations$actual must be a numeric value
                 >= 0", prefix = " ", initial = ""))
  }
  if (is.numeric(x$cons$iterations$setting)) {
    if (!(x$cons$iterations$setting >= 0)) {
      stop(strwrap("Component x$cons$iterations$setting must be >= 0",
                   prefix = " ", initial = ""))
    }
    if (x$cons$iterations$actual > x$cons$iterations$setting) {
      stop(strwrap("Cannot have more actual iterations than was set (cannot have
                   x$cons$iterations$actual > x$cons$iterations$setting",
                   prefix = " ", initial =""))
    }
    if ((x$cons$iterations$setting > x$cons$iterations$actual) &
        !is.numeric(x$cons$convergence$criterion)) {
      stop(strwrap("Can only have fewer actual iterations than the numeric
                   setting (i.e. x$cons$iterations$actual <
                   x$cons$iterations$setting) when a convergence criterion has
                   been set", prefix = " ", initial = ""))
    }
  } # end numeric iterations set

  # check the elements of $model, $res, $stdres, $sel, $type
  if (identical(x$cons$initial$estimator, "saturated") &
    (x$cons$iterations$actual >= 0)) {
    # when initial estimator is 'saturated' then model$m0 is a list with 2 elem
    if(!identical(length(x$model$m0), 2L) | !is.list(x$model$m0)) {
      stop(strwrap("Component x$model$m0 has to be a list with 2 components when
                   initial estimator is 'saturated'", prefix = " ",
                   initial = ""))
    }
    if (!identical(names(x$model$m0), c("split1", "split2"))) {
      stop(strwrap("Component x$model$m0 must have two named components:
                   split1 split2", prefix = " ", initial = ""))
    }
    if (!("ivreg" %in% class(x$model$m0$split1)) |
        !("ivreg" %in% class(x$model$m0$split2))) {
      stop(strwrap("Components x$model$m0$split1 and x$model$m0$split2 must be
                   of class 'ivreg'", prefix = " ", initial = ""))
    }
  } # check model$m0 when saturated
  if (!identical(x$cons$initial$estimator, "saturated") &
      (x$cons$iterations$actual >= 0)) {
    if (!("ivreg" %in% class(x$model$m0))) {
      stop(strwrap(paste("Element 1 of list $model must be of class
                         'ivreg'", sep = " "), prefix = " ", initial = ""))
    }
  } # check model$m0 when NOT saturated

  for (i in seq_len(length(iter.names) - 1)) {
    if (!("ivreg" %in% class(x$model[[i+1]]))) {
      stop(strwrap(paste("Element", i+1, "of list $model must be of class
                         'ivreg'", sep = " "), prefix = " ", initial = ""))
    }
  }
  n <- as.integer(NROW(x$cons$data))
  for (i in seq_along(iter.names)) {
    if (!is.numeric(x$res[[i]]) | !identical(length(x$res[[i]]), n)) {
      stop(strwrap(paste("Element", i, "of list $res must be a numeric vector
                         with length equal to the number of observations in the
                         dataframe", sep = " "), prefix = " ", initial = ""))
    }
  }
  for (i in seq_along(iter.names)) {
    if (!is.numeric(x$stdres[[i]]) | !identical(length(x$stdres[[i]]), n)) {
      stop(strwrap(paste("Element", i, "of list $stdres must be a numeric vector
                         with length equal to the number of observations in the
                         dataframe", sep = " "), prefix = " ", initial = ""))
    }
  }
  for (i in seq_along(iter.names)) {
    if (!is.logical(x$sel[[i]]) | !identical(length(x$sel[[i]]), n)) {
      stop(strwrap(paste("Element", i, "of list $sel must be a logical vector
                         with length equal to the number of observations in the
                         dataframe", sep = " "), prefix = " ", initial = ""))
    }
  }
  for (i in seq_along(iter.names)) {
    if (!is.numeric(x$type[[i]]) | !identical(length(x$type[[i]]), n)) {
      stop(strwrap(paste("Element", i, "of list $type must be a numeric vector
                         with length equal to the number of observations in the
                         dataframe", sep = " "), prefix = " ", initial = ""))
    }
  }
  for (i in seq_along(iter.names)) {
    if (!identical(setdiff(unique(x$type[[i]]), as.integer(c(-1, 0, 1))),
                  integer(0))) {
      stop(strwrap(paste("Element", i, "of list $type must be a numeric vector
                   that only contains the values -1, 0, or 1", sep = " "),
                   prefix = " ", initial = ""))
    }
  }

  x

}

##' Helper of robust2sls class
##'
##' \code{robust2sls} allows the user to create an object of \link{class}
##' "\code{robust2sls}" by specifying the different components of the list. The
##' validator function \code{validate_robust2sls} is called at the end to ensure
##' that the resulting object is a valid object of \link{class}
##' \code{"robust2sls"}.
##'
#
#robust2sls <- function(data, coefficients, gamma, iterations, converged) {
#
#  x <- list(data = data, coefficients = coefficients, gamma = gamma,
#            iterations = iterations, converged = converged)
#  x <- new_robust2sls(x)
#  validate_robust2sls(x)
#
#}


#' Printing summary output
#'
#' Print method for objects of \link{class} \code{"robust2sls"}. Prints a
#' high-level summary of the settings and results of the outlier-detection
#' algorithm.
#'
#' @param x An object of \link{class} \code{"robust2sls"}.
#' @param verbose A logical value, \code{TRUE} or \code{FALSE}, determining
#' whether detailed (\code{TRUE}) or shortened (\code{FALSE}) should be printed.
#' @param ... Further arguments passed to or from other methods, see
#' \link[base]{print}.
#'
#' @export

print.robust2sls <- function(x, verbose = FALSE, ...) {

  if (verbose == FALSE) {

    cat("Outlier-Robust 2SLS Model", "\n")
    cat("Initial estimator: ", x$cons$initial$estimator, "\n")
    cat("Reference distribution: ", x$cons$reference, "\n")
    cat("Two-stage Least-Squares Model: ")
    print(x$cons$formula)
    cat("Iterations: ", x$cons$iterations$actual, "\n")
    cat("Final selection: ", "Outliers found: ",
        outliers(x, x$cons$iterations$actual), "    Outliers proportion: ",
        round(outliers_prop(x, x$cons$iterations$actual),4), "\n")

  } else { # end if verbose == FALSE

    print.default(x = x)

  }

}

#' Plotting of standardised residuals and outliers
#'
#' Plot method for objects of \link{class} \code{"robust2sls"}. Plots the
#' standardised residuals of non-missing observations for a given iteration of
#' the outlier-detection algorithm and distinguishes whether an observation is
#' classified as an outlier by colour.
#'
#' @param x An object of \link{class} \code{"robust2sls"}.
#' @param iteration Either \code{NULL} (default) or an integer specifying the
#' iteration that should be plotted. The default uses the final model.
#' @param ... Arguments to be passed to methods, see \link[base]{plot}.
#'
#' @return \code{plot.robust2sls} returns a graph of \link{class}
#' \link[ggplot2]{ggplot}.
#'
#' @export

plot.robust2sls <- function(x, iteration = NULL, ...) {

  # create accessor
  if (is.null(iteration)) { # default: take final model
    iteration <- x$cons$iterations$actual
  }
  acc <- paste("m", iteration, sep = "")
  # create vector of non-missing observations
  non_missing <- nonmissing(data = x$cons$data, formula = x$cons$formula)
  # collect the data we want to plot in a dataframe
  gr <- as.data.frame(cbind(x$res[[acc]], x$stdres[[acc]], x$sel[[acc]],
                            x$type[[acc]],non_missing))
  colnames(gr) <- c("res","stdres","sel","type","non_missing")
  gr <- gr[non_missing, ] # delete missing values
  gr$index <- 1:NROW(gr)
  critical <- x$cons$cutoff
  # create title
  title_name <- paste("Standardised Residuals and Selection of Iteration",
                      iteration, collapse = " ")
  if (iteration == x$cons$iterations$actual) { # add (Final) if final selection
    title_name <- paste(title_name, "(Final)", collapse = " ")
  }

  plot <- ggplot2::ggplot(data = gr) +
    ggplot2::geom_col(ggplot2::aes(x = index, y = stdres, fill = factor(sel)),
                      width = 0.05, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c("red","blue"), name="non-outlying",
                      guide = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::geom_segment(aes(x=1, y=critical, xend=NROW(gr), yend=critical,
                              color=""), size=1) +
    ggplot2::geom_segment(aes(x=1, y=-critical, xend=NROW(gr), yend=-critical,
                              color=""), size=1) +
    ggplot2::labs(x="index",y="standardised residuals",title=title_name) +
    ggplot2::scale_color_manual(name="critical values",values=c("darkgrey")) +
    ggplot2::theme(legend.position="top")

  return(plot)

}





