library(ggplot2) # to draw graphs
library(assertthat) # to test the inputs of the function
library(AER) # for ivreg command
library(foreign) # just to write Stata .dta file
library(datasets) # just to have more datasets to test
library(mvtnorm) # multivariate normal (and t) --> seems to have dropped this function in more recent versions?
library(matrixcalc) # matrix functions such as checking positive definiteness etc.
library(MASS) # multivariate normal sampling
library(pracma) # for roots of matrices
library(expm) # for matrix roots and exponentiation
library(dplyr) #
library(tidyr) #
library(doFuture)
library(doRNG)
library(tidyverse)

# copied all necessary functions from the "parallelizing.R" and "robust2SLS_v1_test_gauge_asymp_v2.R"

robust2SLS <-
  function(data, regression, sign_level, iterations = 1, initial_est = "robustified", convergence_criterion = NULL, shuffle = FALSE, split = 0.5, graph = TRUE) {

    #check that have received expected input types
    assert_that(is.data.frame(data), msg = "data input is not a dataframe")
    assert_that(class(regression) == "formula", msg = "regression input is not a formula")
    assert_that(is.numeric(sign_level), (sign_level < 1), (0 < sign_level), msg = "sign_level input must be numeric between 0 and 1")
    assert_that((is.numeric(iterations) | is.character(iterations)), msg = "iterations input must be a numeric integer > 0 or the character string \"convergence\"")
    if (is.numeric(iterations)) {
      assert_that(iterations >= 0, msg = "iterations input must be > 0")
      assert_that(iterations %% 1 == 0, msg = "iterations input must be a numeric integer")
      assert_that((is.null(convergence_criterion) | (is.numeric(convergence_criterion))), msg = "convergence_criterion input must be NULL or numeric > 0")
      if (is.numeric(convergence_criterion)) {
        assert_that(convergence_criterion > 0, msg = "convergence_criterion input must be NULL or numeric > 0")
      }
    } else {
      assert_that(iterations == "convergence", msg = "iterations input as character string only allows \"convergence\"")
      assert_that(!is.null(convergence_criterion), msg = "when iterations input is \"convergence\" then must specify a numerical convergence_criterion")
      assert_that(is.numeric(convergence_criterion), convergence_criterion > 0, msg = "when iterations input is \"convergence\" then convergence_criterion input must be numeric > 0")
    }
    assert_that((is.character(initial_est) | class(initial_est) == "ivreg"), msg = "initial_est input must be an allowed character string or an \"ivreg\" object")
    if (is.character(initial_est)) {
      assert_that((initial_est == "robustified" | initial_est == "saturated"), msg = "initial_est input as character string only allows \"robustified\" or \"saturated\"")
    } else {
      assert_that(class(initial_est) == "ivreg", msg = "initial_est input with user-specified initial estimator must be given as \"ivreg\" object")
    }
    assert_that(is.logical(shuffle), msg = "shuffle input must be a logical value")
    assert_that(is.numeric(split), msg = "split input must be numeric stricly between 0 and 1")
    assert_that(split > 0, split < 1, msg = "split input must be numeric larger than 0 and smaller than 1")
    validate_that(split <= 0.8, split >= 0.2, msg = "WARNING: very unequal split") #does not seem to work right now
    assert_that(is.logical(graph), msg = "graph input must be a logical value")

    #calculating variables that will be needed later
    critical <- qnorm(p = 1-(sign_level/2), mean = 0, sd = 1) #critical value of standard normal distribution for cut-off depending on sign level
    bias_corr <- 1/(((1-sign_level)-2*critical*dnorm(critical,mean=0,sd=1))/(1-sign_level)) #bias correction for normally distributed error

    all_vars <- all.vars(regression) #extract all variables appearing in the regression formula
    y_var <- all_vars[1] # dependent variable is first element in regression formula; cannot use transformed dependent variables

    model_it <- vector(mode="list", length=1) #initialise list storing the models, will grow gradually
    res_it <- vector(mode="list", length=1) #initialise list storing the residuals, will grow gradually
    stdres_it <- vector(mode="list", length=1) #initialise list storing the standardised residuals, will grow gradually
    sel_it <- vector(mode="list", length=1) #initialise list storing the selection vector, will grow gradually
    type_it <- vector(mode="list", length=1) #initialise list storing the type vector, will grow gradually

    model_next <- vector(mode="list", length=1) #initialise list storing the next iteration of the model
    res_next <- vector(mode="list", length=1) #initialise list storing the next iteration of the residuals
    stdres_next <- vector(mode="list", length=1) #initialise list storing the next iteration of the standardised residuals
    sel_next <- vector(mode="list", length=1) #initialise list storing the next iteration of the selection vector
    type_next <- vector(mode="list", length=1) #initialise list storing the next iteration of the type vector

    #create a vector storing which observations could in principle be used for estimation (none of y, x, z missing)
    non_missing <- !logical(length = NROW(data)) # initialise as if had no missing values

    for (i in 1:length(all_vars)) {
      non_missing <- (non_missing & !is.na(data[, all_vars[i]])) # only keep as TRUE if wasn't missing in previous variable and not in current variable
    }

    data$nonmissing <- non_missing # add logical vector as new variable/column to dataset

    ###saturated 2SLS
    if (initial_est == "saturated") { # only execute when user specified "saturated" as initial estimator

      #preparing sample split
      full <- ivreg(formula = regression, data = data, model = TRUE)

      #extract the number of observations in the full sample 2SLS regression (is equal to number of nonmissing y, x, z observations)
      num_obs <- full$nobs

      #create an index variable so that we can keep track of the observations even after shuffling
      data$index <- 1:NROW(data)

      #extract the index variable as a vector and store it separately
      ind <- data$index

      if (shuffle == TRUE) { # if user wants to have random allocation to splits

        #we want to sample indices randomly and use the randomly ordered indices to construct the splits of the sample
        #however, when sampling from the indices we need to make sure not to sample from indices that correspond to missing observations
        #for this purpose, create a vector wght based on missing or nonmissing status -> give probability weight zero to missing observations
        wght <- as.numeric(data$nonmissing) # observations with missing get zero, observations with nonzero get 1

        #now sample all indices corresponding to nonmissing observations, is in random order
        set.seed(42)
        ind_nonmissing <- sample(x = ind, size = num_obs, replace = FALSE, prob = wght)

      } else { # if user wants to keep order of dataset, then only need to exclude the indices of observations with missing values before splitting

        ind_nonmissing <- ind[non_missing]

      } # end if-else shuffle

      #splitting the sample
      #special case: split-half (split = 0.5); if odd number of observations then first split is smaller by one than the second split
      splitpoint <- floor(num_obs * split) # rounding to nearest integer smaller than num_obs * split

      split1 <- ind_nonmissing[1 : splitpoint] # selection vector for first split
      split2 <- ind_nonmissing[(splitpoint+1) : num_obs] # selection vector for second split

      #create two new variables in the dataset that indicate the subset to be used in each split
      #TRUE/1 if used in that split, FALSE/0 if not

      data$split1 <- FALSE
      data$split1[split1] <- TRUE
      data$split2 <- FALSE
      data$split2[split2] <- TRUE

      #calculating separate models for each split
      model_split1 <- ivreg(formula = regression, data = data, subset = split1, model = TRUE, y = TRUE) # run 2SLS on first split
      model_split2 <- ivreg(formula = regression, data = data, subset = split2, model = TRUE, y = TRUE) # run 2SLS on second split

      sigma_split1 <- model_split1$sigma * sqrt(model_split1$df.residual / model_split1$nobs) # reverse the df correction
      sigma_split2 <- model_split2$sigma * sqrt(model_split2$df.residual / model_split2$nobs) # reverse the df correction

      #calculating standardised residuals for observations in each split based on parameter and sigma estimates of the other split
      #using ivreg components $residuals or $fitted.values only returns these values for the observations involved in the estimation of ivreg
      #this means that model_split1$residuals only gives a vector of residuals for observations in the first split
      #therefore need to construct residuals based on the coefficients of the other split manually using generic function predict(estimated model, data)
      res_split1 <- data[split1, y_var] - predict(model_split2, data[split1,]) # use estimates of model_split2 on data of model_split1
      res_split2 <- data[split2, y_var] - predict(model_split1, data[split2,]) # use estimates of model_split1 on data of model_split2

      stdres_split1 <- res_split1/sigma_split2 # standardised residuals of model_split1 based on sigma of model_split2
      stdres_split2 <- res_split2/sigma_split1 # standardised residuals of model_split2 based on sigma of model_split1

      #need to attach the residuals to the original dataframe again correctly
      data$res0 <- NA # create new variable res0 initilised as missing for all observations (so will keep NA for observations where y, x, or z missing)
      data$res0[split1] <- res_split1 # replace observations with index in first split with their corresponding residuals
      data$res0[split2] <- res_split2 # replace observations with index in second split with their corresponding residuals

      #need to attach the standardised residuals to the original dataframe again correctly
      data$stdres0 <- NA # create new variable res0 initilised as missing for all observations (so will keep NA for observations where y, x, or z missing)
      data$stdres0[split1] <- stdres_split1 # replace observations with index in first split with their corresponding standardised residuals
      data$stdres0[split2] <- stdres_split2 # replace observations with index in second split with their corresponding standardised residuals

      #creating the selection vector (whether observations is outlier or not) based on other split's estimates
      data$sel0 <- FALSE # create new variable sel0 initialised as FALSE for all observations (will stay FALSE for observations where y, x, or z missing)
      data$sel0[split1] <- (abs(data$stdres0[split1]) <= critical) # returns logical vector for first split, TRUE if non-outlying, FALSE otherwise
      data$sel0[split2] <- (abs(data$stdres0[split2]) <= critical) # returns logical vector for second split, TRUE if non-outlying, FALSE otherwise

      selection0 <- data$sel0 # logical vector for whole dataset (original order)

      #transfer the estimates, residuals and selection to the output lists; they are all now in the order of the original dataset
      #since we have two initial estimators, the first model element in saturated 2SLS is itself a list with two elements
      model_it[[1]] <- list(model_split1, model_split2) # append model results from both splits
      res_it[[1]] <- data$res0 # append residuals
      stdres_it[[1]] <- data$stdres0 # append standardised residuals
      sel_it[[1]] <- data$sel0 # append selection vector

      # have three diferrent types of observations
      # classify them by transforming the logical variables into numeric vectors (TRUE = 1, FALSE = 0)
      # then calculate (using numeric versions of the vectors): nonmissing + sel0 - 1
      # 1) observations where any of x, y, z is missing --> nonmissing = FALSE, sel0 = FALSE --> formula evaluates to -1
      # 2) observations where none missing and not an outlier --> nonmissing = TRUE, sel0 = TRUE --> formula evaluates to 1
      # 3) observations where none missing but is an outlier --> nonmissing = TRUE, sel0 = FALSE --> formula evaluates to 0

      type_it[[1]] <- as.numeric(data$nonmissing) + as.numeric(data$sel0) - 1 # append type vector

      # delete the newly created variables from the dataset except for nonmissing variable
      to_drop <- c("index","res0","sel0","stdres0","split1","split2") # collection of variables to drop
      data <- data[, !names(data) %in% to_drop] # only keep the variables whose names are not in the collection of variables to drop

    } # saturated end if



    ###robustified 2SLS
    if (initial_est == "robustified") { # only execute when user specified "robustified" as initial estimator

      #run 2SLS on the full sample
      full <- ivreg(formula = regression, data = data, model = TRUE)

      res0 <- data[, y_var] - predict(full, data) # calculate residuals for all observations; will be NA if either of y or x missing
      res0[!data$nonmissing] <- NA # replace residual with NA for observations where z missing; will never be used in estimation anyways
      sigma <- full$sigma * sqrt(full$df.residual / full$nobs) # reverse df correction
      stdres0 <- res0 / sigma # calculate standardised residuals, also missing if any of x, y, or z missing
      sel0 <- (abs(stdres0) <= critical) # create new selection vector, if missing before then still missing now
      sel0[is.na(sel0)] <- FALSE # manually exclude all observations that had NA, replace with FALSE to exclude from estimation

      # have three diferrent types of observations
      # classify them by transforming the logical vectors into numeric vectors (TRUE = 1, FALSE = 0)
      # then calculate (using numeric versions of the vectors): non_missing + sel - 1
      # 1) observations where any of x, y, z is missing --> non_missing = FALSE, sel = FALSE --> formula evaluates to -1
      # 2) observations where none missing and not an outlier --> non_missing = TRUE, sel = TRUE --> formula evaluates to 1
      # 3) observations where none missing but is an outlier --> non_missing = TRUE, sel = FALSE --> formular evaluates to 0

      type0 <- as.numeric(non_missing) + as.numeric(sel0) - 1 # this vector will be used as output for user, contains most info

      #transfer the estimates, residuals and selection to the output lists
      model_it[[1]] <- full # append model results
      res_it[[1]] <- res0 # append residuals
      stdres_it[[1]] <- stdres0 # append standardised residuals
      sel_it[[1]] <- sel0 # append selection vector
      type_it[[1]] <- type0 # append type vector

    } # robustified end if



    ###user-specified initial estimators in form of ivreg object
    if (class(initial_est) == "ivreg") { # only execute when user specified their own initial estimator in form of an ivreg object

      #calculate sigma estimate
      sigma <- initial_est$sigma * sqrt(initial_est$df.residual / initial_est$nobs) # reverse the df correction

      #calculating standardised residuals based on the initial estimators for the whole dataset provided
      res0 <- data[, y_var] - predict(initial_est, data) # use initial estimator on whole dataset; will be NA if either of y or x missing
      res0[!data$nonmissing] <- NA # replace residual with NA for observations where z missing; will never be used in estimation anyways
      stdres0 <- res0 / sigma # calculate standardised residuals, also missing if any of x, y, or z missing

      #creating the selection vector based on initial estimates
      #we also set those observations in the selection to 0/FALSE that have at least one of the y, x or z variables missing; cannot be used in future 2SLS estimation anyways
      selection0 <- (abs(stdres0) <= critical) & !is.na(stdres0) # to be non-outlying must be weakly smaller than critical value and not missing

      # have three diferrent types of observations
      # classify them by transforming the logical vectors into numeric vectors (TRUE = 1, FALSE = 0)
      # then calculate (using numeric versions of the vectors): non_missing + sel - 1
      # 1) observations where any of x, y, z is missing --> non_missing = FALSE, sel = FALSE --> formula evaluates to -1
      # 2) observations where none missing and not an outlier --> non_missing = TRUE, sel = TRUE --> formula evaluates to 1
      # 3) observations where none missing but is an outlier --> non_missing = TRUE, sel = FALSE --> formular evaluates to 0

      type0 <- as.numeric(non_missing) + as.numeric(sel0) - 1 # this vector will be used as output for user, contains most info

      #transfer the estimates, residuals and selection to the output lists
      model_it[[1]] <- initial_est # append model result, given by user
      res_it[[1]] <- res0 # append residuals
      stdres_it[[1]] <- stdres0 # append standardised residuals
      sel_it[[1]] <- selection0 # append selection vector, now contains observations where any of x, y, z might be missing (set to FALSE)
      type_it[[1]] <- type0 # append type vector



    } # user-specified initial estimators end if

    #the initial selection sample exists now and we can start the iteration process

    ###iterated 2SLS

    if (iterations == "convergence") { # user specified that should iterate until convergence, must specify stopping criterion "convergence_criterion"

      # initialise difference larger than convergence_criterion so that while loop will start
      difference <- convergence_criterion + 1

      # initialise counter
      counter <- 1

      while (difference > convergence_criterion) {

        # access the last element of sel_it, store it as a variable in the dataframe (will be created in first run, replaced thereafter)
        data$selection <- sel_it[[length(sel_it)]] # take the last element of the selection list

        new <- ivreg(formula = regression, data = data, subset = selection, model = TRUE, y = TRUE)
        res <- data[, y_var] - predict(new, data) # calculate residuals for all observations; will be NA if either of y or x missing
        res[!data$nonmissing] <- NA # replace residual with NA for observations where z missing; will never be used in estimation anyways
        sigma <- new$sigma * sqrt(new$df.residual / new$nobs) * sqrt(bias_corr) # based only on observations used in estimation
        stdres <- res / sigma # create standardised residuals
        sel <- (abs(stdres) <= critical) # create new selection vector, if missing before then still missing now
        sel[is.na(sel)] <- FALSE # manually exclude all observations that had NA, replace with FALSE to exclude from estimation

        # have three diferrent types of observations
        # classify them by transforming the logical vectors into numeric vectors (TRUE = 1, FALSE = 0)
        # then calculate (using numeric versions of the vectors): non_missing + sel - 1
        # 1) observations where any of x, y, z is missing --> non_missing = FALSE, sel = FALSE --> formula evaluates to -1
        # 2) observations where none missing and not an outlier --> non_missing = TRUE, sel = TRUE --> formula evaluates to 1
        # 3) observations where none missing but is an outlier --> non_missing = TRUE, sel = FALSE --> formular evaluates to 0

        type <- as.numeric(non_missing) + as.numeric(sel) - 1 # this vector will be used as output for user, contains most info

        # store the results in the respective lists

        model_next[[1]] <- new # overwrites element of that list if this is not the first iteration
        res_next[[1]] <- res # overwrites element of that list if this is not the first iteration
        stdres_next[[1]] <- stdres # overwrites element of that list if this is not the first iteration
        sel_next[[1]] <- sel # overwrites element of that list if this is not the first iteration
        type_next[[1]] <- type # overwrites element of that list if this is not the first iteration

        model_it <- c(model_it, model_next) # add a new element to the output list
        res_it <- c(res_it, res_next) # add a new element to the output list
        stdres_it <- c(stdres_it, stdres_next) # add a new element to the output list
        sel_it <- c(sel_it, sel_next) # add a new element to the output list
        type_it <- c(type_it, type_next) # add a new element to the output list

        # delete the selection variable, will be re-created in the next iteration
        to_drop <- c("selection") # variable to drop
        data <- data[, !names(data) %in% to_drop] # only delete "selection"

        # calculate the (new) difference between estimates m+1 and m

        if (counter == 1 & initial_est == "saturated") { # this means we are in the first iteration after the initial saturated estimator

          # we now have two initial estimates, one for each split
          # define the difference as the larger difference between the updated estimates and both of the initial split estimates

          diff1 <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]][[1]]$coefficients)^2) # difference with first split
          diff2 <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]][[2]]$coefficients)^2) # difference with second split

          difference <- max(diff1, diff2) # take the maximum of the two differences

        } else {

          difference <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]]$coefficients)^2) # L2 norm between the beta estimates

        } # end if calculating difference

        counter <- counter + 1 # update counter

      } # end while
    } # end if "convergence"

    if (is.numeric(iterations) & iterations!=0) { # user specified a specific number of iterations

      # initialise counter
      counter <- 1

      for (i in 2:(iterations+1)) { # go through the user-specified number of steps (same body as while loop)

        data$selection <- sel_it[[length(sel_it)]] # take the last element of the selection list

        new <- ivreg(formula = regression, data = data, subset = selection, model = TRUE, y = TRUE)
        res <- data[, y_var] - predict(new, data) # calculate residuals for all observations; will be NA if either of y or x missing
        res[!data$nonmissing] <- NA # replace residual with NA for observations where z missing; will never be used in estimation anyways
        sigma <- new$sigma * sqrt(new$df.residual / new$nobs) * sqrt(bias_corr) # based only on observations used in estimation
        stdres <- res / sigma # create standardised residuals
        sel <- (abs(stdres) <= critical) # create new selection vector, if missing before then still missing now
        sel[is.na(sel)] <- FALSE # manually exclude all observations that had NA, replace with FALSE to exclude from estimation

        # have three diferrent types of observations
        # classify them by transforming the logical vectors into numeric vectors (TRUE = 1, FALSE = 0)
        # then calculate (using numeric versions of the vectors): non_missing + sel - 1
        # 1) observations where any of x, y, z is missing --> non_missing = FALSE, sel = FALSE --> formula evaluates to -1
        # 2) observations where none missing and not an outlier --> non_missing = TRUE, sel = TRUE --> formula evaluates to 1
        # 3) observations where none missing but is an outlier --> non_missing = TRUE, sel = FALSE --> formular evaluates to 0

        type <- as.numeric(non_missing) + as.numeric(sel) - 1 # this vector will be used as output for user, contains most info

        # store the results in the respective lists

        model_next[[1]] <- new # overwrites element of that list if this is not the first iteration
        res_next[[1]] <- res # overwrites element of that list if this is not the first iteration
        stdres_next[[1]] <- stdres # overwrites element of that list if this is not the first iteration
        sel_next[[1]] <- sel # overwrites element of that list if this is not the first iteration
        type_next[[1]] <- type # overwrites element of that list if this is not the first iteration

        model_it <- c(model_it, model_next) # add a new element to the output list
        res_it <- c(res_it, res_next) # add a new element to the output list
        stdres_it <- c(stdres_it, stdres_next) # add a new element to the output list
        sel_it <- c(sel_it, sel_next) # add a new element to the output list
        type_it <- c(type_it, type_next) # add a new element to the output list

        # delete the selection variable, will be re-created in the next iteration
        to_drop <- c("selection") # variable to drop
        data <- data[, !names(data) %in% to_drop] # only delete "selection"

        # despite choosing a fixed number of iterations, also a convergence criterion can be specified to end before number of iterations was reached
        # calculate the (new) difference between estimates m+1 and m

        if (counter == 1 & initial_est == "saturated") { # this means we are in the first iteration after the initial saturated estimator

          # we now have two initial estimates, one for each split
          # define the difference as the larger difference between the updated estimates and both of the initial split estimates

          diff1 <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]][[1]]$coefficients)^2) # difference with first split
          diff2 <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]][[2]]$coefficients)^2) # difference with second split

          difference <- max(diff1, diff2) # take the maximum of the two differences

        } else {

          difference <- sum((model_it[[length(model_it)]]$coefficients - model_it[[length(model_it)-1]]$coefficients)^2) # L2 norm between the beta estimates

        } # end if calculating difference

        if (!is.null(convergence_criterion)) { # convergence_criterion was specified
          if (difference <= convergence_criterion) { # convergence_criterion was reached

            print("convergence according to convergence_criterion was reached; exit iterations")
            break

          }
        } # end if break

        counter <- counter + 1 # update counter

      } # end for iterations

    } # end if number of iterations specified

    if (graph == TRUE) { # create dataframe for graph

      gr <- as.data.frame(cbind(res_it[[length(res_it)]],stdres_it[[length(stdres_it)]],sel_it[[length(sel_it)]],type_it[[length(type_it)]],non_missing))
      colnames(gr) <- c("res","stdres","sel","type","non_missing")
      gr <- gr[non_missing, ] # delete missing values
      gr$index <- 1:NROW(gr)

      plot <- ggplot(data = gr) +
        geom_col(aes(x = index, y = stdres, fill = factor(sel)), width = 0.05, na.rm = TRUE) +
        scale_fill_manual(values = c("red","blue"), name="non-outlying", guide = guide_legend(reverse=TRUE)) +
        geom_segment(aes(x=1, y=critical, xend=NROW(gr), yend=critical, color=""), size=1) +
        geom_segment(aes(x=1, y=-critical, xend=NROW(gr), yend=-critical, color=""), size=1) +
        labs(x="index",y="standardised residuals",title="Standardised Residuals and Final Selection") +
        scale_color_manual(name="critical values",values=c("darkgrey")) +
        theme(legend.position="top")

    } # end if graph

    # prepare the final dataframe to be returned to the user
    # we want to add variables specifying nonmissing observations, the final residuals and standardised residuals, the selection and type vector
    data$res <- res_it[[length(res_it)]]
    data$stdres <- stdres_it[[length(stdres_it)]]
    data$nonmissing <- non_missing
    data$sel <- sel_it[[length(sel_it)]]
    data$type <- type_it[[length(type_it)]]


    return(list(model_it,res_it,stdres_it,sel_it,type_it,data,plot)) #return the final lists in another list

  } # end function robust2SLS

param_gen_2SLS <- function(dx1, dx2, dz2, intercept = TRUE, mean_z = NULL, var_z = NULL, sigma, Sigma_half = NULL, Omega = NULL, beta = NULL, Pi = NULL, seed = 42) {
  # dx1 = dimension of exogenous regressors excluding constant/intercept; also equals dz1, because also used as instruments
  # dx2 = dimension of endogenous regressors
  # dz2 = dimension of "outside"/excluded instruments
  # intercept = TRUE means the first element of dx1 (and also beta) is modelled as a deterministic intercept, not a random variable to be drawn
  # mean_z = vector of length dz = dz1 + dz2 specifying the mean of the exogenous variables
  # var_z = variance-covariance matrix of exogenous variables
  # sigma = standard deviation of structural error
  # Sigma = variance-covariance matrix of dimension dx2 of first stage errors that are random
  # Omega = vector of length dx1 specifying correlation between (scaled) random first stage error and structural error
  # beta = vector of length dx specifying the parameters of the structural equation
  # Pi = matrix of dimensions dz by dx specifying the first stage parameter matrix
  # seed = number to set random seed)

  set.seed(seed)

  dz1 <- dx1
  dx <- dx1 + dx2
  dz <- dz1 + dz2

  if (is.null(Sigma_half)) { # if Sigma not specified, create random Sigma matrix that is positive definite
    Sigma_half <- matrix(runif(dx2^2)*2-1, ncol=dx2)
    Sigma <- Sigma_half %*% t(Sigma_half) # var-cov matrix of first stage errors
    Sigma_half <- expm(1/2 * logm(Sigma))
    Sigma_half <- round(Sigma_half, digits=2)
    Sigma <- Sigma_half %*% Sigma_half
  }

  Sigma <- Sigma_half %*% Sigma_half

  if (is.null(Omega)) { # if Omega is not specified, create random Omega vector
    Omega <- matrix(runif(dx2)*2-1, ncol = 1) # covariance of standardised first and second stage errors
    Omega <- round(Omega, digits=2) # needed to avoid numerical imprecision later
  }

  # creating matrix for correlation and mean of first&second-stage errors
  A_scaled <- cbind(1, t(Omega))
  A_scaled <- rbind(A_scaled, cbind(Omega, diag(dx2))) # variance-covariance matrix for the standardised first & second stage errors

  scale_matrix <- cbind(sigma, matrix(0,1,dx2))
  scale_matrix <- rbind(scale_matrix, cbind(matrix(0,dx2,1), Sigma_half))
  A_unscaled <- scale_matrix %*% A_scaled %*% t(scale_matrix)

  mean_ru <- matrix(0, dx2+1, 1)

  # creating matrix for correlation and mean of z

  if (is.null(mean_z)) {

    if (intercept == TRUE) { # if the model includes an intercept, want to draw one exogenous variable less
      mean_z <- matrix(runif(dz-1),dz-1,1)
      mean_z <- round(mean_z, digits=2)
    } else { # if no intercept, then all dz variables are random, so draw all of them
      mean_z <- matrix(runif(dz),dz,1)
      mean_z <- round(mean_z, digits=2)
    }
  } # end if mean_z to be created

  if (is.null(var_z)) {

    if (intercept == TRUE) { # if the model includes an intercept, want to draw one exogenous variable less
      var_z_half <- matrix(runif((dz-1)^2)*2-1, ncol=(dz-1))
      var_z <- var_z_half %*% t(var_z_half) # var-cov matrix of instruments

      var_z_half <- expm(1/2 * logm(var_z))
      var_z_half <- round(var_z_half, digits=2)
      var_z <- var_z_half %*% var_z_half
    } else {
      var_z_half <- matrix(runif((dz)^2)*2-1, ncol=(dz))
      var_z <- var_z_half %*% t(var_z_half) # var-cov matrix of instruments

      var_z_half <- expm(1/2 * logm(var_z))
      var_z_half <- round(var_z_half, digits=2)
      var_z <- var_z_half %*% var_z_half
    }
  } # end if var_z to be created

  Ezz <- var_z + mean_z %*% t(mean_z)

  # creating first and second stage parameter vectors

  if (is.null(beta)) {
    beta <- matrix(runif(dx),dx,1)
    beta <- round(beta, digits=2) # needed to avoid numerical imprecision later
  }

  if (is.null(Pi)) {
    Pi <- cbind(diag(dx1), matrix(0,dx1,dz2))
    Pi1 <- t(matrix(runif(dx2*dz1)*2-1, ncol=dz1))

    pd_half <- matrix(runif(dx2*dx2)*2-1, ncol=dx2)
    pd <- pd_half %*% t(pd_half)
    pd_half <- expm(1/2 * logm(pd))
    pd_half <- round(pd_half, digits=2)
    pd <- pd_half %*% pd_half
    random <- matrix(runif(dx2*(dz2-dx2))*2-1, nrow=dx2, ncol=(dz2-dx2)) # if dz2 = dx2 then creates an empty 0x0 matrix
    Pi2 <- t(cbind(pd, random))
    Pi <- rbind(Pi, cbind(t(Pi1), t(Pi2)))
    Pi <- t(Pi) # to make consistent with notation in theory
    Pi <- round(Pi, digits=2) # needed to avoid numerical imprecision later
  }

  #creating vectors to represent non-correlation with z and the errors

  if (intercept == TRUE) {
    zero_zu <- matrix(0,dz-1,1)
    zero_zr <- matrix(0,dz-1,dx2)
  } else {
    zero_zu <- matrix(0,dz,1)
    zero_zr <- matrix(0,dz,dx2)
  }

  #combine all matrices for the then actual draw of z, u, r

  structural_cov <- cbind(A_unscaled, rbind(t(zero_zu),t(zero_zr)))
  structural_cov <- rbind(structural_cov, cbind(zero_zu, zero_zr, var_z))

  structural_mean <- rbind(mean_ru,mean_z)

  names_u <- "u"

  names_x1 <- c()
  names_x2 <- c()
  names_x <- c()
  names_z2 <- c()
  names_z <- c()
  names_r <- c()

  for (i in 1:dx1) { #"x1" is variable name of the first exogenous regressors (may be constant if intercept = TRUE, random if = FALSE)
    new <- paste("x", i, sep="")
    names_x1 <- cbind(names_x1, new)
  }

  for (i in (dx1+1):dx) {
    new <- paste("x", i, sep="")
    names_x2 <- cbind(names_x2, new)
  }

  names_x <- cbind(names_x1, names_x2)

  for (i in (dx1+1):dz) {
    new <- paste("z", i, sep="")
    names_z2 <- cbind(names_z2, new)
  }

  names_z <- cbind(names_x1, names_z2)

  for (i in 1:dx) {
    new <- paste("r", i, sep="")
    names_r <- cbind(names_r, new)
  }

  names_x1 <- as.character(names_x1)
  names_x2 <- as.character(names_x2)
  names_x <- as.character(names_x)
  names_z2 <- as.character(names_z2)
  names_z <- as.character(names_z)
  names_r <- as.character(names_r)

  #create formula for 2SLS in ivreg()
  x_fmla <- paste(names_x, collapse=" + ")
  z_fmla <- paste(names_z, collapse=" + ")
  fmla <- paste(c("y ~", x_fmla), collapse=" ")
  fmla <- paste(c(fmla, "|", z_fmla), collapse=" ")
  fmla <- as.formula(fmla)

  #return parameters

  return(list(structural_mean, structural_cov, Pi, beta, names_x1, names_x2, names_x, names_z2, names_z, names_u, names_r, fmla, Omega, Sigma_half, var_z, intercept, Ezz))

}

data_gen_2SLS <- function(structural_mean, structural_cov, Pi, beta, names_x1, names_x2, names_x, names_z2, names_z, names_u, names_r, intercept = TRUE, sample.size = 1000) {

  # structural_mean = mean vector for random draws
  # structural_cov = var-cov matrix for random draws
  # Pi = matrix of dimensions dz by dx specifying the first stage parameter matrix
  # beta = vector of length dx specifying the parameters of the structural equation
  # names_xx = character vector to name columns in dataframe
  # intercept = TRUE means the first element of dx1 (and also beta) is modelled as a deterministic intercept, not a random variable to be drawn
  # sample.size = number of observations to be drawn from multivariate normal

  dx1 <- length(names_x1)
  dx2 <- length(names_x2)
  dx <- length(names_x)
  dz1 <- dx1
  dz2 <- length(names_z2)
  dz <- length(names_z)

  zru <- mvrnorm(n = sample.size, structural_mean, structural_cov)

  #add intercept column if intercept = TRUE
  if (intercept == TRUE) {
    constant <- matrix(1,nrow=sample.size,ncol=1)
    zru <- cbind(zru[,1:(dx2+1)], constant, zru[,(dx2+2):ncol(zru)])
  }


  #extract the different parts of the matrices
  u <- as.matrix(zru[, 1])
  colnames(u) <- names_u

  R2 <- zru[, (2:(dx2+1))]
  R <- cbind(matrix(0,sample.size,dx1),R2)
  colnames(R) <- names_r
  Z <- zru[, (dx2+2):ncol(zru)]
  colnames(Z) <- names_z

  #create the matrix of regressors
  X <- Z %*% Pi + R
  colnames(X) <- names_x

  #create the vector of y
  y <- X %*% beta + u
  colnames(y) <- "y"

  #bundle all data
  data <- cbind(y, X, u, Z, R)
  data <- as.data.frame(data)

  #return final dataset
  return(list(data, beta, Pi))

}

gamma <- 0.05 # significance level for selection
c <- qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE) # cut-off value corresponding to gamma
phi <- 1 - gamma # probability of non-outlying
f <- dnorm(c, mean=0, sd=1) # f(c)
tau_c_2 <- phi - 2 * c * f # variance of standardised error truncated to -c to c
tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f # fourth moment of standardised error truncated to -c to c
tau_2 <- 1 # variance of standardised error
tau_4 <- 3 # fourth moment of standardised error
varsigma_c_2 <- tau_c_2 / phi

# Parameter generation
M <- 100
dx1 <- 3
dx2 <- 2
dx <- dx1 + dx2
dz1 <- dx1
dz2 <- 3
dz <- dz1 + dz2
sigma <- 2
n <- 100000
seed <- 42

parameters <- param_gen_2SLS(dx1=dx1, dx2=dx2, dz2=dz2, sigma=sigma, seed=seed, intercept=TRUE)

#extract parameters & update those that were simulated using only a subset due to notation issue
Omega <- parameters[[13]]
Omega <- rbind(matrix(0,dx1,1), Omega)
zeta_c_minus <- 2 * Omega * c

Sigma_half <- parameters[[14]]
Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

Pi <- parameters[[3]]
M_zz <- parameters[[17]] # Ezz

if (parameters[[16]] == TRUE) { # then M_zz only dimension (dz-1) bc intercept has no var-cov
  M_zz_sub <- M_zz
  mean_z <- parameters[[1]][(dx2+2):length(parameters[[1]]),]

  M_zz <- cbind(1, t(mean_z))
  M_zz <- rbind(M_zz, cbind(mean_z, M_zz_sub))

  M_xx_tilde <- t(Pi) %*% M_zz %*% Pi # says is symmetric and pd
  M_xx_tilde_inv <- inv(M_xx_tilde)

}

# calculate theoretical avar

term1 <- gamma * (1 - gamma)
term2 <- (c * f)^2 * (tau_4 - 1)
term3 <- -2 * c * f * (1 - gamma - tau_c_2)
term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*% M_xx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
avar0 <- term1 + term2 + term3 + term4 # 0.02125649

# MC
registerDoFuture()
plan(cluster, workers = 7)
registerDoRNG(seed = seed)

timestart <- proc.time()

results <- foreach (m = (1:M), .combine = "rbind") %dorng% {

  data <- data_gen_2SLS(parameters[[1]], parameters[[2]], parameters[[3]], parameters[[4]], parameters[[5]], parameters[[6]],
                        parameters[[7]], parameters[[8]], parameters[[9]], parameters[[10]], parameters[[11]], sample.size=n)

  model <- robust2SLS(data=data[[1]], regression=parameters[[12]], sign_level=gamma, iterations=0, initial_est="robustified", graph=FALSE)
  num.outliers <- sum((model[[5]][[1]] == 0))
  num.nonmissing <- nrow(data[[1]]) - sum((model[[5]][[1]] == -1))
  gauge <- num.outliers / num.nonmissing

  data.frame(num.outliers, num.nonmissing, gauge)

}

timeend <- proc.time()
duration <- timeend - timestart
print(duration) # 22.68 s

mean(results$gauge) # 0.0499688
var(results$gauge) # 2.030147e-07
var(results$gauge) / (avar0/n) # 0.9550717



# new method:

p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)

results2 <- mc(100, 100000, seed = 42, parallel = TRUE, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = 0.05,
               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)

# duration 14.29 s
mean(results2$gauge) # 0.0499688
var(results2$gauge) # 2.030147e-07
var(results2$gauge) / (avar0/n) # 0.9550717

results1 <- as.data.frame(results)

identical(results1, results2) # not identical but almost
library(waldo)
compare(results1, results2) # original tibble saved as integer vector, df as double vector -> change tibble to df
results1$num.nonmissing <- as.double(results1$num.nonmissing)
identical(results1, results2) # TRUE




p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)



#doFuture::registerDoFuture()
#future::plan(cluster, workers = (ncores-1))
#doRNG::registerDoRNG(seed = seed)
#RNGkind("L'Ecuyer-CMRG") when using this then get different numbers because this is not the default random seed
plan(sequential)

results2 <- mc(1000, 100000, seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = 0.05,
               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)


ncores <- parallel::detectCores()
future::plan(cluster, workers = (ncores-1))
results5 <- mc(1000, 100000, seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = 0.05,
               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)

identical(results2, results3) # TRUE and first option took 300 s while first one only 130 so seems to work
# but for first one get error message: In foreach(m = (1:M), .combine = "rbind") %dorng% { : Foreach loop (doFuture) had changed the current RNG type: RNG was restored to same type, next state
identical(results2, results5)


# test mc_grid()
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
future::plan(cluster, workers = 6)
results <- mc_grid(100, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)

# now that mc_grid works, test whether it produces the same results as before
#####
manual_results <- data.frame()
# n = 1000, gamma = 0.01
gamma <- 0.01 # significance level for selection
c <- qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE) # cut-off value corresponding to gamma
phi <- 1 - gamma # probability of non-outlying
f <- dnorm(c, mean=0, sd=1) # f(c)
tau_c_2 <- phi - 2 * c * f # variance of standardised error truncated to -c to c
tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f # fourth moment of standardised error truncated to -c to c
tau_2 <- 1 # variance of standardised error
tau_4 <- 3 # fourth moment of standardised error
varsigma_c_2 <- tau_c_2 / phi

# Parameter generation
M <- 100
dx1 <- 3
dx2 <- 2
dx <- dx1 + dx2
dz1 <- dx1
dz2 <- 3
dz <- dz1 + dz2
sigma <- 2
n <- 1000
seed <- 42

parameters <- param_gen_2SLS(dx1=dx1, dx2=dx2, dz2=dz2, sigma=sigma, seed=seed, intercept=TRUE)

#extract parameters & update those that were simulated using only a subset due to notation issue
Omega <- parameters[[13]]
Omega <- rbind(matrix(0,dx1,1), Omega)
zeta_c_minus <- 2 * Omega * c

Sigma_half <- parameters[[14]]
Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

Pi <- parameters[[3]]
M_zz <- parameters[[17]] # Ezz

if (parameters[[16]] == TRUE) { # then M_zz only dimension (dz-1) bc intercept has no var-cov
  M_zz_sub <- M_zz
  mean_z <- parameters[[1]][(dx2+2):length(parameters[[1]]),]

  M_zz <- cbind(1, t(mean_z))
  M_zz <- rbind(M_zz, cbind(mean_z, M_zz_sub))

  M_xx_tilde <- t(Pi) %*% M_zz %*% Pi # says is symmetric and pd
  M_xx_tilde_inv <- inv(M_xx_tilde)

}

# calculate theoretical avar

term1 <- gamma * (1 - gamma)
term2 <- (c * f)^2 * (tau_4 - 1)
term3 <- -2 * c * f * (1 - gamma - tau_c_2)
term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*% M_xx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
avar0 <- term1 + term2 + term3 + term4 # 0.02125649

# MC
registerDoFuture()
plan(cluster, workers = 7)
registerDoRNG(seed = seed)

timestart <- proc.time()

results <- foreach (m = (1:M), .combine = "rbind") %dorng% {

  data <- data_gen_2SLS(parameters[[1]], parameters[[2]], parameters[[3]], parameters[[4]], parameters[[5]], parameters[[6]],
                        parameters[[7]], parameters[[8]], parameters[[9]], parameters[[10]], parameters[[11]], sample.size=n)

  model <- robust2SLS(data=data[[1]], regression=parameters[[12]], sign_level=gamma, iterations=0, initial_est="robustified", graph=FALSE)
  num.outliers <- sum((model[[5]][[1]] == 0))
  num.nonmissing <- nrow(data[[1]]) - sum((model[[5]][[1]] == -1))
  gauge <- num.outliers / num.nonmissing

  data.frame(num.outliers, num.nonmissing, gauge)

}

timeend <- proc.time()
duration <- timeend - timestart
print(duration) # 22.68 s

mean_gauge <- mean(results$gauge) # 0.0499688
var_gauge <- var(results$gauge) # 2.030147e-07
var_ratio <- var(results$gauge) / (avar0/n) # 0.9550717
sign_level <- gamma
iterations <- 0
avar <- avar0
res <- data.frame(M, n, sign_level, iterations, mean_gauge, var_gauge, avar, var_ratio)

manual_results <- rbind(manual_results, res)

# n = 100000, gamma = 0.01
gamma <- 0.01 # significance level for selection
c <- qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE) # cut-off value corresponding to gamma
phi <- 1 - gamma # probability of non-outlying
f <- dnorm(c, mean=0, sd=1) # f(c)
tau_c_2 <- phi - 2 * c * f # variance of standardised error truncated to -c to c
tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f # fourth moment of standardised error truncated to -c to c
tau_2 <- 1 # variance of standardised error
tau_4 <- 3 # fourth moment of standardised error
varsigma_c_2 <- tau_c_2 / phi

# Parameter generation
M <- 100
dx1 <- 3
dx2 <- 2
dx <- dx1 + dx2
dz1 <- dx1
dz2 <- 3
dz <- dz1 + dz2
sigma <- 2
n <- 100000
seed <- 42

parameters <- param_gen_2SLS(dx1=dx1, dx2=dx2, dz2=dz2, sigma=sigma, seed=seed, intercept=TRUE)

#extract parameters & update those that were simulated using only a subset due to notation issue
Omega <- parameters[[13]]
Omega <- rbind(matrix(0,dx1,1), Omega)
zeta_c_minus <- 2 * Omega * c

Sigma_half <- parameters[[14]]
Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

Pi <- parameters[[3]]
M_zz <- parameters[[17]] # Ezz

if (parameters[[16]] == TRUE) { # then M_zz only dimension (dz-1) bc intercept has no var-cov
  M_zz_sub <- M_zz
  mean_z <- parameters[[1]][(dx2+2):length(parameters[[1]]),]

  M_zz <- cbind(1, t(mean_z))
  M_zz <- rbind(M_zz, cbind(mean_z, M_zz_sub))

  M_xx_tilde <- t(Pi) %*% M_zz %*% Pi # says is symmetric and pd
  M_xx_tilde_inv <- inv(M_xx_tilde)

}

# calculate theoretical avar

term1 <- gamma * (1 - gamma)
term2 <- (c * f)^2 * (tau_4 - 1)
term3 <- -2 * c * f * (1 - gamma - tau_c_2)
term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*% M_xx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
avar0 <- term1 + term2 + term3 + term4 # 0.02125649

# MC
registerDoFuture()
plan(cluster, workers = 7)
registerDoRNG(seed = seed)

timestart <- proc.time()

results <- foreach (m = (1:M), .combine = "rbind") %dorng% {

  data <- data_gen_2SLS(parameters[[1]], parameters[[2]], parameters[[3]], parameters[[4]], parameters[[5]], parameters[[6]],
                        parameters[[7]], parameters[[8]], parameters[[9]], parameters[[10]], parameters[[11]], sample.size=n)

  model <- robust2SLS(data=data[[1]], regression=parameters[[12]], sign_level=gamma, iterations=0, initial_est="robustified", graph=FALSE)
  num.outliers <- sum((model[[5]][[1]] == 0))
  num.nonmissing <- nrow(data[[1]]) - sum((model[[5]][[1]] == -1))
  gauge <- num.outliers / num.nonmissing

  data.frame(num.outliers, num.nonmissing, gauge)

}

timeend <- proc.time()
duration <- timeend - timestart
print(duration)

mean_gauge <- mean(results$gauge)
var_gauge <- var(results$gauge)
var_ratio <- var(results$gauge) / (avar0/n)
sign_level <- gamma
iterations <- 0
avar <- avar0
res <- data.frame(M, n, sign_level, iterations, mean_gauge, var_gauge, avar, var_ratio)

manual_results <- rbind(manual_results, res)

# n = 1000, gamma = 0.05
gamma <- 0.05 # significance level for selection
c <- qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE) # cut-off value corresponding to gamma
phi <- 1 - gamma # probability of non-outlying
f <- dnorm(c, mean=0, sd=1) # f(c)
tau_c_2 <- phi - 2 * c * f # variance of standardised error truncated to -c to c
tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f # fourth moment of standardised error truncated to -c to c
tau_2 <- 1 # variance of standardised error
tau_4 <- 3 # fourth moment of standardised error
varsigma_c_2 <- tau_c_2 / phi

# Parameter generation
M <- 100
dx1 <- 3
dx2 <- 2
dx <- dx1 + dx2
dz1 <- dx1
dz2 <- 3
dz <- dz1 + dz2
sigma <- 2
n <- 1000
seed <- 42

parameters <- param_gen_2SLS(dx1=dx1, dx2=dx2, dz2=dz2, sigma=sigma, seed=seed, intercept=TRUE)

#extract parameters & update those that were simulated using only a subset due to notation issue
Omega <- parameters[[13]]
Omega <- rbind(matrix(0,dx1,1), Omega)
zeta_c_minus <- 2 * Omega * c

Sigma_half <- parameters[[14]]
Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

Pi <- parameters[[3]]
M_zz <- parameters[[17]] # Ezz

if (parameters[[16]] == TRUE) { # then M_zz only dimension (dz-1) bc intercept has no var-cov
  M_zz_sub <- M_zz
  mean_z <- parameters[[1]][(dx2+2):length(parameters[[1]]),]

  M_zz <- cbind(1, t(mean_z))
  M_zz <- rbind(M_zz, cbind(mean_z, M_zz_sub))

  M_xx_tilde <- t(Pi) %*% M_zz %*% Pi # says is symmetric and pd
  M_xx_tilde_inv <- inv(M_xx_tilde)

}

# calculate theoretical avar

term1 <- gamma * (1 - gamma)
term2 <- (c * f)^2 * (tau_4 - 1)
term3 <- -2 * c * f * (1 - gamma - tau_c_2)
term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*% M_xx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
avar0 <- term1 + term2 + term3 + term4 # 0.02125649

# MC
registerDoFuture()
plan(cluster, workers = 7)
registerDoRNG(seed = seed)

timestart <- proc.time()

results <- foreach (m = (1:M), .combine = "rbind") %dorng% {

  data <- data_gen_2SLS(parameters[[1]], parameters[[2]], parameters[[3]], parameters[[4]], parameters[[5]], parameters[[6]],
                        parameters[[7]], parameters[[8]], parameters[[9]], parameters[[10]], parameters[[11]], sample.size=n)

  model <- robust2SLS(data=data[[1]], regression=parameters[[12]], sign_level=gamma, iterations=0, initial_est="robustified", graph=FALSE)
  num.outliers <- sum((model[[5]][[1]] == 0))
  num.nonmissing <- nrow(data[[1]]) - sum((model[[5]][[1]] == -1))
  gauge <- num.outliers / num.nonmissing

  data.frame(num.outliers, num.nonmissing, gauge)

}

timeend <- proc.time()
duration <- timeend - timestart
print(duration)

mean_gauge <- mean(results$gauge)
var_gauge <- var(results$gauge)
var_ratio <- var(results$gauge) / (avar0/n)
sign_level <- gamma
iterations <- 0
avar <- avar0
res <- data.frame(M, n, sign_level, iterations, mean_gauge, var_gauge, avar, var_ratio)

manual_results <- rbind(manual_results, res)

# n = 100000, gamma = 0.05
gamma <- 0.05 # significance level for selection
c <- qnorm(gamma/2, mean=0, sd=1, lower.tail = FALSE) # cut-off value corresponding to gamma
phi <- 1 - gamma # probability of non-outlying
f <- dnorm(c, mean=0, sd=1) # f(c)
tau_c_2 <- phi - 2 * c * f # variance of standardised error truncated to -c to c
tau_c_4 <- 3 * phi - 2 * c * (c^2 + 3) * f # fourth moment of standardised error truncated to -c to c
tau_2 <- 1 # variance of standardised error
tau_4 <- 3 # fourth moment of standardised error
varsigma_c_2 <- tau_c_2 / phi

# Parameter generation
M <- 100
dx1 <- 3
dx2 <- 2
dx <- dx1 + dx2
dz1 <- dx1
dz2 <- 3
dz <- dz1 + dz2
sigma <- 2
n <- 100000
seed <- 42

parameters <- param_gen_2SLS(dx1=dx1, dx2=dx2, dz2=dz2, sigma=sigma, seed=seed, intercept=TRUE)

#extract parameters & update those that were simulated using only a subset due to notation issue
Omega <- parameters[[13]]
Omega <- rbind(matrix(0,dx1,1), Omega)
zeta_c_minus <- 2 * Omega * c

Sigma_half <- parameters[[14]]
Sigma_half <- cbind(matrix(0,dx2,dx1), Sigma_half)
Sigma_half <- rbind(cbind(matrix(0,dx1,dx1),matrix(0,dx1,dx2)), Sigma_half)

Pi <- parameters[[3]]
M_zz <- parameters[[17]] # Ezz

if (parameters[[16]] == TRUE) { # then M_zz only dimension (dz-1) bc intercept has no var-cov
  M_zz_sub <- M_zz
  mean_z <- parameters[[1]][(dx2+2):length(parameters[[1]]),]

  M_zz <- cbind(1, t(mean_z))
  M_zz <- rbind(M_zz, cbind(mean_z, M_zz_sub))

  M_xx_tilde <- t(Pi) %*% M_zz %*% Pi # says is symmetric and pd
  M_xx_tilde_inv <- inv(M_xx_tilde)

}

# calculate theoretical avar

term1 <- gamma * (1 - gamma)
term2 <- (c * f)^2 * (tau_4 - 1)
term3 <- -2 * c * f * (1 - gamma - tau_c_2)
term4 <- f^2 * t((2*c*Omega - zeta_c_minus)) %*% Sigma_half %*% M_xx_tilde_inv %*% Sigma_half %*% (2*c*Omega - zeta_c_minus)
avar0 <- term1 + term2 + term3 + term4 # 0.02125649

# MC
registerDoFuture()
plan(cluster, workers = 7)
registerDoRNG(seed = seed)

timestart <- proc.time()

results <- foreach (m = (1:M), .combine = "rbind") %dorng% {

  data <- data_gen_2SLS(parameters[[1]], parameters[[2]], parameters[[3]], parameters[[4]], parameters[[5]], parameters[[6]],
                        parameters[[7]], parameters[[8]], parameters[[9]], parameters[[10]], parameters[[11]], sample.size=n)

  model <- robust2SLS(data=data[[1]], regression=parameters[[12]], sign_level=gamma, iterations=0, initial_est="robustified", graph=FALSE)
  num.outliers <- sum((model[[5]][[1]] == 0))
  num.nonmissing <- nrow(data[[1]]) - sum((model[[5]][[1]] == -1))
  gauge <- num.outliers / num.nonmissing

  data.frame(num.outliers, num.nonmissing, gauge)

}

timeend <- proc.time()
duration <- timeend - timestart
print(duration)

mean_gauge <- mean(results$gauge)
var_gauge <- var(results$gauge)
var_ratio <- var(results$gauge) / (avar0/n)
sign_level <- gamma
iterations <- 0
avar <- avar0
res <- data.frame(M, n, sign_level, iterations, mean_gauge, var_gauge, avar, var_ratio)

manual_results <- rbind(manual_results, res)

# results via function
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
future::plan(cluster, workers = 6)
package_results <- mc_grid(100, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                   initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
identical(manual_results, package_results) # TRUE --> NIIIICEEE!


# also works for only one "combination"
package_results <- mc_grid(100, n = 1000, seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = 0.01,
                           initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)



p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
future::plan(cluster, workers = 6)
package_results1 <- mc_grid(500, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                           initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
# duration: 129.08 s
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
future::plan(sequential)
package_results2 <- mc_grid(500, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                           initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
# duration: 283.80 s

# now took out the doFuture:registerDoFuture()
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doFuture)
registerDoFuture()
future::plan(cluster, workers = 6)
package_results1 <- mc_grid(500, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                            initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
# get the same results as before, so nothing broke


# now want to try using the function with a different backend such as doParallel
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
package_results_new <- mc_grid(500, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                            initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
stopCluster(cl)



# try new grid possibilities
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doFuture)
registerDoFuture()
future::plan(cluster, workers = 6)
package_results1 <- mc_grid(100, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                            initial_est = c("robustified", "saturated"), iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = c(0.4,0.5))


p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doFuture)
registerDoFuture()
future::plan(cluster, workers = 6)
package_results1 <- mc_grid(1000, n = 500000, seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                            initial_est = "saturated", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = c(0.2, 0.3, 0.4, 0.5))





# testing
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
package_results_new <- mc_grid(500, n = c(1000, 100000), seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
stopCluster(cl)

# testing MC 2
p <- generate_param(3, 2, 3, sigma = 2, intercept = TRUE, seed = 42)
library(doParallel)
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
package_results_new <- mc_grid(3, n = 50, seed = 42, parameters = p, formula = p$setting$formula, ref_dist = "normal", sign_level = c(0.01, 0.05),
                               initial_est = "robustified", iterations = 0, shuffle = FALSE, shuffle_seed = NULL, split = 0.5)
stopCluster(cl)
