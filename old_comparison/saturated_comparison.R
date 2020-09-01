# initial_estimators setup for testing as develop

library(ggplot2) # to draw graphs
library(assertthat) # to test the inputs of the function
library(AER) # for ivreg command
library(foreign) # just to write Stata .dta file
library(datasets) # just to have more datasets to test

data <- mtcars
formula <- mpg ~ cyl + disp | cyl + wt
split <- 0.5
shuffle <- TRUE
shuffle_seed <- 42
cutoff <- 1.96
data[1, "mpg"] <- NA
data[2, "cyl"] <- NA
data[3, "disp"] <- NA
data[4, "wt"] <- NA

test <- saturated_init(data = data, formula = formula, cutoff = cutoff, shuffle = shuffle, shuffle_seed = shuffle_seed, split = split)

robust2SLS <-
  function(data, regression, sign_level, iterations = 1, initial_est = "robustified", convergence_criterion = NULL, shuffle = FALSE, split = 0.5, graph = TRUE) {

    #check that have received expected input types
    assert_that(is.data.frame(data), msg = "data input is not a dataframe")
    assert_that(class(regression) == "formula", msg = "regression input is not a formula")
    assert_that(is.numeric(sign_level), (sign_level < 1), (0 < sign_level), msg = "sign_level input must be numeric between 0 and 1")
    assert_that((is.numeric(iterations) | is.character(iterations)), msg = "iterations input must be a numeric integer > 0 or the character string \"convergence\"")
    if (is.numeric(iterations)) {
      assert_that(iterations > 0, msg = "iterations input must be > 0")
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

    if (is.numeric(iterations)) { # user specified a specific number of iterations

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

original <- robust2SLS(data = data, regression = formula, sign_level = 0.05, iterations = 5, initial_est = "saturated", shuffle = TRUE, split = 0.5, graph = FALSE)

identical(test$res, original[[2]][[1]]) # TRUE
identical(test$stdres, original[[3]][[1]]) # TRUE
identical(test$sel, original[[4]][[1]]) # TRUE
identical(test$type, original[[5]][[1]]) # TRUE



original <- robust2SLS(data = data, regression = formula, sign_level = 0.05, iterations = 5, initial_est = "robustified", shuffle = TRUE, split = 0.5, graph = FALSE)
