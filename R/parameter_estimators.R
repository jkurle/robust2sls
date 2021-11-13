#' Estimation of moments of the data
#'
#' \code{estimate_param_null} can be used to estimate certain moments of the
#' data that are required for calculating the asymptotic variance of the gauge.
#' Such moments are the covariance between the standardised first stage errors
#' and the structural error \eqn{\Omega}, the covariance matrix of the first
#' stage errors \eqn{\Sigma}, the first stage parameter matrix \eqn{\Pi}, and
#' more.
#'
#' @param robust2SLS_object An object of class \code{"robust2sls"} for which
#' the moments will be calculated.
#'
#' @return \code{estimate_param_null} returns a list with a similar structure as
#' the output of the Monte Carlo functionality \link{generate_param}. Hence, the
#' resulting list can be given to the function \link{gauge_avar} as argument
#' \code{parameters} to return an estimate of the asymptotic variance of the
#' gauge.
#'
#' @section Warning:
#' The function uses the full sample to estimate the moments. Therefore, they
#' are only consistent under the null hypothesis of no outliers and estimators
#' are likely to be inconsistent under the alternative.
#' @export

estimate_param_null <- function(robust2SLS_object) {

  # extract the dataset
  data <- robust2SLS_object$cons$data

  # calculate first stage linear projections
  fml <- extract_formula(robust2SLS_object$cons$formula)
  dx1 <- length(fml$x1_var)
  dx2 <- length(fml$x2_var)
  dz1 <- length(fml$z1_var)
  dz2 <- length(fml$z2_var)

  Pi_hat <- NULL
  R2_hat <- NULL

  # create the first stage formulas
  z <- union(fml$z1_var, fml$z2_var)
  part2 <- paste(z, collapse = " + ")
  part2 <- paste("0 + ", part2, sep = "") # take out intercept
  for (i in seq_along(fml$x2_var)) {
    depvar <- fml$x2_var[[i]]
    formula1 <- paste(depvar, part2, sep = " ~ ")
    model1 <- NULL
    # run first stages
    command <- paste("model1 <- stats::lm(formula = as.formula(formula1),
                     data = data)")
    expr <- parse(text = command)
    eval(expr)
    pi_hat <- as.matrix(model1$coefficients, (dz1+dz2), 1) # into column vector
    colnames(pi_hat) <- depvar
    Pi_hat <- cbind(Pi_hat, pi_hat)

    r2_hat <- data[, fml$x2_var[[i]]] - stats::predict(model1, newdata = data)
    R2_hat <- cbind(R2_hat, r2_hat)

  }

  dimnames(R2_hat) <- NULL

  # alternative to get R2_hat via ivreg model object
  # other <- data[data[[selection_name]], fml$x2_var] -
  #   stats::model.matrix(robust2SLS_object$model[[iteration + 2]],
  #                       component = c("projected"))[, fml$x2_var]

  # pad the matrix to account for perfectly fit exogenous regressors
  Pi_hat <- cbind(rbind(diag(dx1), matrix(0,dz2,dx1)), Pi_hat)

  # estimate Mzz = Ezz' = Var(z) + Ez Ez'
  Z <- data[, z]
  ZZ <- t(as.matrix(Z)) %*% as.matrix(Z)
  Mzz_hat <- ZZ / NROW(Z)

  # estimate Mxx_tilde_inv
  Mxx_tilde_hat <- t(Pi_hat) %*% Mzz_hat %*% Pi_hat
  Mxx_tilde_inv_hat <- pracma::inv(Mxx_tilde_hat)

  # estimate Sigma2 = E(r2i r2i')
  Sigma2_hat <- stats::cov(R2_hat)
  Sigma2_half_hat <- pracma::sqrtm(Sigma2_hat)$B
  Sigma2_half_inv_hat <- pracma::inv(Sigma2_half_hat)

  # pad the matrix to get estimate of Sigma
  Sigma_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                     cbind(matrix(0, dx2, dx1), Sigma2_hat))
  Sigma_half_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                          cbind(matrix(0, dx2, dx1), Sigma2_half_hat))
  Sigma_half_inv_hat <- rbind(cbind(matrix(0, dx1, dx1), matrix(0, dx1, dx2)),
                              cbind(matrix(0, dx2, dx1), Sigma2_half_inv_hat))

  # estimate E(r2i ui)
  # run full model and obtain the standardised residuals
  full <- AER::ivreg(formula = robust2SLS_object$cons$formula, data = data,
                     model = TRUE, y = TRUE)
  u_std_hat <- full$residuals / full$sigma
  MRu_hat <- colMeans(R2_hat * u_std_hat)

  # estimate Omega2
  Omega2_hat <- Sigma2_half_inv_hat %*% MRu_hat

  # pad the vector to get estimate of Omega
  Omega_hat <- rbind(matrix(0, dx1, 1), Omega2_hat)

  # try alternative and see whether works better; get virtually the same
  # probably only different because below didn't reverse the df correction
  # u_hat <- robust2SLS_object$model$m1$residuals
  # MRu <- colMeans(R2_hat * u_hat)
  # sigma <- robust2SLS_object$model$m1$sigma * sqrt(robust2SLS_object$cons$bias_corr)
  # Omega2_hat <- Sigma2_half_inv_hat %*% MRu / sigma

  set <- list(call = robust2SLS_object$cons$call,
              formula = robust2SLS_object$cons$formula, dx1 = dx1, dx2 = dx2,
              dz2 = dz2)
  nam <- list(y = fml$y_var, x1 = fml$x1_var, x2 = fml$x2_var, z2 = fml$z2_var)
  par <- list(Omega = Omega_hat, Sigma_half = Sigma_half_hat,
              Mxx_tilde_inv = Mxx_tilde_inv_hat)
  out <- list(params = par, setting = set, names = nam)
  return(out)

}
