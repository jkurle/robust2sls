norm_snap <- function(x, digits = 12) {
  if (is.double(x)) {
    x <- round(x, digits)
    x[abs(x) < 10^(-digits)] <- 0
    return(x)
  }

  if (is.list(x)) {
    x[] <- lapply(x, norm_snap, digits = digits)
    return(x)
  }

  x
}
