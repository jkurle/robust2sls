#' Create indices for nonparametric bootstrap
#'
#' \code{nonparametric} is used for nonparametric resampling, for example
#' nonparametric case or error/residual resampling. The function takes a vector
#' of indices that correspond to the index of observations that should be used
#' in the resampling procedure.
#'
#' @param indices A vector of indices (integer) from which to sample.
#' @param R An integer specifying the number of resamples.
#' @param size An integer specifying the size of the resample. Standard
#' bootstrap suggests to resample as many datapoints as in the original sample,
#' which is set as the default.
#' @param replacement A logical value whether to sample with (TRUE) or without
#' (FALSE) replacement. Standard bootstrap suggests to resample with
#' replacement, which is set as the default.
#' @param seed \code{NULL} if seed should not be set explicitly or an integer to
#' which the seed is set. Since this function is usually used inside other
#' functions, it might not be desirable to set a seed explicitly.
#'
#' @return \code{nonparametric} returns a list of length \code{R} containing
#' vectors with the resampled indices.

nonparametric <- function(indices, R, size = length(indices),
                          replacement = TRUE, seed = NULL) {

  if (!is.null(seed)) {
    if (!(seed %% 1 == 0)) {
      stop(strwrap("Argument 'seed' must either be NULL or an integer",
                   prefix = " ", initial = ""))
    } else {
      set.seed(seed = seed)
    }
  }

  resamples <- vector("list", length = R)

  for (i in 1:R) {
    resamples[[i]] <- sample(x = indices, size = size, replace = replacement)
  }

  return(resamples)

}

#' Counts the number of times each index was sampled
#'
#' \code{count_indices} takes a list of indices for resampling and counts how
#' each index was sampled in each resample. The results is returned as a matrix
#' where each row corresponds to a different resample and each column to each
#' index.
#'
#' @param resamples A list of resamples, as created by \link{nonparametric}.
#' @param indices The vector of original indices from which the resamples were
#' drawn.
#'
#' @return \code{count_indices} returns a list with two names elements. Each
#' element is a matrix that stores how often each observation/index was
#' resampled (column) for each resample (row). \code{$count_clean} only has
#' columns for observations that were available in the indices.
#' \code{$count_all} counts the occurrence of all indices in the range of
#' indices that were provided, even if the index was actually not available in
#' the given indices. These are of course zero since they were not available for
#' resampling. If the given indices do not skip any numbers, the two coincide.

count_indices <- function(resamples, indices) {

  # take max value as the number of indices so each index gets its own bin
  bins <- max(indices)

  index_counts <- NULL

  for (i in 1:length(resamples)) {

    tab <- tabulate(resamples[[i]], nbins = bins)
    index_counts <- rbind(index_counts, tab)

  }

  rownames(index_counts) <- NULL
  index_counts_clean <- index_counts[, indices]
  colnames(index_counts) <- paste("o", 1:bins, sep = "")
  colnames(index_counts_clean) <- paste("o", indices, sep = "")

  out <- list(count_all = index_counts, count_clean = index_counts_clean)

  return(out)

}

#' Nonparametric resampling
#'
#'
#'

#nonparametric_resampling <- function()



