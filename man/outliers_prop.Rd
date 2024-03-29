% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gauge.R
\name{outliers_prop}
\alias{outliers_prop}
\title{Proportion of outliers}
\usage{
outliers_prop(robust2sls_object, iteration)
}
\arguments{
\item{robust2sls_object}{An object of class \code{"robust2sls"}.}

\item{iteration}{An integer >= 0 representing the iteration for which the
outliers are calculated.}
}
\value{
\code{outliers_prop} returns the proportion of outliers for a given
iteration as determined by the outlier-detection algorithm.
}
\description{
\code{outliers_prop} calculates the proportion of outliers relative to all
non-missing observations in the full sample from a \code{"robust2sls"} object
for a given iteration.
}
