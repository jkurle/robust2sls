# robust2sls 0.2.0

## Major changes

Introduction of a suite of new functions implementing several tests for the
presence of outliers in the sample.

* proportion test
* count test
* global test using Simes (1986) procedure
* scaling sum test
* scaling sup test

A vignette is included that illustrates the usage of these testing functions.

## Minor changes

* change gauge_avar() to accept NULL as input for split argument
* implement asymptotic covariance of the FODR for different cut-offs
* utility function multi_cutoff() that allows to apply the same outlier detection algorithm with different cut-off / gamma values

# robust2sls 0.1.0 (initial release)
