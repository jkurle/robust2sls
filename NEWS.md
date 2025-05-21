# robust2sls (development version)

# robust2sls 0.2.3

## Bug Fixes

* update documentation
* re-create binaries with newer R version to address CRAN NOTEs

# robust2sls 0.2.2

## Minor changes

* move package [doRNG](https://cran.r-project.org/package=doRNG) from `Imports` to `Suggests`
because the package has been orphaned as of 2022-12-19. Only affects the function 
`mc_grid()`, which is anyway for advanced users only

## Bug Fixes

* fix bug in unit testing

# robust2sls 0.2.1

## Major changes

* addition of Impulse Indicator Saturation (IIS) as initial estimator for outlier detection
(from ivgets package)

## Minor changes

* code coverage has been extended to 100%
* update formulae for FODR under normality (estimation not required)
* all 2SLS estimation commands originally from AER package now use the ivreg package
* fixed \eqn or \deqn or with empty second argument, as suggested by CRAN maintainers


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
