# robust2sls 0.2.4

This version only updates the unit tests and vignettes.

## Changes to Unit Tests
* discovered slight changes in 14th or 15th decimals
* discovered changes to some random number generations from `generate_data()`, 
which were traced back to changes in the environment and changes in the `MASS` pkg
* restructured the tests to rely on fixtures
  - created fixed datasets, stored in tests/testthat/testdata/
  - replaced `generate_data()` with `readRDS()` where possible
  - for `mc_grid()`, artifical data generation happens within the function body
  using `generate_data()`; to make it use fixed datasets, used the `mockery` pkg
  and macked the function to read in the stored datasets instead of drawing
  randim data
* fixed GH issue #19, making parallel execution more robust
* introduced some tolerance to numerical comparisons
  - sup_test simulates pvalue, allowed for 0.015 tolerance diff (e.g. across OS)
  - iis_init snapshots rounded to tolerance 12 digits, set to 0 if smaller than that

## Detailed Comments
* created two tags, which represented steps along the way of these improvements:
  - v0.2.3-fixtures was developed on the old environment, changed tests to rely
  on the fixed datasets and ensured all tests still passed; then converted to
  new environment
  - v0.2.3-ivreg-fixes updated code (tests and vignettes) to avoid new warnings
  about collinearity, which did not appear previously
  - also updated the `gets` pkg to v0.40, no changes were required

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
