
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2sls

<!-- badges: start -->

<!-- badges: end -->

The goal of r2sls is to provide easy-to-use tools for outlier-robust
inference and outlier testing in two-stage least-squares (2SLS) models.

## Installation

There is no released version on [CRAN](https://CRAN.R-project.org) yet.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jkurle/r2sls")
```

## Note about Versions of Dependencies

The `Depends:` and `Suggests:` fields in the `DESCRIPTION` file have no
minimum or maximum version because we cannot test how far our package is
compatible with older versions of the dependencies. However, we also did
not want to require the versions that were used during the development
to not force users to update their packages and potentially break their
other existing code.

For your information, r2sls was developed under the following versions:

  - R: version 4.0.2 (2020-06-22)
  - datasets: version 4.0.2
  - grDevices: version 4.0.2
  - stats: version 4.0.2
  - AER: version 1.2.9
  - ggplot2: 3.3.2
  - testthat: 2.99.0.9000 (development version using edition 3)
