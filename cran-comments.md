## Resubmission
This is a resubmission. Thank you for your helpful comments! In this version, I have:

* added a link to the working paper on which some of the methods in the R package are based to the description field in DESCRIPTION

* added descriptions of the return value to the documentation of the exported functions for which it was missing: gauge_avar(), print.robust2sls(), saturated_init(), user_init(), validate_robust2sls()

* deleted any code within functions that uses remove() to clean up the workspace. All my internal "testthat" tests still pass.

* made it clear that the vignette "monte-carlo" uses 2 cores and not more

* the local R CMD check as well as the three GitHub checks all still passed with 0 errors, 0 warnings, and 0 notes

# CRAN Comments for: robust2sls
#### 14 Nov 2021, version 0.1.0

## Test environments

* GitHub Actions:
  * Windows 10.0.17763 Build 2300, x86_64-w64-mingw32 (64-bit), R version 4.1.2
  * MacOS x86_64-apple-darwin17.0, R version 4.1.2
  * Ubuntu x86_64-pc-linux-gnu, R version 4.1.2
* win-builder:
  * Windows x86_64-w64-mingw32 (64-bit), R version 4.1.1
  * Windows x86_64-w64-mingw32 (64-bit), R development version (2021-11-12 r81187)
* own machine:
  * Windows 10.0.19042 Build 1348, x86_64-w64-mingw32 (64-bit), R version 4.1.1

## R CMD check results

There were no ERRORs, WARNINGS, or NOTEs.

## Downstream dependencies

First submission, so there are no downstream dependencies for this package.
