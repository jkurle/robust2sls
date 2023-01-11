# CRAN Comments for: robust2sls
#### 07 January 2023, version 0.2.2

* Follows the email from Prof Brian Ripley that the package "doRNG" has been orphaned and therefore 
needs to be removed from the Imports field.
* I moved "doRNG" from Imports to Suggests because it is only used in one function for advanced users anyway.
* I did not want to remove "doRNG" completely. To my knowledge, the [maintainer has started working on it again](https://github.com/renozao/doRNG/issues/23#issuecomment-1376285971), so its orphaned status might soon be reverted.

## Test environments

* GitHub Actions:
  ** Windows 10.0.20348, Build 768, x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
  ** MacOS 11.6.7 (20G630), x86_64-apple-darwin17.0 (64-bit), R version 4.2.1 (2022-06-23)
  ** Ubuntu 20.04.4 LTS, Linux kernel version: 5.15.0-1014-azure, x86_64-pc-linux-gnu (64-bit),
    ** R version 4.1.3 (2022-03-10)
    ** R version 4.2.1 (2022-06-23)
    ** R under development (unstable) (2022-07-21 r82611)
* win-builder:
  * old-release: Windows x86_64-w64-mingw32 (64-bit), R version 4.1.3 (2022-03-10)
  * release: Windows x86_64-w64-mingw32 (64-bit), R version 4.2.2 (2022-06-23 ucrt)
  * development: Windows x86_64-w64-mingw32 (64-bit), R under development, (2023-01-04 r83561 ucrt), to be R version 4.3.0
* own machine:
  * Windows 11, version 10.0.22621, Build 22621, x86_64-w64-mingw32/x64 (64-bit), R version 4.1.3 (2022-03-10)

## R CMD check results

Checks produced no ERRORs, no WARNINGs, 2 NOTEs.

NOTE 1: 

* Possibly mis-spelled words in DESCRIPTION: *Jiao* and *SLS*
* Both spellings are correct: *Jiao* is an author name, *SLS* is a standard abbreviation in *2SLS* standing for "Two Stage Least Squares"

NOTE 2:

* Suggests orphaned package: `doRNG'
* As explained above, I want to keep "doRNG" for now. I hope that its orphaned status will be reverted soon. If not, I can remove the package completely in a future update.

## Downstream dependencies

There are currently no downstream dependencies for this package.
