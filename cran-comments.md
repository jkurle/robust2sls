# CRAN Comments for: robust2sls
#### 20 May 2025, version 0.2.3

* Follows the email from Kurt Hornik alerting me about NOTEs in the CRAN incoming feasibility checks
* These issues are now fixed with minor changes to function documentation and re-building the binaries

## Test environments

* GitHub Actions:
  * Windows 10.0.20348, Build 1366, x86_64-w64-mingw32 (64-bit), R version 4.2.2 (2022-10-31 ucrt)
  * MacOS 12.6.2 (21G320), x86_64-apple-darwin17.0 (64-bit), R version 4.2.2 (2022-10-31)
  * Ubuntu 20.04.5 LTS, Linux kernel version: 5.15.0-1023-azure, x86_64-pc-linux-gnu (64-bit),
    * R version 4.1.3 (2022-03-10)
    * R version 4.2.2 (2022-10-31)
    * R under development (unstable) (2023-01-09 r83588)
* win-builder:
  * old-release: Windows x86_64-w64-mingw32 (64-bit), R version 4.1.3 (2022-03-10)
  * release: Windows x86_64-w64-mingw32 (64-bit), R version 4.2.2 (2022-06-23 ucrt)
  * development: Windows x86_64-w64-mingw32 (64-bit), R under development, (2023-01-04 r83561 ucrt), to be R version 4.3.0
* own machine:
  * Windows 11, version 24H2, Build 226100, x86_64-w64-mingw32/x64 (64-bit), R version 4.5.0 (2025-04-11)

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
