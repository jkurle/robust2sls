# CRAN Comments for: robust2sls
#### 15 August 2022, version 0.2.1

## Test environments

* GitHub Actions:
  * Windows 10.0.20348, Build 768, x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
  * MacOS 11.6.7 (20G630), x86_64-apple-darwin17.0 (64-bit), R version 4.2.1 (2022-06-23)
  * Ubuntu 20.04.4 LTS, Linux kernel version: 5.15.0-1014-azure, x86_64-pc-linux-gnu (64-bit),
    * R version 4.1.3 (2022-03-10)
    * R version 4.2.1 (2022-06-23)
    * R under development (unstable) (2022-07-21 r82611)
* win-builder:
  * old-release: Windows x86_64-w64-mingw32 (64-bit), R version 4.1.3 (2022-03-10)
  * release: Windows x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
  * development: Windows x86_64-w64-mingw32 (64-bit), R under development (unstable) (2022-08-14 r82716 ucrt)
* own machine:
  * Windows 11, version 21H2, Build 22000.739, x86_64-w64-mingw32/x64 (64-bit), R version 4.1.3 (2022-03-10)

## R CMD check results

Checks produced no ERRORs, WARNINGS, or NOTEs except for:

* win-builder old-release had 1 NOTE: Possibly mis-spelled words in DESCRIPTION: *Jiao* and *SLS*
  * both spellings are correct: *Jiao* is an author name, *SLS* is a standard abbreviation in *2SLS* standing for "Two Stage Least Squares" 

## Downstream dependencies

There are currently no downstream dependencies for this package.
