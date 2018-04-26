## Resubmission

* Address feedback kindly given by the CRAN reviewer:
    * use single quote around packages and software names ('efts' and 'netCDF') in DESCRIPTION.
    * function examples are not marked as 'dontrun' or 'donttest'
    * all tests, examples, vignettes use 'tempfile()' for transient file creation
* Functional changes: facilities and stricter checks for compliance with the proposed netCDF convention.

## Test environments

* Local Linux Debian, R 3.4.4
* Local Windows 7 R 3.4.4 
* win-builder (devel and release)

```R
library(testthat)
library(devtools)
efts_dir <- '~/src/github_jm/efts'
devtools::document(efts_dir)
devtools::test(efts_dir)
devtools::check(efts_dir, document = TRUE, 
  manual = TRUE, cran = TRUE, check_version = TRUE,
  force_suggests = TRUE, run_dont_test = TRUE)
devtools::build_win(pkg = efts_dir, version = c("R-release", "R-devel"))
```

## R CMD check results

No ERRORs or WARNINGs

1 NOTE which I interpret as unavoidable:
Maintainer: 'Jean-Michel Perraud <jean-michel.perraud@csiro.au>'
New submission


## Downstream dependencies

None

