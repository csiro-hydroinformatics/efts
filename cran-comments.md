
## Test environments

* Local Linux Debian, R 3.4.4
* Local Windows 7 R 3.4.4   and R 3.3.3
* win-builder (devel and release)

```R
library(testthat)
library(devtools)
efts_dir <- '~/src/github_jm/efts'
devtools::test(efts_dir)
devtools::check(efts_dir, cran=TRUE)
devtools::build_win(pkg = efts_dir, version = c("R-release", "R-devel"))
```

## R CMD check results

No ERRORs or WARNINGs

1 NOTE which I interpret as unavoidable:
Maintainer: 'Jean-Michel Perraud <jean-michel.perraud@csiro.au>'
New submission


## Downstream dependencies

None

