## Maintenance Submission

Maintenance release with a few fixes and a minor feature listed in news.md

## Test environments

* Local Linux Debian, R 3.5.2
* Local Windows 10 R 3.5.2
* win-builder (release and devel)

```R
library(testthat)
library(devtools)
efts_dir <- '~/src/github_jm/efts'
devtools::document(efts_dir)
devtools::test(efts_dir)
devtools::check(efts_dir, document = TRUE,
  manual = TRUE, cran = TRUE, 
  # check_version = TRUE,
  force_suggests = TRUE, run_dont_test = TRUE)
devtools::build_win(pkg = efts_dir, version = c("R-release", "R-devel"))
```

## R CMD check results

Local Windows 10, win-builder (release) and Linux, R 3.5.2

No ERRORs or WARNINGs
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

win-builder (devel):

Status: 2 ERRORs, 1 WARNING, but not reproducible on Windows 10 R 3.6.0 prerelease. `devtools::test(...)` passes, though devtools::check fails to run the tests with an error that appears external to `efts`.

## Downstream dependencies

None
