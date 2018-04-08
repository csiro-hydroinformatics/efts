

http://stat545.com/packages05_foofactors-package-02.html#use-readme.rmd


```R
library(devtools)
devtools::document('~/src/github_jm/efts')
devtools::build_win(pkg = '~/src/github_jm/efts', version = c("R-release", "R-devel"))
```

```R
library(devtools)
efts_dir <- '/path/to/efts'
document(efts_dir)
build(efts_dir, vignettes = FALSE)
```

```R
library(testthat)
library(devtools)
devtools::test('~/src/github_jm/efts')
```

```
R CMD build efts
R CMD check efts_0.7.0.tar.gz 
```

```
library(efts)
help('efts', help_type='html')
```

```R

timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Canberra")
(time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, tStep = "hours since", tStepDelta = 3L, tzoffset = "+1000"))

# Note that the time zone information of thes sart date is NOT 
# used by create_time_info; the tzoffset argument takes precedence 
timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Perth")
(time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, tStep = "hours since", tStepDelta = 3L, tzoffset = "+1000"))

create_time_info(from, n, tStep = "hours since", tStepDelta = 1L, tzoffset)
```