

[Using Rmd format for generating Readme.md](http://stat545.com/packages05_foofactors-package-02.html#use-readme.rmd)


```R
library(testthat)
library(devtools)
efts_dir <- '~/src/github_jm/efts'
devtools::test(efts_dir)
devtools::document(efts_dir)
devtools::build(efts_dir, vignettes = FALSE)
setwd(efts_dir)
rmarkdown::render("README.Rmd")
```

For CRAN submission checks:

```R
devtools::check(efts_dir, cran=TRUE)

devtools::build(efts_dir)
# To use the CRAN win-builder
devtools::build_win(pkg = efts_dir, version = c("R-release", "R-devel"))
```

```sh
R CMD build efts
R CMD check efts_0.7.0.tar.gz 
```

```R
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

# Troubleshooting 

A note: I need to reinstall roxygen2. Something got messed up ans xml2 failed to install. 
```
  libicui18n.so.58: cannot open shared object file: No such file or directory
```

doing full txt search with `locate` I notice that I have a `~/anaconda2/lib/libicuio.so.58`. Thanks Anaconda. 

```sh
# added by Anaconda2 installer to .bashrc
export PATH="/home/per202/anaconda2/bin:$PATH"
```
Sure enough removing that fixes things.