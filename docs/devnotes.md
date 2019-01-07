

[Using Rmd format for generating Readme.md](http://stat545.com/packages05_foofactors-package-02.html#use-readme.rmd)


```R
library(testthat)
library(devtools)
efts_dir <- '~/src/github_jm/efts'
efts_dir <- 'c:/src/github_jm/efts'
devtools::document(efts_dir)
devtools::test(efts_dir)
devtools::build(efts_dir, vignettes = FALSE)
devtools::build(efts_dir, binary=TRUE)
```

Rmarkdown will require the external prog pandoc to be installed.

```R
setwd(efts_dir)
rmarkdown::render("README.Rmd")
```

```R
library(devtools)
# efts_dir <- 'c:/src/github_jm/efts'
efts_dir <- '~/src/github_jm/efts'
devtools::load_all(efts_dir)
```

For CRAN submission checks:

```R
devtools::check(efts_dir, cran=TRUE)

devtools::check(efts_dir, document = TRUE, 
  manual = TRUE, cran = TRUE, check_version = TRUE,
  force_suggests = TRUE, run_dont_test = TRUE)

devtools::build(efts_dir)
# To use the CRAN win-builder
devtools::build_win(pkg = efts_dir, version = c("R-release", "R-devel"))

devtools::submit_cran(pkg = efts_dir)

```

```sh
R CMD build efts
R CMD check efts_0.7.0.tar.gz
R CMD INSTALL --build efts_0.9.0.tar.gz
```

```R
install.packages('X:/Staff/per202/Software/swift/tmp/efts_0.9-0.zip')
library(efts)
help('efts', help_type='html')
```

```R
timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Canberra")
(time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, time_step = "hours since", time_step_delta = 3L, tzoffset = "+1000"))

# Note that the time zone information of thes sart date is NOT 
# used by create_time_info; the tzoffset argument takes precedence 
timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Perth")
(time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, time_step = "hours since", time_step_delta = 3L, tzoffset = "+1000"))

create_time_info(from, n, time_step = "hours since", time_step_delta = 1L, tzoffset)
```

# Troubleshooting 

A note: I need to reinstall roxygen2. Something got messed up ans xml2 failed to install.

```txt
  libicui18n.so.58: cannot open shared object file: No such file or directory
```

doing full txt search with `locate` I notice that I have a `~/anaconda2/lib/libicuio.so.58`. Thanks Anaconda. 

```sh
# added by Anaconda2 installer to .bashrc
export PATH="/home/per202/anaconda2/bin:$PATH"
```

Sure enough removing that fixes things.