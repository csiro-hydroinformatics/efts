

http://stat545.com/packages05_foofactors-package-02.html#use-readme.rmd


```R
library(devtools)
devtools::document('~/src/github_jm/efts')
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
help('efts', help_type='html')
```