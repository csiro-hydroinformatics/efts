Version 0.9.2
=============

### Fixes

Fixing the upcoming problem: The 'rmarkdown' package should be declared as a dependency of the
'validate' package (e.g., in the 'Suggests' field of DESCRIPTION),
because the latter contains vignette(s) built with the 'rmarkdown'
package. Please see https://github.com/yihui/knitr/issues/1864 for
more information.

Version 0.9.1
=============

### Fixes

* [Workaround a possible change of behavior in lubridate and/or udunits that cause issues with date time arithmetics. Led to incorrect time dimension, offset by a second compared to user specifications.](https://github.com/csiro-hydroinformatics/efts/issues/3)
* [Remove assumption that lead_time dimension is in hours.](https://github.com/csiro-hydroinformatics/efts/issues/6)

### Improvements

* [Make it easier to specify the lead time axis](https://github.com/csiro-hydroinformatics/efts/issues/7)

Version 0.9.0
=============

### Improvements

* Fixes based on CRAN reviewer feedback, for second submission to CRAN
* Add helper template functions for optional metadata variable and mandatory file attributes 
* Related to above, revised key functions signatures for creating datasets
* Example code in documentation is not marked as 'dontrun'
* Add stricter checks for mandatory variables and attributes 
* Add stricter checks for direct get_values and put_values 

Version 0.8.0
=============

### NEW FEATURES

* First submission to CRAN

