#' Accessing ensemble forecast time series (EFTS) data set stored in netCDF formats
#' 
#' \tabular{ll}{
#' Package: \tab efts\cr
#' Type: \tab Package\cr
#' Version: \tab 0.9-0\cr
#' Date: \tab 2018-04-22\cr
#' Release Notes: \tab Facilities and stricter checks for comliance with netCDF EFTS conventions. Fixes for addressing feedback from previous CRAN submission. \cr
#' License: \tab GPL-2 \cr
#' }
#'
#' Accessing ensemble forecast time series (EFTS) data set stored in netCDF formats, without the need for lower-level ncdf4 operations.
#' See \code{\link{open_efts}} \code{\link{create_efts}} for code examples to read/write files using this package.
#' 
#' This work was carried out in the CSIRO Water for Healthy Country National Research Flagship and was 
#' supported by the Water Information Research and Development Alliance between CSIRO and the Australian 
#' Bureau of Meteorology. 
#' 
#'
#' \tabular{lll}{
#' Version \tab Date \tab Notes \cr
#' 0.8.0 \tab 2018-04-08 \tab Improve and test subsetting and reordering of multidimensional arrays retrieved via ncdf4. Remove issues in 'R CMD check' for first CRAN submission \cr
#' 0.7.0 \tab 2018-02-14 \tab Initial commit to github \cr
#' 0.6.x \tab 2017 \tab Non public releases of a package originally used only for unit test purposes \cr
#' }
#'
#' @name efts-package
#' @aliases efts
#' @docType package
#' @title Accessing ensemble forecast time series (EFTS) data set stored in netCDF formats
#' @author Jean-Michel Perraud \email{jean-michel.perraud_at_csiro.au}
#' @keywords package netCDF ensemble forecast time series 
NULL 
