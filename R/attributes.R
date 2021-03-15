#' Create variable attribute definition
#'
#' Create variable attribute definition
#'
#' @param type A data type identifier, as a coded description.
#' @param type_description description of this data type identifier.
#' @param location_type a character, type of location, e.g. 'Point'
#' @param dat_type a character, the type of data stored in this variable
#' @param dat_type_description a character, human readable description of the data stored in this variable
#' @export
#' @return a list of attributes, describing the type of variable stored
#' @examples
#' va <- create_var_attribute_definition(type=2L, 
#'   type_description='accumulated over the preceding interval', location_type='Point')
#' vdef <- create_variable_definition(name='rain_sim', 
#'   longname='Rainfall ensemble forecast derived from some prediction', 
#'   units='mm', missval=-9999.0, precision='double', 
#'   var_attribute=va)
#'
create_var_attribute_definition <- function(
  type = 2L, 
  type_description = "accumulated over the preceding interval", 
  dat_type = "der", 
  dat_type_description = "AWAP data interpolated from observations",
  location_type = "Point") {
  list(
    type = type, 
    type_description = type_description, 
    dat_type = dat_type,
    dat_type_description = dat_type_description,
    location_type = location_type)
}

# The following cannot be hard-coded.  ncdf4::ncatt_put(nc,0,'institution',
# 'CSIRO Land and Water') ncdf4::ncatt_put(nc,0,'comment', '')
# ncdf4::ncatt_put(nc,0,'source', '') catchment <- paste(letters[1:9],
# collapse='') ncdf4::ncatt_put(nc,0,'Catchment', catchment)
# ncdf4::ncatt_put(nc,0,'title', paste('Rainfall Observations for',
# catchment))

#' Add a value to a global attribute of a netCDF file
#'
#' Add a value to a global attribute of a netCDF file
#'
#' @param nc an object 'ncdf4'
#' @param attribute_name the name of the global attribute to add to
#' @param attribute_value the value to pad
#' @param sep separator to add between the existing value and the padded value.
#' @export
#' @import ncdf4
pad_global_attribute <- function(nc, attribute_name, attribute_value, sep = "\n") {
  attVal <- ""
  a <- ncdf4::ncatt_get(nc, 0, attribute_name)
  if (a$hasatt) {
    attVal <- paste(a$value, sep)
    attVal <- paste(attVal, attribute_value)
  } else {
    attVal <- attribute_value
  }
  ncdf4::ncatt_put(nc, 0, attribute_name, as.character(attVal))
}

#' Define a set of global attributes for netCDF files.
#'
#' The conventions require a set of global attributes to be present, 
#' see \url{https://github.com/csiro-hydroinformatics/efts/blob/master/docs/netcdf_for_water_forecasting.md#global-attributes}.
#' This function is recommended to define these attributes.
#'
#' @param title text, a succinct description of what is in the dataset
#' @param institution text, Where the original data was produced
#' @param source text, published or web-based references that describe the data or methods used to produce it
#' @param catchment text, the catchment for which the data is created. White spaces are replaced with underscores
#' @param comment text, miscellaneous information
#' @param strict logical, if true perform extra checks on the input information
#' @export
#' @importFrom stringr str_replace_all
create_global_attributes <- function(title, institution, source, catchment, comment, strict = FALSE) {
  title <- as.character(title)
  institution <- as.character(institution)
  source <- as.character(source)
  catchment <- as.character(catchment)
  comment <- as.character(comment)

  # catchment info should not have white spaces
  # catchment <- 'Upper  Murray River '
  catchment <- stringr::str_replace_all(catchment, pattern='\\s+', '_')

  if(strict) {
    if(title == "") stop("Empty title is not accepted as a valid attribute")
  }

  list(
    title = title,
    institution = institution,
    source = source,
    catchment = catchment,
    comment = comment
  )
}

########################################
# Below are functions not exported
########################################

check_global_attributes <- function(nc_attributes)
{
  stopifnot(is.list(nc_attributes))
  expected <- mandatory_global_attributes
  present_attr <- expected %in% names(nc_attributes)
  missing_attr <- which(!present_attr)
  if(length(missing_attr) > 0) stop(paste("missing global attributes: ",paste(expected[missing_attr], collapse=","), sep=" "))
}

put_variable_attributes <- function(data_var_def, nc) {
  a <- data_var_def
  stopifnot("name" %in% names(a))
  varname <- a[["name"]]
  if ("attributes" %in% names(a)) {
    attribs <- a[["attributes"]]
    for (attribute_name in names(attribs)) {
      ncdf4::ncatt_put(nc, varname, attribute_name, attribs[[attribute_name]])
    }
  }
}
