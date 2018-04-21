#' Check that a date-time is in the UTC time zone, and return the date time offset 'zero'
#'
#' Check that a date-time is in the UTC time zone, and return the date time offset 'zero'
#'
#' @param d an object coercible to a POSIXct
#' @export
#' @return a character, the time zone offset string '+0000'
#' @importFrom lubridate tz
#' @examples
#' start_time <- ISOdate(year=2010, month=08, day=01, hour = 12, min = 0, sec = 0, tz = 'UTC')
#' check_is_utc(d=start_time)
#'
check_is_utc <- function(d) {
  return(any(lubridate::tz(d) %in% c("UTC", "GMT")))
}

#' Create a time axis unit known to work for netCDF
#'
#' Create a time axis unit known to work for netCDF
#'
#' @param d an object coercible to a POSIXct
#' @param time_step the character prefix to put before the date, in the netCDF time axis unit definition.
#' @param tzoffset an optional character, the time offset from UTC, e.g. '+1000' for 10 hours ahead of UTC. 
#'   Can be missing, in which case it must be explicitly a UTC time. 
#'   Note that the tzoffset completely supersedes the time zone if present.
#' @export
#' @return a character, the axis units to use for the netCDF 'time' dimension
#' @examples
#' start_time <- ISOdate(year=2010, month=08, day=01, hour = 12, min = 0, sec = 0, tz = 'UTC')
#' create_netcdf_time_axis(d=start_time)
#' start_time <- ISOdate(year=2015, month=10, day=04, hour = 01, 
#'   min = 0, sec = 0, tz = 'Australia/Sydney')
#' create_netcdf_time_axis(d=start_time, tzoffset='+1000')
#'
create_netcdf_time_axis <- function(d, time_step = "hours since", tzoffset) {
  if (missing(tzoffset)) {
    if (!check_is_utc(d)) 
      stop("date time must have UTC or GMT as time zone")
    tzoffset <- "+0000"
  }
  paste(time_step, format(d, "%Y-%m-%d %H:%M:%S"), tzoffset)
}

#' Creates a EftsDataSet for access to a netCDF EFTS data set
#'
#' Creates a EftsDataSet for access to a netCDF EFTS data set
#'
#' @param ncfile name of the netCDF file, or an object of class 'ncdf4'
#' @param writein if TRUE the data set is opened in write mode
#' @export
#' @import ncdf4
#' @examples
#' library(efts)
#' ext_data <- system.file('extdata', package='efts')
#' ens_fcast_file <- file.path(ext_data, 'Upper_Murray_sample_ensemble_rain_fcast.nc')
#' stopifnot(file.exists(ens_fcast_file))
#' snc <- open_efts(ens_fcast_file)
#' (variable_names <- snc$get_variable_names())
#' (stations_varnames <- snc$get_values('station_id'))
#' nEns <- snc$get_ensemble_size()
#' nLead <- snc$get_lead_time_count()
#' td <- snc$get_time_dim()
#' stopifnot('rain_fcast_ens' %in% variable_names)
#' 
#' ens_fcast_rainfall <- snc$get_ensemble_forecasts('rain_fcast_ens',
#'   stations_varnames[1], start_time=td[2])
#' names(ens_fcast_rainfall) <- as.character(1:ncol(ens_fcast_rainfall))
#' plot(ens_fcast_rainfall, legend.loc='right')
#' 
#' snc$close()
#' 
#' @return A EftsDataSet object
#' @importFrom methods is
open_efts <- function(ncfile, writein = FALSE) {
  if (is.character(ncfile)) {
    nc <- ncdf4::nc_open(ncfile, readunlim = FALSE, write = writein)
  } else if (methods::is(ncfile, "ncdf4")) {
    nc <- ncfile
  }
  EftsDataSet$new(nc)
}

#' Helper function to create the definition of the time dimension for use in a netCDF file
#'
#' Helper function to create the definition of the time dimension for use in a netCDF file. Defaults to create an hourly axis.
#'
#' @param from the start date of the time axis
#' @param n length of the time dimension
#' @param time_step unit prefix in the time dimension units
#' @param time_step_delta integer, length of time units between each steps
#' @param tzoffset an optional character, the time offset from UTC, e.g. '+1000' for 10 hours ahead of UTC. Can be missing, in which case 'from' must be explicitly a UTC time. Note that the tzoffset completely supersedes the time zone if present.
#' @import ncdf4
#' @export
#' @return A list with keys units and values
#' @seealso See
#'    \code{\link{create_efts}} for examples
#' @examples
#' timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Canberra")
#' (time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, 
#'   time_step = "hours since", time_step_delta = 3L, tzoffset = "+1000"))
#' 
#' # Note that the time zone information of thes sart date is NOT 
#' # used by create_time_info; the tzoffset argument takes precedence 
#' timeAxisStart <- ISOdate(2015, 10, 4, 0, 0, 0, tz = "Australia/Perth")
#' (time_dim_info <- create_time_info(from = timeAxisStart, n = 24L, 
#'   time_step = "hours since", time_step_delta = 3L, tzoffset = "+1000"))
#' 
create_time_info <- function(from, n, time_step = "hours since", time_step_delta = 1L, 
  tzoffset) {
  list(units = create_netcdf_time_axis(d = from, time_step = time_step, tzoffset = tzoffset), 
    values = (0:(n - 1)) * time_step_delta)
}

#' Create variable attribute definition
#'
#' Create variable attribute definition
#'
#' @param type A data type identifier, as a coded description.
#' @param type_description description of this data type identifier.
#' @param location_type a character, type of location, e.g. 'Point'
#' @export
#' @return a list of attributes, describing the type of variable stored
#' @examples
#' va <- create_var_attribute_definition(type=2L, 
#'   type_description='accumulated over the preceding interval', location_type='Point')
#' vdef <- create_variable_definition(name='rain_fcast_ens', 
#'   longname='Rainfall ensemble forecast derived from some prediction', 
#'   units='mm', missval=-9999.0, precision='double', 
#'   var_attribute=va)
#'
create_var_attribute_definition <- function(type = 2L, type_description = "accumulated over the preceding interval", 
  location_type = "Point") {
  list(type = type, type_description = type_description, location_type = location_type)
}

#' Create a variable definition
#'
#' Create a variable definition usable by the function \code{\link{create_efts_variables}} to create netCDF variables.
#'
#' @param name variable name
#' @param longname variable long name
#' @param units variable units
#' @param missval value code for missing data
#' @param precision precision
#' @param dim_type dimension type (EFTS integer code)
#' @param var_attribute list of attributes for the netCDF variable to create
#' @export
#' @return a list 
#' @examples
#' \dontrun{
#' create_variable_definition(name='rain_fcast_ens', 
#    longname='Rainfall ensemble forecast derived from some prediction', units='mm', 
#    missval=-9999.0, precision='double', var_attribute=list(type=2L))
#' }
create_variable_definition <- function(name, longname = "", units = "mm", missval = -9999, 
  precision = "double", dim_type = "4", var_attribute = create_var_attribute_definition()) {
  if (missing(name)) 
    stop("name argument must be present")
  if (is.na(name)) 
    stop("name argument must not be NA")
  list(name = name, longname = longname, units = units, dim_type = dim_type, 
    missval = missval, precision = precision, attributes = var_attribute)
}

#' Create a variables definition data frame
#'
#' Create a variable definition usable by the function \code{\link{create_variable_definitions}} 
#' to create netCDF variables. The use of this function is not compulsory to create a EFTS 
#' netCDF schema, just offered as a convenience.
#'
#' @param variable_names character vector, names of the variables
#' @param long_names character vector, long names of the variables (defaults to variable_names if missing)
#' @param standard_names character vector, standard names of the variables (optional, defaults to variable_names)
#' @param units character vector, units for the variable(s)
#' @param missval numeric vector, missing value code(s) for the variable(s)
#' @param precision character vector, precision of the variables
#' @param dimensions character or integer vector, number of dimensions each variable (2, 3 or 4)
#' @param type A data type identifier, as a coded description.
#' @param type_description description of this data type identifier.
#' @param location_type a character, type of location, e.g. 'Point'
#' @export
#' @return a data frame suitable for \code{\link{create_variable_definition}}
#' @seealso See
#'    \code{\link{create_variable_definition}} and \code{\link{create_efts}} for examples
create_variable_definition_dataframe <- function(variable_names, long_names = variable_names, standard_names = variable_names, units = "mm", missval = -9999, 
  precision = "double", dimensions = 4L, type = 2L, type_description = "accumulated over the preceding interval", 
  location_type = "Point") {
  stopifnot(is.character(variable_names))
  varsDef <- data.frame(name = variable_names, stringsAsFactors = FALSE)
  varsDef$longname <- long_names
  varsDef$standard_name <- standard_names
  varsDef$units <- units
  varsDef$missval <- missval
  varsDef$precision <- precision
  varsDef$dimensions <- as.integer(dimensions)
  varsDef$type <- type
  varsDef$type_description <- type_description
  varsDef$location_type <- location_type
  return(varsDef)
}

#' Create variable definitions from a data frame
#'
#' Given a data frame as input, create a list of variable definitions usable by the function \code{\link{create_efts_variables}} to create netCDF variables.
#'
#' @param dframe a data frame, one line is one variable definition. Must have at least the following column names: 'name', 'longname', 'units', 'missval', 'precision', 'type', 'type_description', 'location_type'
#' @export
#' @return a list of length equal to the number of rows in the input data frame
#' @seealso See
#'    \code{\link{create_efts}} for examples
#' @examples
#' varsDef = data.frame(name=letters[1:3], stringsAsFactors=FALSE)
#' varsDef$longname=paste('long name for', varsDef$name)
#' varsDef$units='mm'
#' varsDef$missval=-999.0
#' varsDef$precision='double'
#' varsDef$type=2
#' varsDef$type_description='accumulated over the previous time step'
#' varsDef$location_type='Point'
#' str(create_variable_definitions(varsDef))
#'
create_variable_definitions <- function(dframe) {
  dframe$rownum <- 1:nrow(dframe)
  f <- function(varDef) {
    create_variable_definition(name = varDef[["name"]], longname = varDef[["longname"]], 
      units = varDef[["units"]], missval = varDef[["missval"]], precision = varDef[["precision"]], 
      dim_type = varDef[["dimensions"]], var_attribute = create_var_attribute_definition(type = varDef[["type"]], 
        type_description = varDef[["type_description"]], location_type = varDef[["location_type"]]))
  }  # f
  plyr::dlply(.data = dframe, .variables = "rownum", .fun = f)
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
#' see \link{https://github.com/jmp75/efts/blob/master/docs/netcdf_for_water_forecasting.md#global-attributes}.
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

#' Creates a EftsDataSet for write access to a netCDF EFTS data set
#'
#' Creates a EftsDataSet for write access to a netCDF EFTS data set
#'
#' @param fname file name to create to. The file must not exist already.
#' @param time_dim_info a list with the units and values defining the time dimension of the data set
#' @param data_var_definitions a data frame, acceptable by \code{\link{create_variable_definitions}}, or list of netCDF variable definitions, e.g. 
#'       \code{list(rain_fcast_ens=list(name='rain_fcast_ens', longname='ECMWF Rainfall ensemble forecasts', units='mm', missval=-9999.0, precision='double', attributes=list(type=2, type_description='accumulated over the preceding interval')))}
#' @param stations_varnames station identifiers, coercible to an integer vector (note: may change to be a more flexible character storage)
#' @param station_names optional; names of the stations
#' @param nc_attributes a named list of characters, attributes for the whole file.
#' @param lead_length length of the lead forecasting time series.
#' @param ensemble_length number of ensembles, i.e. number of forecasts for each point on the main time axis of the data set
#' @examples
#'
#' fname <- tempfile()
#' stations_varnames <- c(123,456)
#' nEns <- 3
#' nLead <- 4
#' nTimeSteps <- 12
#' 
#' timeAxisStart <- ISOdate(year=2010, month=08, day=01, hour = 14, min = 0, sec = 0, tz = 'UTC')
#' time_dim_info <- create_time_info(from=timeAxisStart, n=nTimeSteps)
#' 
#' # It is possible to define variables for three combinations of dimensions.
#' # dimensions '4' ==> [lead_time,station,ens_member,time]
#' # dimensions '3' ==> [station,ens_member,time]   
#' # dimensions '2' ==> [station,time]   
#' 
#' variable_names <- c('var1_fcast_ens','var2_fcast_ens', 'var1_obs', 
#'   'var2_obs', 'var1_ens','var2_ens')
#' varDef <- create_variable_definition_dataframe(variable_names=variable_names, 
#'   long_names = paste(variable_names, 'synthetic data'))
#' varDef$dimensions <- c('4','4','2','2','3','3')
#' 
#' glob_attr <- create_global_attributes(
#'   title="data set title", 
#'   institution="my org", 
#'   catchment="Upper_Murray", 
#'   source="A journal reference, URL", 
#'   comment="example for vignette")
#' 
#' snc <- create_efts(fname, time_dim_info, varDef, stations_varnames, 
#'   nc_attributes=glob_attr, lead_length=nLead, ensemble_length=nEns)
#' 
#' # Following is code that was used to create unit tests for EFTS.
#' # This is kept in this example to provide sample on now to write data of various dimension.
#' td <- snc$get_time_dim()
#' m <- matrix(ncol=nEns, nrow=nLead)
#' for (rnum in 1:nLead) {
#'     for (cnum in 1:nEns) {
#'       m[rnum,cnum] = rnum*0.01 + cnum*0.1
#'   }
#' }
#' #      [,1] [,2] [,3]
#' # [1,] 0.11 0.21 0.31
#' # [2,] 0.12 0.22 0.32
#' # [3,] 0.13 0.23 0.33
#' # [4,] 0.14 0.24 0.34
#' for (i in 1:length(td)) {
#'   for (j in 1:length(stations_varnames)) {
#'     station <- stations_varnames[j]
#'     var1Values <- i + 0.1*j + m
#'     var2Values <- 2*var1Values
#'     dtime = td[i]
#'     snc$put_ensemble_forecasts(var1Values, variable_name = variable_names[1], 
#'        identifier = station, start_time = dtime)
#'     snc$put_ensemble_forecasts(var2Values, variable_name = variable_names[2], 
#'        identifier = station, start_time = dtime)
#'   }
#' }
#' 
#' timeSteps <- 1:length(td)
#' for (j in 1:length(stations_varnames)) {
#'   var3Values <- timeSteps + 0.1*j
#'   var4Values <- var3Values + 0.01*timeSteps + 0.001*j
#' 
#'   station <- stations_varnames[j]
#'   snc$put_single_series(var3Values, variable_name = variable_names[3], identifier = station)
#'   snc$put_single_series(var4Values, variable_name = variable_names[4], identifier = station)
#' }
#' 
#' for (j in 1:length(stations_varnames)) {
#' 
#'   var5Xts <- matrix(rep(1:nEns, each=nTimeSteps) + timeSteps + 0.1*j, ncol=nEns)
#' 
#'   # [time,ens_member] to [ens_member,time], as expected by put_ensemble_series
#'   var5Values <- t(var5Xts) 
#'   var6Values <- 0.25 * var5Values
#' 
#'   station <- stations_varnames[j]
#'   snc$put_ensemble_series(var5Values, variable_name = variable_names[5], identifier = station)
#'   snc$put_ensemble_series(var6Values, variable_name = variable_names[6], identifier = station)
#' }
#' snc$close()
#' # Cleaning up temp file:
#' if (file.exists(fname)) 
#'   file.remove(fname)
#'
#' @export
#' @import ncdf4
#' @importFrom utils packageDescription
#' @importFrom methods new
#' @return A EftsDataSet object
create_efts <- function(fname, time_dim_info, data_var_definitions, stations_varnames, station_names, 
  nc_attributes, optional_vars, lead_length = 48, ensemble_length = 50, lead_time_tstep = "hours") {
  
  if (missing(stations_varnames)) {
    stop("You must provide station identifiers when creating a new EFTS netCDF data set")
  }
  
  if (file.exists(fname)) 
    stop(paste("File already exists:", fname))
  
  if (is.data.frame(data_var_definitions)) 
    data_var_definitions <- create_variable_definitions(data_var_definitions)
    
  varDefs <- create_efts_variables(data_var_definitions, 
                                    time_dim_info, 
                                    num_stations = length(stations_varnames), 
                                    lead_length = lead_length, 
                                    ensemble_length = ensemble_length,
                                    lead_time_tstep = lead_time_tstep)
  
  allVars <- c(varDefs$datavars, varDefs$metadatavars)

  nc <- ncdf4::nc_create(fname, vars = allVars)
  
  hasStationNames <- !missing(station_names)
  
  infoList <- function(theList) {
    paste(paste(names(theList), theList, sep = ": "), collapse = ", ")
  }
  
  createSchema <- function() {
    lapply(data_var_definitions, put_variable_attributes, nc)
    ncdf4::ncatt_put(nc, time_dim_name, "standard_name", time_dim_name)
    ncdf4::ncatt_put(nc, time_dim_name, "time_standard", "UTC")
    ncdf4::ncatt_put(nc, time_dim_name, "axis", "t")
    ncdf4::ncatt_put(nc, "ens_member", "standard_name", "ens_member")
    ncdf4::ncatt_put(nc, "ens_member", "axis", "u")
    ncdf4::ncatt_put(nc, lead_time_dim_name, "standard_name", "lead_time")
    ncdf4::ncatt_put(nc, lead_time_dim_name, "axis", "v")
    ncdf4::ncatt_put(nc, "lat", "axis", "y")
    ncdf4::ncatt_put(nc, "lon", "axis", "x")
    
    var_names <- names(allVars)
    if("x" %in% var_names){
      ncdf4::ncatt_put(nc, "x", "axis", "x")
    }
    if("y" %in% var_names){
      ncdf4::ncatt_put(nc, "y", "axis", "y")
    }

    ncdf4::ncatt_put(nc, 0, "STF_convention_version", 2)
    ncdf4::ncatt_put(nc, 0, "STF_nc_spec", "https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md")
    ncdf4::ncatt_put(nc, 0, "history", 
      paste( 
        as.character(lubridate::now(tzone="UTC")),
        "UTC", 
        "file created with the R package efts", packageDescription("efts")$Version 
      ) %>% infoList)
    
    for (k in names(nc_attributes)) {
      pad_global_attribute(nc, k, nc_attributes[k])
    }

    ncdf4::ncvar_put(nc, "station_id", stations_varnames)
    ncdf4::ncvar_put(nc, lead_time_dim_name, 0:(lead_length - 1))
    ncdf4::ncvar_put(nc, "ens_member", 1:ensemble_length)
    if (hasStationNames) {
      ncdf4::ncvar_put(nc, "station_name", station_names)
    }
    # One seems to need to close/reopen the newly created file, otherwise some
    # ncvar_get operations will fail with a cryptic message.  I follow the
    # advice in this and associated posts
    # https://www.unidata.ucar.edu/mailing_lists/archives/netcdfgroup/2012/msg00270.html
    ncdf4::nc_close(nc)
    nc <- ncdf4::nc_open(fname, write = TRUE, readunlim = FALSE)
    EftsDataSet$new(nc)
  }
  
  result <- tryCatch(createSchema(), error = function(e) {
    stop(paste("netCDF schema creation failed", e))
    NULL
  }, finally = function() {
  })
  result
}

#' Retrieves the first date of the time dimension from a netCDF file
#'
#' Retrieves the first date of the time dimension from a netCDF file of daily data, given the units found in the netCDF attribute for the time dimension
#'
#' @param time_units The string description of the units of the time dimension e.g. 'days since 1980-01-01' or 'hours since 2010-08-01 13:00:00 +0000'
#' @param time_zone the time zone to use for the returned value.
#' @export
#' @importFrom udunits2 ud.convert
#' @import lubridate
#' @return A POSIXct object, origin of the time dimension as defined
#' @examples
#'
#' x <- "hours since 2015-10-04 00:00:00 +1023"
#' get_start_date(x)
#' get_start_date(x,time_zone = 'UTC')
#' get_start_date(x,time_zone = 'Australia/Perth')
#' get_start_date(x,time_zone = 'Australia/Canberra')
#'
get_start_date <- function(time_units, time_zone = "UTC") {
  refDate <- lubridate::origin  # 
  class(refDate) <- c("POSIXct", "POSIXt")  # workaround what I think is a lubridate bug; try origin + days(1)  and its effect, visibly because of class ordering on origin.
  isDaily <- is_daily_time_step(time_units)
  refDateUnits <- paste(ifelse(isDaily, "days", "hours"), "since 1970-01-01 00:00:00 +0000")
  offsetSinceRef <- udunits2::ud.convert(0, time_units, refDateUnits)
  offsetFun <- get_time_step_function(time_units)
  startDateUtc <- refDate + offsetFun(offsetSinceRef)
  startDate <- lubridate::with_tz(startDateUtc, time_zone)
  return(startDate)
}


#' Retrieves the unit string of the time dimension from a netCDF file
#'
#' @export
#' @param ncfile an object of class ncdf4
#' @param time_dim_name The name of the time dimension, by default 'time' as per the CF conventions.
#' @return a character
get_time_units <- function(ncfile, time_dim_name = "time") {
  return(ncdf4::ncatt_get(ncfile, time_dim_name, "units")$value)
}

#' Retrieves the time dimension from a netCDF file
#'
#' @export
#' @param ncfile an object of class ncdf4
#' @param time_dim_name The name of the time dimension, by default 'time' as per the CF conventions.
#' @param time_zone the time zone to use for the returned value.
#' @return A vector of Dates
get_time_dimension <- function(ncfile, time_dim_name = "time", time_zone = "UTC") {
  time_units <- get_time_units(ncfile, time_dim_name)
  timeValues <- ncdf4::ncvar_get(ncfile, time_dim_name)
  startDate <- get_start_date(time_units, time_zone = time_zone)
  offsetFun <- get_time_step_function(time_units)
  startDate + offsetFun(timeValues)
}

#' @importFrom stringr str_sub
offset_as_duration <- function(delta) {
  h <- stringr::str_sub(delta, 1L, 2L)  # 10
  m <- stringr::str_sub(delta, 3L, 4L)  # 30
  return((lubridate::dhours(as.integer(h)) + lubridate::dminutes(as.integer(m))))
}

#' @importFrom stringr str_sub
offset_as_difftime <- function(delta) {
  h <- stringr::str_sub(delta, 1L, 2L)  # 10
  m <- stringr::str_sub(delta, 3L, 4L)  # 30
  b <- lubridate::origin + lubridate::dhours(as.integer(h)) + lubridate::dminutes(as.integer(m))
  b - lubridate::origin
}

#' Finds the UTC offset in a date-time string
#'
#' Finds the UTC offset in a date-time or time axis specification string 
#'  such as 'hours since 2015-10-04 00:00:00 +1030'
#'
#' @param time_units the string to process
#' @param as_string a boolean. If true, return the time offset as a character, otherwise return a difftime object.
#' @return the time offset as a character, or as a difftime object.
#' @export
#' @examples
#'
#' x <- "hours since 2015-10-04 00:00:00 +1023"
#' find_utc_offset(x)
#' find_utc_offset(x, FALSE)
#' x <- "hours since 2015-10-04 00:00:00 -0837"
#' find_utc_offset(x)
#' find_utc_offset(x, FALSE)    
#' x <- "hours since 2015-10-04 00:00:00"
#' find_utc_offset(x)
#' find_utc_offset(x, FALSE)
#'
find_utc_offset <- function(time_units, as_string = TRUE) {
  # TODO: there may be a smarter way using udunits to determine the offset,
  # but not trivial either.
  x <- stringr::str_split(time_units, "\\+")[[1]]
  # [1] 'hours since 2015-10-04 00:00:00 ' '1030' the offset would have been
  # with a positive sign: +1030
  if (length(x) > 1) {
    delta <- last(x)  # 1030
    if (as_string) {
      return(paste0("+", delta))
    } else {
      return(+offset_as_difftime(delta))
    }
  }
  x <- stringr::str_split(time_units, "[\\-]")[[1]]
  # [1] 'hours since 2015' '10' '04 00:00:00 ' '1030' the offset would have
  # been with a negative sign: -1030
  if (length(x) == 4) {
    delta <- last(x)  # 1030
    if (as_string) {
      return(paste0("-", delta))
    } else {
      return(-offset_as_difftime(delta))
    }
  } else {
    # length(x) < 4 : no offset detected
    if (as_string) {
      return("")
    } else {
      return(lubridate::origin - lubridate::origin)
    }
  }
}

default_optional_vardefs_v2 <- function() {
  varsDef <- data.frame(name = c("x","y","area","elevation"), stringsAsFactors = FALSE)
  varsDef$longname <- c(
    "easting from the GDA94 datum in MGA Zone 55",
    "northing from the GDA94 datum in MGA Zone 55",
    "catchment area",
    "station elevation above sea level"
  )
  varsDef$standard_name <- c(
    "northing_GDA94_zone55",
    "easting_GDA94_zone55",
    "area",
    "elevation",
    )
  varsDef$units <- c("","","km^2","m")
  varsDef$missval <- c(NA,NA,-9999,-9999)
  varsDef$precision <- "float"
  return(varsDef)
}
