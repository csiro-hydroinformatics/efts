
# https://stackoverflow.com/a/17419100  otherwise cannot use setRefClass...
setOldClass("ncdf4")


#' Reference class convenient for access to a netCDF file.
#'
#' Reference class convenient for access to a netCDF file. 
#' Note for internal implementation that ncdf4 objects are 
#' basically lists with a class attribute. This class [NetCdfDataSet-class] 
#' is used as a parent class to the [EftsDataSet-class] class 
#' but may be reused and expanded for other types of netCDF data. 
#'
#' @name NetCdfDataSet-class
#' @rdname NetCdfDataSet-class
#' @field ncfile an object of class ncdf4
#' @export
#' @exportClass NetCdfDataSet
NetCdfDataSet <- setRefClass("NetCdfDataSet", fields = list(ncfile="ncdf4"), methods = list(initialize = function(nc = NULL) {
  "Create an object wrapping an ncdf4 object"
  if (!is.null(nc)) stopifnot("ncdf4" %in% class(nc))
  ncfile <<- nc
}, show = function() {
  "Method for automatically printing ncdf4 objects"
  print(ncfile)
}))

#' Reference class convenient for access to a Ensemble Forecast Time Series in netCDF file.
#'
#' Reference class convenient for access to a Ensemble Forecast Time Series in netCDF file.
#'
#' @import xts
#' @name EftsDataSet-class
#' @rdname EftsDataSet-class
#' @export
#' @exportClass EftsDataSet
#' @field time_dim a cached POSIXct vector, the values for the time dimension of the data set.
#' @field time_zone the time zone for the time dimensions of this data set.
#' @field identifiers_dimensions a cache, list of values of the primary data identifiers; e.g. station_name or station_id
#' @field stations_varname name of the variable that stores the names of the stations for this data set.
#' @seealso See
#'    \code{\link{create_efts}} and \code{\link{open_efts}} for examples on how to read or write EFTS netCDF files using this dataset.
EftsDataSet <- setRefClass("EftsDataSet", contains = "NetCdfDataSet", fields = list(time_dim = "POSIXct", 
  time_zone = "character", identifiers_dimensions = "list", stations_varname = "character", 
  stations_dim_name = "character", lead_time_dim_name = "character", 
  ensemble_member_dim_name = "character"), methods = list(initialize = function(nc = NULL) {
  callSuper(nc)
  time_dim <<- as.POSIXct(NA)
  time_zone <<- "UTC"
  stations_dim_name <<- "station"
  stations_varname <<- "station_id"
  lead_time_dim_name <<- "lead_time"
  ensemble_member_dim_name <<- "ens_member"
  identifiers_dimensions <<- list()
}, set_time_zone = function(tzone_id) {
  "Sets the time zone to use for the read time series"
  time_zone <<- tzone_id
  time_dim <<- as.POSIXct(NA)
}, get_time_zone = function() {
  "Gets the time zone to use for the read time series"
  return(time_zone)
}, get_utc_offset = function(as_string = TRUE) {
  "Gets the time zone to use for the read time series, i.e. \"hours since 2015-10-04 00:00:00 +1030\". Returns the string \"+1030\" or \"-0845\" if as_string is TRUE, or a lubridate Duration object if FALSE"
  time_units <- get_time_units(ncfile, time_dim_name = "time")
  find_utc_offset(time_units, as_string)
}, get_time_unit = function() {
  "Gets the time units of a read time series, i.e. \"hours since 2015-10-04 00:00:00 +1030\". Returns the string \"hours\""
  inTimeUnits <- get_time_units(ncfile, time_dim_name = "time")
  tu <- unlist(strsplit(inTimeUnits, " "))[1]
  return(tu)
}, get_lead_time_unit = function() {
  "Gets the time units of a read time series, i.e. \"hours since forecast time\". Returns the string \"hours\""
  inTimeUnits <- get_time_units(ncfile, time_dim_name = "lead_time")
  tu <- unlist(strsplit(inTimeUnits, " "))[1]
  return(tu)
}, get_variable_names = function() {
  "Gets the name of all variables in the data set"
  return(names(ncfile$var))
}, get_dim_names = function() {
  "Gets the name of all dimensions in the data set"
  return(names(ncfile$dim))
}, get_stations_varname = function() {
  "Gets the name of the variable that has the station identifiers"
  return(stations_varname)
}, get_variable_dim_names = function(variable_name) {
  "Gets the names of the dimensions that define the geometry of a given variable"
  stopifnot(variable_name %in% names(ncfile$var))
  dims <- ncfile$var[[variable_name]]$dim
  f <- function(x) {
    x$name
  }
  return(sapply(dims, f))
},  add_variable = function(varDef) {
  "Adds a variable to the netcdf file"
  stopifnot(!(varDef$name %in% names(ncfile$var)))
  station_dim <- ncfile$dim[['station']]
  time_dim <- ncfile$dim[['time']]
  ensemble_dim <- ncfile$dim[['ens_member']]
  lead_time_dim <- ncfile$dim[['lead_time']]
  
  dimensions <- switch(as.numeric(varDef$dim_type), list(station_dim),  
	list(station_dim, time_dim),
	list(station_dim, ensemble_dim, time_dim),
	list(lead_time_dim,station_dim, ensemble_dim, time_dim))

  vDef <- create_data_variable(varDef, dimensions) 
  ncdf4::ncvar_add(ncfile,vDef)
  
  ncdf4::nc_enddef(ncfile)
 # ncdf4::nc_close(ncfile)
 # ncfile <- ncdf4::nc_open(fname, write = TRUE, readunlim = FALSE)
  return()
}, get_ensemble_forecasts = function(variable_name = "rain_sim", identifier, dimension_id = get_stations_varname(), 
  start_time = NA, lead_time_count = NA) {
  "Return a time series, ensemble of forecasts over the lead time"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  if (is.na(start_time)) start_time <- td[1]
  nEns <- get_ensemble_size()
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  lead_time_count <- ifelse(is.na(lead_time_count), get_lead_time_count(), lead_time_count[1])
  indTime <- index_for_time(start_time)
  # float rain_sim[lead_time,station,ens_member,time]
  ensData <- ncdf4::ncvar_get(ncfile, variable_name, start = c(1, index_id, 1, indTime), 
  count = c(lead_time_count, 1, nEns, 1), collapse_degen = FALSE)
  tu <- get_lead_time_unit()
  if (tu == "days"){
    timeAxis <- start_time + lubridate::ddays(1) * ncfile$dim$lead_time$vals
  } else {
    timeAxis <- start_time + lubridate::dhours(1) * ncfile$dim$lead_time$vals
  }
  out <- xts(x = ensData[, 1, , 1], order.by = timeAxis, tzone = tz(start_time))
  return(out)
}, get_ensemble_for_stations = function(variable_name = "rain_sim", identifier, dimension_id = "ens_member", 
  start_time = NA, lead_time_count = NA) {
  "Return a time series, representing a single ensemble member forecast for all stations over the lead time"
  # TODO revise this function. Unclear what the intent and context was
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  if (is.na(start_time)) start_time <- td[1]
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  lead_time_count <- ifelse(is.na(lead_time_count), get_lead_time_count(), lead_time_count[1])
  stationCount <- get_station_count()
  indTime <- index_for_time(start_time)
  # float rain_sim[lead_time,station,ens_member,time]
  ensData <- ncdf4::ncvar_get(ncfile, variable_name, start = c(1, 1, index_id, indTime), 
    count = c(lead_time_count, stationCount, 1, 1), collapse_degen = FALSE)
  tu <- get_lead_time_unit()
  if (tu == "days"){
    timeAxis <- start_time + lubridate::ddays(1) * ncfile$dim$lead_time$vals
  } else {
    timeAxis <- start_time + lubridate::dhours(1) * ncfile$dim$lead_time$vals
  }
  colnam <- get_values(station_id_varname)
  out <- xts(x = ensData[, , 1, 1], order.by = timeAxis, tzone = tz(start_time))
  names(out) <- colnam
  return(out)
}, get_ensemble_forecasts_for_station = function(variable_name = "rain_sim", identifier, dimension_id = get_stations_varname()) {
  "Return an array, representing all ensemble member forecasts for a single stations over all lead times"
  td <- get_time_dim()
  start_time <- td[1]
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  # float rain_sim[lead_time,station,ens_member,time]
  ensData <- ncdf4::ncvar_get(ncfile, variable_name, start = c(1, index_id, 1, 1), 
    count = c(-1, 1, -1, -1), collapse_degen = TRUE)
  return(ensData)
}, get_single_series = function(variable_name = "rain_obs", identifier, dimension_id = get_stations_varname()) {
  "Return a single point time series for a station identifier. Falls back on get_all_series if the argument \"identifier\" is missing"
  stopifnot(variable_name %in% names(ncfile$var))
  if (missing(identifier)) return(get_all_series(variable_name, dimension_id))
  td <- get_time_dim()
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  ncdims <- get_variable_dim_names(variable_name)
  # could be e.g.: double q_obs[lead_time,station,ens_member,time] float
  # rain_obs[station,time] lead_time,station,ens_member,time reordered
  # according to the variable present dimensions:
  tsstart <- splice_named_var(c(1, index_id, 1, 1), ncdims)
  tscount <- splice_named_var(c(1, 1, 1, length(td)), ncdims)
  tsData <- ncdf4::ncvar_get(ncfile, variable_name, start = tsstart, count = tscount, 
    collapse_degen = TRUE)
  v <- xts(x = tsData, order.by = td, tzone = tz(td))
  colnames(v) <- identifier
  return(v)
}, get_all_series = function(variable_name = "rain_obs", dimension_id = get_stations_varname()) {
  "Return a multivariate time series, where each column is the series for one of the identifiers (e.g. rainfall station identifiers)"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  identifiers <- get_values(dimension_id)
  ncdims <- get_variable_dim_names(variable_name)
  # could be e.g.: double q_obs[lead_time,station,ens_member,time] float
  # rain_obs[station,time] lead_time,station,ens_member,time reordered
  # according to the variable present dimensions:
  tsstart <- splice_named_var(c(1, 1, 1, 1), ncdims)
  tscount <- splice_named_var(c(1, length(identifiers), 1, length(td)), ncdims)
  rawData <- ncdf4::ncvar_get(ncfile, variable_name, start = tsstart, count = tscount, 
    collapse_degen = FALSE)
  dim_names(rawData) <- ncdims
  # [station,time] to [time, station] for xts creation
  # NOTE: why can this not be dimension_id instead of stations_dim_name?
  tsData <- reduce_dimensions(rawData,c(time_dim_name, stations_dim_name))
  v <- xts(x = tsData, order.by = td, tzone = tz(td))
  colnames(v) <- identifiers
  return(v)
}, get_ensemble_series = function(variable_name = "rain_ens", identifier, dimension_id = get_stations_varname()) {
  "Return an ensemble of point time series for a station identifier"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  nEns <- get_ensemble_size()
  # float rain_ens[station,ens_member,time]
  rawData <- ncdf4::ncvar_get(ncfile, variable_name, start = c(index_id, 1, 1), count = c(1, 
    nEns, length(td)), collapse_degen = FALSE)
  x <- rawData[1, , ]
  tsData <- (x)  # [ens_member,time] to [time,ens_member]
  xts(x = t(tsData), nrow = length(td), order.by = td, tzone = tz(td))
}, get_time_dim = function() {
  "Gets the time dimension variable as a vector of date-time stamps"
  # if(is.na(time_dim[1]))
  time_dim <<- get_time_dimension(ncfile, time_dim_name, time_zone)
  time_dim
}, get_ensemble_size = function() {
  "Length of the ensemble size dimension"
  ncfile$dim$ens_member$len
}, get_lead_time_count = function() {
  "Length of the lead time dimension"
  ncfile$dim$lead_time$len
}, get_station_count = function() {
  "Length of the lead time dimension"
  ncfile$dim$station$len
}, get_values = function(variable_name) {
  "Gets (and cache in memory) all the values in a variable. Should be used only for dimension variables"
  # TODO: reconsider the name and purpose. This should be a 'private'
  # function, meant only to get dimension variables.
  if (!(variable_name %in% conventional_varnames)) {
    stop(paste(variable_name, "cannot be directly retrieved. Must be in", paste(conventional_varnames, collapse=", ")))
  }  
  if (!(variable_name %in% names(identifiers_dimensions))) {
    identifiers_dimensions[[variable_name]] <<- ncdf4::ncvar_get(ncfile, variable_name)
  }  
  identifiers_dimensions[[variable_name]]
}, index_for_time = function(dateTime) {
  "Gets the index at which a date-time is found in the main time axis of this data set"
  timeAxis <- get_time_dimension(ncfile, time_dim_name, time_zone)
  if (is.na(dateTime)) dateTime <- timeAxis[1]
  dateTime <- as.POSIXct(dateTime)  # WARN: time zones ambiguity if the argument is not fully TZ qualified
  
  if (dateTime <= max(timeAxis)) {
    outindex <- which(dateTime == timeAxis)
  } else {
    if (length(timeAxis) == 1) {
      outindex <- 2
    } else {
      deltat <- timeAxis[2] - timeAxis[1]
      diftime <- dateTime - timeAxis[1]
      outindex <- as.integer(diftime)/as.integer(deltat) + 1
      # print(paste(deltat, diftime))
    }
  }
  # print(paste(outindex,as.POSIXct(dateTime), as.POSIXct(timeAxis[1])))
  return(outindex)
}, index_for_identifier = function(identifier, dimension_idifier = get_stations_varname()) {
  "Gets the index at which an identifier is found in a dimension variable"
  identValues <- get_values(dimension_idifier)
  if (is.na(identifier)) {
    stop("Identifier cannot be NA")
  } else {
    index_id <- which(identifier == identValues)
  }
  index_id
}, put_ensemble_forecasts = function(x, variable_name = "rain_sim", identifier, dimension_id = get_stations_varname(), 
  start_time = NA) {
  "Puts one or more ensemble forecast into a netCDF file"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  if (is.na(start_time)) start_time <- td[1]
  nEns <- get_ensemble_size()
  nEns_x <- dim(x)[2]
  nLT_x <- dim(x)[1]
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  indTime <- index_for_time(start_time)
  if (indTime > length(td)) {
    tu <- get_time_unit()
    timeVal1 <- ncdf4::ncvar_get(ncfile, time_dim_name, start = c(1), count = c(1))
    deltat <- (difftime(td[2], td[1], units = tu))
    outtime <- indTime * deltat
    if (timeVal1 == 0) outtime <- (indTime - 1) * deltat
    ncdf4::ncvar_put(ncfile, time_dim_name, outtime, start = c(indTime), count = c(1))
  }
  if (is.array(x)) {
    # float rain_sim[lead_time,station,ens_member,time]
    lead_time_count <- get_lead_time_count()
    ensData <- array(NA, dim = c(nLT_x, 1, nEns_x, 1))
    # print(c(dim(ensData),dim(x)))
    ensData[, 1, , 1] <- x
    ncdf4::ncvar_put(ncfile, variable_name, ensData, start = c(1, index_id, 1, indTime), 
      count = c(nLT_x, 1, nEns_x, 1))
  } else if (is.list(x)) {
    stop("lists of ensemble forecasts not supported yet")
  }
}, put_ensemble_forecasts_for_station = function(x, variable_name = "rain_sim", identifier, dimension_id = "ens_member", 
  start_time = NA) {
  "Puts a single ensemble member forecasts for all stations into a netCDF file"
  # TODO: review intent and purpose
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dimension(ncfile, time_dim_name, time_zone)
  # td <- get_time_dim()
  if (is.na(start_time)) start_time <- td[1]
  nEns <- get_ensemble_size()
  nStations <- get_station_count()
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  indTime <- index_for_time(start_time)
  if (indTime > length(td)) {
    tu <- get_time_unit()
    timeVal1 <- ncdf4::ncvar_get(ncfile, time_dim_name, start = c(1), count = c(1))
    if (length(td) > 1) {
      deltat <- (difftime(td[2], td[1], units = tu))
    } else {
      deltat <- (difftime(start_time, td[1], units = tu))
    }
    outtime <- indTime * deltat
    if (timeVal1 == 0) outtime <- (indTime - 1) * deltat
    ncdf4::ncvar_put(ncfile, time_dim_name, outtime, start = c(indTime), count = c(1))
    ncdf4::nc_sync(ncfile)
  }
  ltsize <- nrow(x)
  if (is.array(x)) {
    # float rain_sim[lead_time,station,ens_member,time]
    lead_time_count <- get_lead_time_count()
    ensData <- array(NA, dim = c(lead_time_count, nStations, 1, 1))
    ensData[1:ltsize, , 1, 1] <- x
    ncdf4::ncvar_put(ncfile, variable_name, ensData, start = c(1, 1, index_id, indTime), 
      count = c(lead_time_count, nStations, 1, 1))
    ncdf4::nc_sync(ncfile)
  } else if (is.list(x)) {
    stop("lists of ensemble forecasts not supported yet")
  }
}, put_single_series = function(x, variable_name = "rain_obs", identifier, dimension_id = get_stations_varname(), 
  start_time = NA) {
  "Puts a time series, or part thereof"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  if (is.na(start_time)) start_time <- td[1]
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  indTime <- index_for_time(start_time)
  if (is.vector(x)) {
    # float rain_obs[station,time]
    if ((indTime + length(x) - 1) > length(td)) {
      stop(paste("too many elements (", length(x), ") - goes past the end of the time dimension"))
    }
    ncdf4::ncvar_put(ncfile, variable_name, x, start = c(index_id, indTime), count = c(1, 
      length(x)))
  } else {
    stop(paste("putting data of type", class(x), "not yet supported"))
  }
}, put_ensemble_series = function(x, variable_name = "rain_ens", identifier, dimension_id = get_stations_varname()) {
  "Puts an ensemble of time series, e.g. replicate rainfall series"
  stopifnot(variable_name %in% names(ncfile$var))
  td <- get_time_dim()
  index_id <- index_for_identifier(identifier, dimension_id)
  check_index_found(index_id, identifier, dimension_id)
  nEns <- get_ensemble_size()
  nSteps <- length(td)
  if (is.xts(x)) {
    x <- t(as.matrix(x))  # [time,ens_member] to [ens_member,time]
  }
  if (is.array(x)) {
    if (length(dim(x)) != 2) {
      stop("input data must be coercible to an array with two dimensions")
    }
    # float rain_ens[station,ens_member,time]
    if (nrow(x) != nEns) {
      stop(paste("number of rows in the input array is not equal to the number of ensembles", 
        nrow(x), "!=", nEns))
    }
    if (ncol(x) != nSteps) {
      stop(paste("number of columns in the input array is not equal to the length of the time dimension", 
        ncol(x), "!=", nSteps))
    }
    ensData <- array(x, dim = c(1, nEns, nSteps))
    # ensData[1,,] <- x
    ncdf4::ncvar_put(ncfile, variable_name, ensData, start = c(1,index_id, 1, 1), count = c(1,1, 
      nEns, nSteps))
  } else {
    stop(paste("putting data of type", class(x), "not yet supported"))
  }
}, put_values = function(x, variable_name) {
  # TODO: reconsider the name and purpose. This should be a 'private'
  # function, meant only to get dimension variables.
  "Puts all the values in a variable. Should be used only for dimension variables"
  if (!(variable_name %in% conventional_varnames)) {
    stop(paste(variable_name, "cannot be directly set. Must be one of", paste(conventional_varnames, collpase=", ")))
  }  
  ncdf4::ncvar_put(ncfile, variable_name, x)
  identifiers_dimensions[[variable_name]] <<- x
}, summary = function() {
  "Print a summary of this EFTS netCDF file"
  tAxis <- get_time_dim()  # TODO: may be optimized for less access on disk.
  tspan <- as.character(tAxis[c(1, length(tAxis))])
  cat("Tzone:", as.character(tz(tAxis)), "\n")
  cat("Start:", tspan[1], "\n")
  cat("End:", tspan[2], "\n")
  cat("Ens. size:", get_ensemble_size(), "\n")
  cat("Lead time:", get_lead_time_count(), "\n")
  cat("Variables:", ncfile$nvars, "\n")
  if (ncfile$nvars > 0) {
    for (i in 1:ncfile$nvars) {
      nd <- ncfile$var[[i]]$ndims
      dimstring <- "["
      if (nd > 0) {
        for (j in 1:nd) {
          dimstring <- paste(dimstring, ncfile$var[[i]]$dim[[j]]$name, 
          sep = "")
          if (j < nd) dimstring <- paste(dimstring, ",", sep = "")
        }
      }
      dimstring <- paste(dimstring, "] ", sep = "")
      cat(paste0("        ", ncfile$var[[i]]$prec, " ", ncfile$var[[i]]$name, 
        dimstring, "\n"))
      atts <- ncatt_get(ncfile, ncfile$var[[i]]$name)
    }
  }
}, close = function() {
  ncdf4::nc_close(ncfile)
}, sync = function() {
  ncdf4::nc_sync(ncfile)
}))


findMatches <- function(pattern, values)
{
  # findMatches in utils is not exported (has an option for fuzzy matching)
  # from utils findExactMatches: 
  grep(pattern, values, value = TRUE)
}

#' method for dollar-tab-completion in R consoles.
#'
#' method for dollar-tab-completion in R consoles. It may be unnecessary in 
#' the future but such a method was required at some time to at least avoid 
#' some issues in RStudio. We may also want to customise matches compared to default reference classes.
#'
#' @name eftsdotDollarNames
#' @param x A reference class object
#' @param pattern the regex pattern to search for in potential methods. Default value is possibly required by some versions of RStudio
#' @importFrom utils .DollarNames
#' @export
.DollarNames.EftsDataSet <- function(x, pattern = "") {

  if(isS4(x)) {
    findMatches(pattern, getRefClass(class(x))$methods())
  } else {
    character(0)
  }
  
  # The dollar completion thing documentation is hard to find(to me)
  # Sources of information
  # https://svn.r-project.org/R/trunk/src/library/utils/R/completion.R
  # https://stat.ethz.ch/R-manual/R-devel/library/utils/html/rcompgen.html
  # https://github.com/cran/tensorflow/blob/master/R/generics.R
  # http://stackoverflow.com/a/26711603/2752565
  # utils:::findMatches(pattern, getRefClass(class(x))$methods())
} 

