check_index_found <- function(index_id, identifier, dimension_id) {
  if (length(index_id) == 0) 
    stop(paste0("identifier '", identifier, "' not found in the dimension '", 
      dimension_id, "'"))
}

splice_named_var <- function(d, ncdims = character()) {
  default_order <- c("lead_time", "station", "ens_member", "time")
  d <- as.integer(d)
  stopifnot(length(d) == 4)
  stopifnot(is.vector(d))
  # lead_time,station,ens_member,time
  names(d) <- default_order
  if (length(ncdims) > 0) {
    if (!all(ncdims %in% default_order)) {
      stop(paste0("Invalid dimensions for a data variable: ", paste(ncdims, 
        collapse = ",")))
    } else {
      d <- d[ncdims]
    }
  }
  return(d)
}

#' Creates dimensions for a netCDF EFTS data set
#'
#' Creates dimensions for a netCDF EFTS data set. Note that end users are unlikely to need to use this function directly, hence this is not exported
#'
#' @param time_dim_info a list with the units and values defining the time dimension of the data set
#' @param str_len maximum length of the character for the station identifiers.
#' @param lead_length length of the lead time.
#' @param ensemble_length number of ensembles, i.e. number of forecasts for each point on the main time axis of the data set
#' @param num_stations number of stations
#' @import ncdf4
#' @return A list of ncdf4 dimensions
#' @seealso See
#'    \code{\link{create_efts}} for examples
create_nc_dims <- function(time_dim_info, str_len = 30, lead_length = 1, ensemble_length = 1, num_stations = 1) {
  time_dim <- ncdf4::ncdim_def("time", units = time_dim_info$units, vals = time_dim_info$values, 
    unlim = T, create_dimvar = TRUE, longname = "time")
  station_dim <- ncdf4::ncdim_def("station", units = "", vals = c(1:num_stations), 
    unlim = F, create_dimvar = TRUE, longname = "")
  str_dim <- ncdf4::ncdim_def("str_len", units = "", vals = c(1:str_len), unlim = FALSE, 
    create_dimvar = TRUE, longname = "string length")
  lead_time_dim <- ncdf4::ncdim_def("lead_time", units = "", vals = 1:lead_length, 
    unlim = F, create_dimvar = FALSE)
  ensemble_dim <- ncdf4::ncdim_def("ens_member", units = "", vals = 1:ensemble_length, 
    unlim = F, create_dimvar = FALSE)
  list(time_dim = time_dim, lead_time_dim = lead_time_dim, station_dim = station_dim, 
    str_dim = str_dim, ensemble_dim = ensemble_dim)
}

#' @import magrittr
create_data_variable <- function(data_var_def, dimensions) {
  a <- data_var_def
  (c("name", "units") %in% names(a)) %>% all %>% stopifnot
  varname <- a[["name"]]
  longname <- ifelse("longname" %in% names(a), a[["longname"]], varname)
  precision <- ifelse("precision" %in% names(a), a[["precision"]], "double")
  missval <- ifelse("missval" %in% names(a), a[["missval"]], -9999)
  vardef <- ncdf4::ncvar_def(name = varname, units = a[["units"]], dim = dimensions, 
    missval = missval, longname = longname, prec = precision)
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

#' Create netCDF variables according to the definition 
#'
#' Create netCDF variables according to the definition 
#'
#' @param data_var_def a list, with each item itself a list suitable as a variable definition argument to create_data_variable
#' @param time_dim_info a list with the units and values defining the time dimension of the data set
#' @param num_stations number of (gauging) stations identifying points in the data set
#' @param lead_length length of the lead forecasting time series.
#' @param ensemble_length number of ensembles, i.e. number of forecasts for each point on the main time axis of the data set
create_efts_variables <- function(data_var_def, time_dim_info, num_stations, lead_length, 
  ensemble_length) {
  efts_dims <- create_nc_dims(time_dim_info = time_dim_info, num_stations = num_stations, 
    lead_length = lead_length, ensemble_length = ensemble_length)
  time_dim <- efts_dims$time_dim
  lead_time_dim <- efts_dims$lead_time_dim
  station_dim <- efts_dims$station_dim
  str_dim <- efts_dims$str_dim
  ensemble_dim <- efts_dims$ensemble_dim
  
  variables <- list(station_names_var = ncdf4::ncvar_def("station_name", units = "", 
    dim = list(str_dim, station_dim), missval = NULL, longname = "station or node name", 
    prec = "char"), station_ids_var = ncdf4::ncvar_def("station_id", units = "", 
    dim = list(station_dim), missval = NULL, longname = "station or node identification code", 
    prec = "integer"), lead_time_var = ncdf4::ncvar_def("lead_time", units = "hours since time", 
    dim = list(lead_time_dim), missval = NULL, longname = "forecast lead time", 
    prec = "integer"), ensemble_var = ncdf4::ncvar_def("ens_member", units = "member id", 
    dim = list(ensemble_dim), missval = NULL, longname = "ensemble member", prec = "integer"), 
    latitude_var = ncdf4::ncvar_def("lat", units = "degrees north", dim = list(station_dim), 
      missval = -9999, longname = "latitude", prec = "float"), longitude_var = ncdf4::ncvar_def("lon", 
      units = "degrees east", dim = list(station_dim), missval = -9999, 
      longname = "longitude", prec = "float"), elevation_var = ncdf4::ncvar_def("elevation", 
      units = "m", dim = list(station_dim), missval = -9999, longname = "station elevation above sea level", 
      prec = "float"))
  
  unknownDims <- which(plyr::laply(data_var_def, .fun = function(x) {
    (x$dim_type %in% c("2", "3", "4")) == FALSE
  }))
  if (length(unknownDims) > 0) 
    stop(paste("Invalid dimension specifications for", length(unknownDims), 
      "variables. Only supported are characters 2, 3, 4"))
  
  ensFcastDataVarDef <- data_var_def[sapply(data_var_def, function(x) {
    x$dim_type == "4"
  })]
  ensDataVarDef <- data_var_def[sapply(data_var_def, function(x) {
    x$dim_type == "3"
  })]
  pointDataVarDef <- data_var_def[sapply(data_var_def, function(x) {
    x$dim_type == "2"
  })]
  
  variables$datavars <- lapply(ensFcastDataVarDef, create_data_variable, list(lead_time_dim, 
    station_dim, ensemble_dim, time_dim))  # [lead_time,station,ens_member,time]
  
  variables$datavars <- c(variables$datavars, lapply(ensDataVarDef, create_data_variable, 
    list(station_dim, ensemble_dim, time_dim))  # [station,ens_member,time]
)
  
  variables$datavars <- c(variables$datavars, lapply(pointDataVarDef, create_data_variable, 
    list(station_dim, time_dim))  # [station,time]
)
  
  names(variables$datavars) <- lapply(c(ensFcastDataVarDef, ensDataVarDef, 
    pointDataVarDef), `[[`, "name")
  variables
}


is_daily_time_step <- function(time_units) {
  isDaily <- charmatch("days since", time_units)
  isDaily <- ifelse(is.na(isDaily), FALSE, isDaily == 1)
  isHourly <- charmatch("hours since", time_units)
  isHourly <- ifelse(is.na(isHourly), FALSE, isHourly == 1)
  if (!(isDaily | isHourly)) 
    stop(paste("Could not detect if hourly or daily - Unit not supported", 
      time_units))
  isDaily
}

#' Detect the unit of the time step in the time axis unit
#'
#' @param time_units The string description of the units of the time dimension e.g. 'days since 1980-01-01' or 'hours since 2010-08-01 13:00:00 +0000'
#' @import lubridate
#' @return A duration function from lubridate
get_time_step_function <- function(time_units) {
  isDaily <- is_daily_time_step(time_units)
  offsetFun <- ifelse(isDaily, lubridate::ddays, lubridate::dhours)
  return(offsetFun)
}

