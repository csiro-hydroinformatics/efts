check_index_found <- function(index_id, identifier, dimension_id) {
  if (length(index_id) == 0) 
    stop(paste0("identifier '", identifier, "' not found in the dimension '", 
      dimension_id, "'"))
}

stations_dim_name <- "station"
lead_time_dim_name <- "lead_time"
time_dim_name <- "time"
ensemble_member_dim_name <- "ens_member"

get_default_dim_order <- function() {
  return(c(lead_time_dim_name, stations_dim_name, ensemble_member_dim_name, time_dim_name))
}

splice_named_var <- function(d, ncdims = character()) {
  default_order <- get_default_dim_order()
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
      names(d) <- ncdims
    }
  }
  return(d)
}


dim_names <- function(x) {
  attr(x, 'dim_names')
}

"dim_names<-" <- function(x, value) {
  d <- dim(x)
  if(is.array(x)) {
    if(length(d) != length(value)) stop("dim names is not equal to the number of dimensions of the array")
    if(length(unique(value)) != length(d)) stop("specified dim names are not unique")
  } else if (is.vector(x)){
    stopifnot(length(value) == 1)
  } else { stop('not an array nor a vector - cannot set dim_names')}
  attr(x, 'dim_names') <- value
  return(x)
}

reduce_dimensions <- function(x, subset_dim_names){
  dimsize_input <- dim(x)
  dn <- dim_names(x)
  if(is.null(dn)) stop('the input array must have a valid dim_names attribute')
  if(length(dn) != length(dimsize_input)) stop('the input array and its dim_names attribute are differing in length')

  names(dimsize_input) <- dn
  if(missing(subset_dim_names) || is.na(subset_dim_names))
    subset_dim_names = dn[dimsize_input > 1]

  diffdim <- setdiff(subset_dim_names, dn)
  if (length(diffdim)>0) stop(paste0('Dimension names to slice but not found in array dim names: ', paste(diffdim, collapse=', ')))

  dropped_dims <- setdiff(dn,subset_dim_names)
  if( any(dimsize_input[dropped_dims] > 1)) stop('Cannot drop non-degenerate when subsetting')

  w <- match(subset_dim_names,dn)
  other <- match(setdiff(dn, subset_dim_names),dn)

  x_reordered <- aperm(x, c(w, other))

  reordered_dim_names <- dn[c(w, other)]
  reordered_dim_sizes <- dim(x_reordered)

  new_dim_sizes <- reordered_dim_sizes[1:length(w)]
  new_dim_names <- reordered_dim_names[1:length(w)]

  y <- drop(x_reordered)
  # We want however to maintain degenerate 
  # dimensions that have been explicitly asked for, 
  # and that would have been otherwise dropped
  y <- array(y, new_dim_sizes)

  dim_names(y) <- new_dim_names
  return(y)
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
  time_dim <- ncdf4::ncdim_def(time_dim_name, units = time_dim_info$units, vals = time_dim_info$values, 
    unlim = T, create_dimvar = TRUE, longname = "time")
  station_dim <- ncdf4::ncdim_def("station", units = "", vals = c(1:num_stations), 
    unlim = F, create_dimvar = TRUE, longname = "")
  str_dim <- ncdf4::ncdim_def("str_len", units = "", vals = c(1:str_len), unlim = FALSE, 
    create_dimvar = TRUE, longname = "string length")
  lead_time_dim <- ncdf4::ncdim_def(lead_time_dim_name, units = "", vals = 1:lead_length, 
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

create_mandatory_vardefs <- function(station_dim, str_dim, ensemble_dim, lead_time_dim, lead_time_tstep = "hours") {

  # https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#mandatory-variables
  # float time(time)
  # int station_id(station)
  # char station_name(strLen, station)
  # int ens_member(ens_member)
  # float lead_time(lead_time)
  # float lat (station)
  # float lon (station)
  variables <- list(
    station_ids_var = ncdf4::ncvar_def("station_id", units = "", 
      dim = list(station_dim), missval = NULL, longname = "station or node identification code", 
      prec = "integer"), 
    station_names_var = ncdf4::ncvar_def("station_name", units = "", 
      dim = list(str_dim, station_dim), missval = NULL, longname = "station or node name", 
      prec = "char"), 
    ensemble_var = ncdf4::ncvar_def("ens_member", units = "member id", 
      dim = list(ensemble_dim), missval = NULL, longname = "ensemble member", prec = "integer"), 
    lead_time_var = ncdf4::ncvar_def(lead_time_dim_name, units = paste(lead_time_tstep, "since time"), 
      dim = list(lead_time_dim), missval = NULL, longname = "forecast lead time", 
      prec = "integer"), 
    latitude_var = ncdf4::ncvar_def("lat", units = "degrees north", 
      dim = list(station_dim), missval = -9999, longname = "latitude", prec = "float"), 
    longitude_var = ncdf4::ncvar_def("lon", 
      units = "degrees east", dim = list(station_dim), missval = -9999, 
      longname = "longitude", prec = "float")
  )
  return(variables)
}

create_optional_vardefs <- function(station_dim, vars_def = default_optional_vardefs_v2()) {
  # https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#mandatory-variables
  vars_def$rownum <- 1:nrow(vars_def)
  f <- function(vd) {
    ncdf4::ncvar_def(vd$name, 
      units = vd$units, dim = list(station_dim), missval = vd$missval, longname = vd$longname, 
      prec = vd$precision)
  } 
  ncvar_defs <- plyr::dlply(.data = vars_def, .variables = "rownum", .fun = f)
  return(ncvar_defs)
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
#' @seealso See
#'    \code{\link{create_efts}} for examples
create_efts_variables <- function(data_var_def, time_dim_info, num_stations, lead_length, 
  ensemble_length, optional_vars, lead_time_tstep) {
  efts_dims <- create_nc_dims(time_dim_info = time_dim_info, num_stations = num_stations, 
    lead_length = lead_length, ensemble_length = ensemble_length)
  time_dim <- efts_dims$time_dim
  lead_time_dim <- efts_dims$lead_time_dim
  station_dim <- efts_dims$station_dim
  str_dim <- efts_dims$str_dim
  ensemble_dim <- efts_dims$ensemble_dim
  
  mandatory_var_ncdefs <- create_mandatory_vardefs(station_dim, str_dim, ensemble_dim, lead_time_dim, lead_time_tstep);
  variables_metadata <- mandatory_var_ncdefs
  if(!missing(optional_vars)) {
    optional_var_ncdefs <- create_optional_vardefs(station_dim, vars_def = optional_vars)
    # TODO if not native to ncdf4: check name clashes
    # already_defs <- names(variables)
    variables_metadata <- c(variables_metadata, optional_var_ncdefs)
  }

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
  
  variables <- list()
  variables$metadatavars <- variables_metadata
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
    stop(paste("Could not detect if hourly or daily - unit not supported:", 
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

