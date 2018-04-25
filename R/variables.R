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
#' var_def <- create_variable_definition(name='rain_der', 
#'   longname='Rainfall ensemble forecast derived from some prediction', units='mm', 
#'   missval=-9999.0, precision='double', var_attribute=list(type=2L, 
#'     description="accumulated over the preceding interval", 
#'     dat_type = "der", dat_type_description="AWAP data interpolated from observations",
#'     location_type = "Point"))
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
#' @param var_attributes a list of named attributes. See \code{\link{create_var_attribute_definition}} 
#' @export
#' @return a data frame suitable for \code{\link{create_variable_definition}}
#' @seealso See
#'    \code{\link{create_variable_definition}} and \code{\link{create_efts}} for examples
create_variable_definition_dataframe <- function(variable_names, long_names = variable_names, standard_names = variable_names, units = "mm", missval = -9999, 
  precision = "double", dimensions = 4L, var_attributes = create_var_attribute_definition()) {
  stopifnot(is.character(variable_names))
  varsDef <- data.frame(name = variable_names, stringsAsFactors = FALSE)
  varsDef$longname <- long_names
  varsDef$standard_name <- standard_names
  varsDef$units <- units
  varsDef$missval <- missval
  varsDef$precision <- precision
  varsDef$dimensions <- as.integer(dimensions)

  va <- data.frame(var_attributes, stringsAsFactors = FALSE)
  if(nrow(va) < nrow(varsDef)) {
    va <- va[ rep(1:nrow(va), length.out=nrow(varsDef)), ]
  }

  varsDef <- cbind(varsDef, va)
  rownames(varsDef) <- varsDef$name
  return(varsDef)
}

#' Provide a template definition of optional geolocation variables
#'
#' Provide a template definition of optional geolocation and geographic variables x, y, area and elevation. 
#' See \url{https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#optional-variables}.
#'
#' @export
#' @return a data frame
#' @seealso See
#'    \code{\link{create_variable_definition}} and \code{\link{create_efts}} for examples
#' @export
default_optional_variable_definitions_v2_0 <- function() {
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
    "elevation"
    )
  varsDef$units <- c("","","km^2","m")
  varsDef$missval <- c(NA,NA,-9999,-9999)
  varsDef$precision <- "float"
  rownames(varsDef) <- varsDef$name
  return(varsDef)
}


########################################
# Below are functions not exported
########################################

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
  in_names <- names(dframe)
  non_opt_attr <- c("name", "longname", "units", "missval", "precision", "dimensions")
  varargs_attr <- setdiff(in_names, non_opt_attr)
  f <- function(varDef) {
    create_variable_definition(
      name = varDef[["name"]], 
      longname = varDef[["longname"]], 
      units = varDef[["units"]], 
      missval = varDef[["missval"]], 
      precision = varDef[["precision"]], 
      dim_type = varDef[["dimensions"]], 
      var_attribute = as.list(varDef[,varargs_attr]))
  }  # f
  dframe$rownum <- 1:nrow(dframe)
  r <- plyr::dlply(.data = dframe, .variables = "rownum", .fun = f)
  names(r) <- rownames(dframe)
  return(r)
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
    station_ids_var = ncdf4::ncvar_def(station_id_varname, units = "", 
      dim = list(station_dim), missval = NULL, longname = "station or node identification code", 
      prec = "integer"), 
    station_names_var = ncdf4::ncvar_def(station_name_varname, units = "", 
      dim = list(str_dim, station_dim), missval = NULL, longname = "station or node name", 
      prec = "char"), 
    ensemble_var = ncdf4::ncvar_def(ensemble_member_dim_name, units = "member id", 
      dim = list(ensemble_dim), missval = NULL, longname = "ensemble member", prec = "integer"), 
    lead_time_var = ncdf4::ncvar_def(lead_time_dim_name, units = paste(lead_time_tstep, "since time"), 
      dim = list(lead_time_dim), missval = NULL, longname = "forecast lead time", 
      prec = "integer"), 
    latitude_var = ncdf4::ncvar_def(lat_varname, units = "degrees north", 
      dim = list(station_dim), missval = -9999, longname = "latitude", prec = "float"), 
    longitude_var = ncdf4::ncvar_def(lon_varname, 
      units = "degrees east", dim = list(station_dim), missval = -9999, 
      longname = "longitude", prec = "float")
  )
  return(variables)
}

create_optional_vardefs <- function(station_dim, vars_def = default_optional_variable_definitions_v2_0()) {
  # https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#mandatory-variables
  vars_def$rownum <- 1:nrow(vars_def)
  f <- function(vd) {
    ncdf4::ncvar_def(vd$name, 
      units = vd$units, dim = list(station_dim), missval = vd$missval, longname = vd$longname, 
      prec = vd$precision)
  } 
  ncvar_defs <- plyr::dlply(.data = vars_def, .variables = "rownum", .fun = f)
  names(ncvar_defs) <- rownames(vars_def)
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
#' @param optional_vars a data frame defining optional netCDF variables. For a templated default see 
#' \code{\link{default_optional_variable_definitions_v2_0}} and 
#' \url{https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#optional-variables}
#' @param lead_time_tstep string specifying the time step of the forecast lead length.
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
  if(!is.null(optional_vars)) {
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

