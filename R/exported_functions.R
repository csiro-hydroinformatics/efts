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
#' (stations_ids <- snc$get_values('station_id'))
#' nEns <- snc$get_ensemble_size()
#' nLead <- snc$get_lead_time_count()
#' td <- snc$get_time_dim()
#' stopifnot('rain_fcast_ens' %in% variable_names)
#' 
#' ens_fcast_rainfall <- snc$get_ensemble_forecasts('rain_fcast_ens',
#'   stations_ids[1], start_time=td[2])
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

#' Creates a EftsDataSet for write access to a netCDF EFTS data set
#'
#' Creates a EftsDataSet for write access to a netCDF EFTS data set
#'
#' @param fname file name to create to. The file must not exist already.
#' @param time_dim_info a list with the units and values defining the time dimension of the data set
#' @param data_var_definitions a data frame, acceptable by \code{\link{create_variable_definitions}}, or list of netCDF variable definitions, e.g. 
#'       \code{list(rain_sim=list(name='rain_sim', longname='ECMWF Rainfall ensemble forecasts', units='mm', missval=-9999.0, precision='double', attributes=list(type=2, type_description='accumulated over the preceding interval')))}
#' @param stations_ids station identifiers, coercible to an integer vector (note: may change to be a more flexible character storage)
#' @param station_names optional; names of the stations
#' @param nc_attributes a named list of characters, attributes for the whole file, 
#' including mandatory ones: title, institution, source, catchment, comment. 
#' You may use \code{\link{create_global_attributes}} as a starting template.
#' @param lead_length length of the lead forecasting time series.
#' @param optional_vars a data frame defining optional netCDF variables. For a templated default see 
#' \code{\link{default_optional_variable_definitions_v2_0}} and 
#' \url{https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md#optional-variables}
#' @param lead_time_tstep string specifying the time step of the forecast lead length.
#' @param ensemble_length number of ensembles, i.e. number of forecasts for each point on the main time axis of the data set
#' @examples
#'
#' # NOTE
#' # The sample code below is purposely generic; to produce 
#' # a data set conforming with the conventions devised for 
#' # ensemble streamflow forecast you will need to 
#' # follow the additional guidelines at 
#' # https://github.com/jmp75/efts/blob/master/docs/netcdf_for_water_forecasting.md
#'
#' fname <- tempfile()
#' 
#' stations_ids <- c(123,456)
#' nEns <- 3
#' nLead <- 4
#' nTimeSteps <- 12
#' 
#' timeAxisStart <- ISOdate(year=2010, month=08, day=01, hour = 14, min = 0, sec = 0, tz = 'UTC')
#' time_dim_info <- create_time_info(from=timeAxisStart, 
#'   n=nTimeSteps, time_step = "hours since")
#' 
#' # It is possible to define variables for three combinations of dimensions.
#' # dimensions '4' ==> [lead_time,station,ens_member,time]
#' # dimensions '3' ==> [station,ens_member,time]   
#' # dimensions '2' ==> [station,time]   
#' 
#' variable_names <- c('var1_fcast_ens','var2_fcast_ens', 'var1_obs', 
#'   'var2_obs', 'var1_ens','var2_ens')
#' 
#' va <- create_var_attribute_definition(
#'   type = 2L, 
#'   type_description = "accumulated over the preceding interval", 
#'   dat_type = "der", 
#'   dat_type_description = paste(rep(c("var1", "var2"), 3), "synthetic test data"),
#'   location_type = "Point")
#' 
#' 
#' (varDef <- create_variable_definition_dataframe(
#'   variable_names=variable_names, 
#'   long_names = paste(variable_names, 'synthetic data'), 
#'   dimensions = c(4L,4L,2L,2L,3L,3L),
#'   var_attributes = va))
#' 
#' glob_attr <- create_global_attributes(
#'   title="data set title", 
#'   institution="my org", 
#'   catchment="Upper_Murray", 
#'   source="A journal reference, URL", 
#'   comment="example for vignette")
#' 
#' (opt_metadatavars <- default_optional_variable_definitions_v2_0())
#' 
#' snc <- create_efts(
#'   fname=fname, 
#'   time_dim_info=time_dim_info, 
#'   data_var_definitions=varDef, 
#'   stations_ids=stations_ids, 
#'   nc_attributes=glob_attr, 
#'   optional_vars = opt_metadatavars, 
#'   lead_length=nLead, 
#'   ensemble_length=nEns,
#'   lead_time_tstep = "hours")
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
#'   for (j in 1:length(stations_ids)) {
#'     station <- stations_ids[j]
#'     var1Values <- i + 0.1*j + m
#'     var2Values <- 2*var1Values
#'     dtime = td[i]
#'     snc$put_ensemble_forecasts(var1Values, variable_name = variable_names[1], 
#'       identifier = station, start_time = dtime)
#'     snc$put_ensemble_forecasts(var2Values, variable_name = variable_names[2], 
#'       identifier = station, start_time = dtime)
#'   }
#' }
#' 
#' timeSteps <- 1:length(td)
#' for (j in 1:length(stations_ids)) {
#'   var3Values <- timeSteps + 0.1*j
#'   var4Values <- var3Values + 0.01*timeSteps + 0.001*j
#' 
#'   station <- stations_ids[j]
#'   snc$put_single_series(var3Values, variable_name = variable_names[3], identifier = station)
#'   snc$put_single_series(var4Values, variable_name = variable_names[4], identifier = station)
#' }
#' 
#' for (j in 1:length(stations_ids)) {
#' 
#'   var5Xts <- matrix(rep(1:nEns, each=nTimeSteps) + timeSteps + 0.1*j, ncol=nEns)
#' 
#'   # [time,ens_member] to [ens_member,time], as expected by put_ensemble_series
#'   var5Values <- t(var5Xts) 
#'   var6Values <- 0.25 * var5Values
#' 
#'   station <- stations_ids[j]
#'   snc$put_ensemble_series(var5Values, variable_name = variable_names[5], identifier = station)
#'   snc$put_ensemble_series(var6Values, variable_name = variable_names[6], identifier = station)
#' }
#' 
#' # We can get/put values for some metadata variables:
#' snc$get_values("x")
#' snc$put_values(c(1.1, 2.2), "x")
#' snc$put_values(letters[1:2], "station_name")
#' 
#' # Direct get/set access to data variables, however, is prevented;
#' #  the following would thus cause an error:
#' # snc$get_values("var1_fcast_ens")
#' 
#' snc$close()
#' # Cleaning up temp file:
#' if (file.exists(fname)) 
#'   file.remove(fname)
#' 
#'  
#' 
#' @export
#' @import ncdf4
#' @importFrom utils packageDescription
#' @importFrom methods new
#' @return A EftsDataSet object
create_efts <- function(fname, time_dim_info, data_var_definitions, stations_ids, station_names=NULL, 
  nc_attributes=NULL, optional_vars=NULL, lead_length = 48, ensemble_length = 50, lead_time_tstep = "hours") {
  
  if (missing(stations_ids)) {
    stop("You must provide station identifiers when creating a new EFTS netCDF data set")
  }
  
  if (is.null(nc_attributes)) {
    stop(paste("You must provide a suitable list for nc_attributes, including", paste(mandatory_global_attributes, collapse=", ")))
  }
  check_global_attributes(nc_attributes)
  
  if (file.exists(fname)) 
    stop(paste("File already exists:", fname))
  
  if (is.data.frame(data_var_definitions)) 
    data_var_definitions <- create_variable_definitions(data_var_definitions)
    
  varDefs <- create_efts_variables(data_var_definitions, 
                                    time_dim_info, 
                                    num_stations = length(stations_ids), 
                                    lead_length = lead_length, 
                                    ensemble_length = ensemble_length,
                                    optional_vars = optional_vars,
                                    lead_time_tstep = lead_time_tstep)

  ## Determine if there is real value in a tryCatch. What is the point if we cannot close/delete the file.
  # nc <- tryCatch(
  #   createSchema(fname, varDefs, data_var_definitions, nc_attributes, optional_vars, 
  #     stations_ids, lead_length, ensemble_length, station_names),
  #   error = function(e) {
  #     stop(paste("netCDF schema creation failed", e))
  #     NULL
  #   }, finally = function() {
  #   }
  # )
  nc <- createSchema(fname, varDefs, data_var_definitions, nc_attributes, optional_vars, 
    stations_ids, lead_length, ensemble_length, station_names)

  return(EftsDataSet$new(nc))
}

########################################
# Below are functions not exported
########################################

infoList <- function(theList) {
  paste(paste(names(theList), theList, sep = ": "), collapse = ", ")
}

createSchema <- function(fname, varDefs, data_var_definitions, nc_attributes, optional_vars, 
  stations_ids, lead_length, ensemble_length, station_names=NA) {

  allVars <- c(varDefs$datavars, varDefs$metadatavars)
  nc <- ncdf4::nc_create(fname, vars = allVars)

  ## attributes for data variables
  lapply(data_var_definitions, put_variable_attributes, nc)

  ## attributes for dimensions variables
  ncdf4::ncatt_put(nc, time_dim_name, "standard_name", time_dim_name)
  ncdf4::ncatt_put(nc, time_dim_name, "time_standard", "UTC")
  ncdf4::ncatt_put(nc, time_dim_name, "axis", "t")
  ncdf4::ncatt_put(nc, ensemble_member_dim_name, "standard_name", "ens_member")
  ncdf4::ncatt_put(nc, ensemble_member_dim_name, "axis", "u")
  ncdf4::ncatt_put(nc, lead_time_dim_name, "standard_name", "lead_time")
  ncdf4::ncatt_put(nc, lead_time_dim_name, "axis", "v")
  ncdf4::ncatt_put(nc, lat_varname, "axis", "y")
  ncdf4::ncatt_put(nc, lon_varname, "axis", "x")
  
  ## attributes for optional metadata variables
  if(!is.null(optional_vars))
  {
    var_names <- rownames(optional_vars)
    if("standard_name" %in% colnames(optional_vars)){
      for (v in var_names) {
        sn <- optional_vars[v, "standard_name"]
        if(!is.na(sn)) ncdf4::ncatt_put(nc, v, "standard_name", sn)
      }
    }
    if(x_varname %in% var_names){
      ncdf4::ncatt_put(nc, x_varname, "axis", "x")
    }
    if(y_varname %in% var_names){
      ncdf4::ncatt_put(nc, y_varname, "axis", "y")
    }
  }

  ## Add global attributes
  ncdf4::ncatt_put(nc, 0, "STF_convention_version", 2)
  ncdf4::ncatt_put(nc, 0, "STF_nc_spec", "https://github.com/jmp75/efts/blob/107c553045a37e6ef36b2eababf6a299e7883d50/docs/netcdf_for_water_forecasting.md")
  ncdf4::ncatt_put(nc, 0, "history", 
    paste( 
      as.character(lubridate::now(tzone="UTC")),
      "UTC", 
      "file created with the R package efts", packageDescription("efts")$Version 
    ) %>% infoList)
  
  if(!is.null(nc_attributes)) {
    for (k in names(nc_attributes)) {
      pad_global_attribute(nc, k, nc_attributes[k])
    }
  }

  ## populate metadata variables
  ncdf4::ncvar_put(nc, station_id_varname, stations_ids)
  ncdf4::ncvar_put(nc, lead_time_dim_name, 1:lead_length)
  ncdf4::ncvar_put(nc, ensemble_member_dim_name, 1:ensemble_length)
  if (!is.null(station_names)) {
    ncdf4::ncvar_put(nc, station_name_varname, station_names)
  }
  # One seems to need to close/reopen the newly created file, otherwise some
  # ncvar_get operations will fail with a cryptic message.  I follow the
  # advice in this and associated posts
  # https://www.unidata.ucar.edu/mailing_lists/archives/netcdfgroup/2012/msg00270.html
  ncdf4::nc_close(nc)
  nc <- ncdf4::nc_open(fname, write = TRUE, readunlim = FALSE)
  return(nc)
}
