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

#' Retrieves the first date of the time dimension from a netCDF file
#'
#' Retrieves the first date of the time dimension from a netCDF file of daily data, given the units found in the netCDF attribute for the time dimension
#'
#' @param time_units The string description of the units of the time dimension e.g. 'days since 1980-01-01' or 'hours since 2010-08-01 13:00:00 +0000'
#' @param time_zone the time zone to use for the returned value.
#' @export
#' @importFrom udunits2 ud.convert
#' @importFrom stringr str_split
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

  # temporary work around https://github.com/jmp75/efts/issues/3
  udu <- stringr::str_split(time_units, pattern = ' +')[[1]]
  s <- paste(udu[3], udu[4], sep='T')
  dt <- ymd_hms(s)
  if(is.na(dt)) stop(paste0('Could not parse date time out of string ', time_units))
  return(dt)

  # refDate <- lubridate::origin  # 
  # class(refDate) <- c("POSIXct", "POSIXt")  # workaround what I think is a lubridate bug (possibly now wolved); 
  # # try origin + days(1)  and its effect, visibly because of class ordering on origin.
  # isDaily <- is_daily_time_step(time_units)
  # refDateUnits <- paste(ifelse(isDaily, "days", "hours"), "since 1970-01-01 00:00:00 +0000")
  # offsetSinceRef <- udunits2::ud.convert(0, time_units, refDateUnits)
  # offsetFun <- get_time_step_function(time_units)
  # startDateUtc <- refDate + offsetFun(offsetSinceRef)
  # startDate <- lubridate::with_tz(startDateUtc, time_zone)
  # return(startDate)
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


########################################
# Below are functions not exported
########################################

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

