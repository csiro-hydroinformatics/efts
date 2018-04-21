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


