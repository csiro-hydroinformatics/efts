context("Write-Read Round trip EFTS netCDF data set")

library(xts)

variable_names <- c("variable_1", "variable_2")
stations_ids <- c(123, 456)

nEns <- 3
nLead <- 4
x <- 1:(nEns * nLead)
x <- matrix(as.numeric(x), nrow = nLead)
y <- x + nEns * nLead

timeAxisStart <- ISOdate(year = 2010, month = 8, day = 1, hour = 12, min = 0, 
  sec = 0, tz = "UTC")
tested_fcast_issue_time <- timeAxisStart + lubridate::hours(6L)
v1 <- variable_names[1]
s1 <- stations_ids[1]
v2 <- variable_names[2]
s2 <- stations_ids[2]


# put tests in a tryCatch, to maximise the chances of cleaning up temporary
# files.
doTests <- function(lead_time_tstep = "hours", time_step = "hours since", time_step_delta = 1L, lead_time_step_start_offset = 1L, lead_time_step_delta = 1L) {

# lead_time_tstep = "days"
# time_step = "days since"
# time_step_delta = 1L
# lead_time_step_start_offset = 1L
# lead_time_step_delta = 1L

  case_params <- paste0("lts=", lead_time_tstep, ",ts=", time_step, ",tsdelta=", time_step_delta, ",ltsoffset=", lead_time_step_start_offset, ",ltsdelta=", lead_time_step_delta)

  time_dim_info <- create_time_info(from = timeAxisStart, n = 10, time_step = time_step, time_step_delta = time_step_delta)

  test_that(paste0("Can create a small EFTS netCDF file ", case_params), {
    
    varsDef <- data.frame(name = variable_names, stringsAsFactors = FALSE)
    varsDef$longname <- paste("long name for", varsDef$name)
    varsDef$units <- "mm"
    varsDef$missval <- -999
    varsDef$precision <- "double"
    varsDef$type <- 2
    varsDef$dimensions <- "4"
    varsDef$type_description <- "accumulated over the previous time step"
    varsDef$location_type <- "Point"
    
    glob_attr <- create_global_attributes(title="title test", institution="test", source="test", catchment="dummy", comment="none")

    snc <- create_efts(tempNcFname, time_dim_info, create_variable_definitions(varsDef), 
      stations_ids, nc_attributes=glob_attr, lead_length = nLead, ensemble_length = nEns, lead_time_tstep=lead_time_tstep)
        
    snc$put_ensemble_forecasts(x, variable_name = v1, identifier = s1, start_time = tested_fcast_issue_time)
    snc$put_ensemble_forecasts(y, variable_name = v2, identifier = s2, start_time = tested_fcast_issue_time)
    
    r1 <- snc$get_ensemble_forecasts(variable_name = v1, identifier = s1, start_time = tested_fcast_issue_time)
    r2 <- snc$get_ensemble_forecasts(variable_name = v2, identifier = s2, start_time = tested_fcast_issue_time)
    expect_equal(as.numeric(r1[2, 2]), 6)
    expect_equal(as.numeric(r2[2, 2]), 18)
    snc$close()
  })

  if(lead_time_tstep == "hours") {
    lead_ts <- lubridate::dhours
  } else if (lead_time_tstep == "days") {
    lead_ts <- lubridate::ddays
  }
  
  test_that(paste0("Can read back a small EFTS netCDF file", case_params), {
    snc <- open_efts(tempNcFname)
    r1 <- snc$get_ensemble_forecasts(variable_name = v1, identifier = s1, start_time = tested_fcast_issue_time)
    r2 <- snc$get_ensemble_forecasts(variable_name = v2, identifier = s2, start_time = tested_fcast_issue_time)
    expect_equal(as.numeric(r1[2, 2]), 6)
    expect_equal(as.numeric(r2[2, 2]), 18)
    # Check the lead time axix:
    fcast_timeaxis <- index(r1)
    expect_equal(fcast_timeaxis[1], tested_fcast_issue_time + lead_ts(lead_time_step_start_offset))
    expect_equal(fcast_timeaxis[2], tested_fcast_issue_time + lead_ts(lead_time_step_start_offset + lead_time_step_delta))
    snc$close()
  })
}  # end doTests


tested_fcast_issue_time <- timeAxisStart + lubridate::ddays(2L)

# Covers https://github.com/jmp75/efts/issues/6
tempNcFname <- tempfile()
tryCatch(doTests(lead_time_tstep = "days", time_step = "days since", time_step_delta = 1L, lead_time_step_start_offset = 1L, lead_time_step_delta = 1L), finally = function() {
  if (file.exists(tempNcFname)) 
    file.remove(tempNcFname)
}) 

tested_fcast_issue_time <- timeAxisStart + lubridate::dhours(6L)

tempNcFname <- tempfile()
tryCatch(doTests(lead_time_tstep = "hours", time_step = "hours since", time_step_delta = 1L, lead_time_step_start_offset = 1L, lead_time_step_delta = 1L), finally = function() {
  if (file.exists(tempNcFname)) 
    file.remove(tempNcFname)
}) 

tempNcFname <- tempfile()
tryCatch(doTests(lead_time_tstep = "hours", time_step = "hours since", time_step_delta = 1L, lead_time_step_start_offset = 1L, lead_time_step_delta = 3L), finally = function() {
  if (file.exists(tempNcFname)) 
    file.remove(tempNcFname)
}) 

