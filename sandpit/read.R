library(devtools)
load_all('/home/per202/src/github_jm/efts')

# id <- as.character(zoo::index(d))
# s <- which(id == "2006-01-01 00:00:00")
# [1] 140267
# e <- which(id == "2006-12-31 23:00:00")
# [1] 149026
# ncks -d time,140266,149025 ~/Data/UnitTests/Obs_data/Upper_Murray_rain_1hr.nc Upper_Murray_sample_rain.nc

ext_data <- system.file('extdata', package='efts')
rain_file <- file.path(ext_data, 'Upper_Murray_sample_rain.nc')

stopifnot(file.exists(rain_file))

rain_dat <- open_efts(rain_file)
rain_dat

rain_dat$get_lead_time_count()
rain_dat$get_station_count()

d <- rain_dat$get_all_series(variable_name = 'rain_der')
str(d)

# How do we silence 
# Warning message:
# timezone of object (UTC) is different than current timezone (). 

plot(d[1:48], main="Interpolated rainfall (mm/h)")

d_q <- rain_dat$get_all_series(variable_name = 'rain_der_qual')
plot(d_q, main="Interpolated rainfall quality code")

# Note: the following is not enough as an error msg:
# > d <- rain_dat$get_all_series()
# Error: variable_name %in% names(ncfile$var) is not TRUE

close_efts(rain_dat)


# cd ~/Data/UnitTests/Fct_Data/
# cd tmp/
# rm *
# ncrcat ../Upper_Murray_F1_1_2010080121_shuffle.nc ../Upper_Murray_F1_1_2010080221_shuffle.nc tmp_collate.nc
# cd ~/src/github_jm/efts/inst/extdata/
# ncks -d ens_member,1,10 ~/Data/UnitTests/Fct_Data/tmp/tmp_collate.nc Upper_Murray_sample_ensemble_rain_fcast.nc

rain_fct_ens <- open_efts('~/src/github_jm/efts/inst/extdata/Upper_Murray_sample_ensemble_rain_fcast.nc')
rain_fct_ens

all_vars_names <- rain_fct_ens$get_variable_names()
station_ids <- rain_fct_ens$get_values("station_id")
variable_names <- "rain_fcast_ens"

issue_times <- rain_fct_ens$get_time_dim()

ensfcasts = list()
ensfcasts[["1"]] <- rain_fct_ens$get_ensemble_forecasts(variable_names[1], station_ids[1], start_time = issue_times[1])
ensfcasts[["2"]] <- rain_fct_ens$get_ensemble_forecasts(variable_names[1], station_ids[1], start_time = issue_times[2])

# Note: method with signature ‘Duration#ANY’ chosen for function ‘*’,
#  target signature ‘Duration#array’.
#  "vector#structure" would also be valid

d <- ensfcasts[["1"]]
# TODO: would be nice to have the title retrieved, but not realistic for multivariate files
dat_desc <- "Post Processed ACCESS-G (APS1) Rainfall Forecasts"
plot(d[,1:3], main=paste0(dat_desc, " (mm/h)"))

d <- ensfcasts[["2"]]
# TODO: would be nice to have the title retrieved, but not realistic for multivariate files
dat_desc <- "Post Processed ACCESS-G (APS1) Rainfall Forecasts"
plot(d[,1:3], main=paste0(dat_desc, " (mm/h)"))

close_efts(rain_fct_ens)