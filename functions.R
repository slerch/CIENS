## CIENS: Functions for reading in data from netCDF files

## Required packages:
## - ncdf4
## - dplyr
## - lubridate

# Function that converts string format of the data to time
str2time <- function(str_tm, tm_format = "%Y%m%d%H"){
  ###-----------------------------------------------------------------------------
  ###Input
  #str_tm......String representing the time (string)
  #tm_format...Date format of string (string)
  #............Default: "%Y%m%d%H" -> used in COSMO data set
  ###-----------------------------------------------------------------------------
  ###Output
  #tm...String transformed to time format/POSIXct (list; "time"/POSIXct type)
  ###-----------------------------------------------------------------------------
  
  # Convert time to string in given format
  return(strptime(str_tm, format = tm_format, tz = "UTC"))
}

# Function that converts time to the string format of the data
time2str <- function(tm, tm_format = "%Y%m%d%H"){
  ###-----------------------------------------------------------------------------
  ###Input
  #tm..........Time to be transformed (list; "time"/POSIXct type)
  #tm_format...Date format of string (string)
  #............Default: "%Y%m%d%H" -> used in COSMO data set
  ###-----------------------------------------------------------------------------
  ###Output
  #str...String representing the time to read out from data (string)
  ###-----------------------------------------------------------------------------
  
  # Convert time to string in given format
  return(format(tm, tm_format))
}

# Get ensemble forecasts and observations for a given initialization time
get_init <- function(tm, dir_path,
                     location_vec = NULL,
                     step_vec = 0:21,  
                     ens_vec = 1:20, 
                     met_vars = c("VMAX_10M"), 
                     obs_vars = c("wind_speed_of_gust"),
                     console = TRUE){
  ###-----------------------------------------------------------------------------
  ###Input
  #tm.............Initialization time of forecasts (time object)
  #dir_path.......Path to netCDF-directory (string)
  #location_vec...Locations of interest (String vector)
  #...............Choose locations via station ID
  #...............Default: NULL -> Get all locations
  #step_vec.......Lead times of interest (Integer vector)
  #...............Choose lead times from 0,...,21 hours
  #...............Default: 0:21 -> Get all steps
  #ens_vec........Ensemble members of interest (Integer vector)
  #...............Choose members from 1,...,20
  #...............Default: 1:20 -> All ensemble members
  #met_vars.......Meteorological variables of interest (String vector)
  #...............Choose meteorological variable via abbreviation
  #...............Default: 1:20 -> All ensemble members
  #met_vars.......Observational variables of interest (String vector)
  #...............Choose observational variable via name
  #...............Default: 1:20 -> All ensemble members
  #console........Show current initialization as console output? (Logical)
  #...............Default: TRUE -> Show output
  ###-----------------------------------------------------------------------------
  ###Output
  #res...Data frame with following columns:
  #...init_tm......Initialization time (POSIXct)
  #...step......Lead time resp. forecast step (integer)
  #...location.....Location (String)
  #...obs_tm.......Observation time (POSIXct)
  #...observational variables
  #...ensemble members of meteorological variables variables
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Console output
  if(console){ print(paste0("--- Initialization: ", tm)) }
  
  # Load package
  library(ncdf4)
  library(dplyr)
  library(lubridate)
  
  # Path to observational data
  obs_path <- paste0(dir_path, "observations/")
  
  # Path forecast files
  fc_path <- paste0(dir_path, "run", hour(tm), "/")
  
  # Get name of forecast ncdf file
  temp_file <- paste0(fc_path, year(tm), sprintf("%02d", month(tm)), "/grib_", time2str(tm), ".nc")
  
  # Open corresponding netCDF
  fc_nc <- nc_open(temp_file)
  
  # Get locations
  fc_loc_vec <- ncvar_get(nc = fc_nc,
                          varid = "loc")
  
  # Location default
  if(is.null(location_vec)){ location_vec <- fc_loc_vec }
  
  # Columns of resulting data frame
  col_vec <- c("init_tm", "step", "location", "obs_tm", obs_vars, 
               sapply(met_vars, function(x) paste0(x, "_", ens_vec)))
  
  # Initiate resulting data frame (one row for each location and step)
  res <- as.data.frame(matrix(nrow = length(location_vec)*length(step_vec),
                              ncol = length(col_vec)))
  
  # Set names of variables
  names(res) <- col_vec
  
  #### Get ensemble data ####
  # For-Loop over meteorological variables
  for(temp_var in met_vars[is.element(met_vars, names(fc_nc$var))]){
    # Get ensemble forecasts
    res[,paste0(temp_var, "_", ens_vec)] <- do.call(rbind, lapply(step_vec, function(x){
      t(ncvar_get(nc = fc_nc, varid = temp_var)[ens_vec, (x + 1), is.element(fc_loc_vec, location_vec)]) }))
  }
  
  # Close netCDF file
  nc_close(fc_nc)
  
  # Are spatial variables included?
  if(any(sapply(c("_LS", "_MS"), function(x) grepl(pattern = x, 
                                                   x = met_vars, 
                                                   fixed = TRUE)))){
    # Path forecast files
    fc_path <- paste0(dir_path, "run", hour(tm), "_spatial/")
    
    # Get name of forecast ncdf file
    temp_file <- paste0(fc_path, year(tm), sprintf("%02d", month(tm)), "/grib_", time2str(tm), ".nc")
    
    # Open corresponding netCDF
    fc_nc <- nc_open(temp_file)
    
    # Get locations
    fc_loc_vec <- ncvar_get(nc = fc_nc,
                            varid = "loc")
    
    # For-Loop over meteorological variables
    for(temp_var in met_vars[is.element(met_vars, names(fc_nc$var))]){
      # Get ensemble forecasts
      res[,paste0(temp_var, "_", ens_vec)] <- do.call(rbind, lapply(step_vec, function(x){
        t(ncvar_get(nc = fc_nc, varid = temp_var)[ens_vec, (x + 1), is.element(fc_loc_vec, location_vec)]) }))
    }
    
    # Close netCDF file
    nc_close(fc_nc)
  }
  
  #### Fill data frame ####
  # Get initialization time
  res[,"init_tm"] <- time2str(tm)
  
  # Get forecast step / lead time
  res[,"step"] <- rep(x = step_vec,
                         each = length(location_vec))
  
  # Get locations
  res[,"location"] <- rep(x = fc_loc_vec[is.element(fc_loc_vec, location_vec)],
                          times = length(step_vec))
  
  # Get observation time
  res[,"obs_tm"] <- time2str(tm + hours(res[,"step"]))
  
  # Transform time variables (str2time makes a POSIXlt object)
  res <- res %>% mutate(init_tm = as.POSIXct(init_tm, format = '%Y%m%d%H', tz = "UTC")) %>%
    mutate(obs_tm = as.POSIXct(obs_tm, format = '%Y%m%d%H', tz = "UTC"))
  
  #### Get observational data ####
  ## Special case: 31-12-20xx and steps of 12h and larger
  ## Year of initialization and observation are not equal
  # Get years of observations
  years_vec <- unique(year(tm + hours(step_vec)))
  
  # For-Loop over years
  for(temp_year in years_vec){
    # Get indices in corresponding year
    i_year <- which(year(res[["obs_tm"]]) == temp_year)
    
    # Get name of observational ncdf file
    temp_file <- paste0(obs_path, "obs-", temp_year, ".nc")
    
    # Open netCDF of corresponding year
    obs_nc <- nc_open(temp_file)
    
    # Get observation times
    obs_tm_vec <- as.POSIXct(ncvar_get(nc = obs_nc,
                                       varid = "time"),
                             origin = "1970-01-01", tz = "UTC")
    
    # Get observation locations
    obs_loc_vec <- ncvar_get(nc = obs_nc,
                             varid = "station_id")
    
    # For-Loop over observational variables
    for(temp_var in obs_vars){
      # Read out observational variables
      obs <- ncvar_get(nc = obs_nc,
                       varid = temp_var)
      
      # Assign observations 
      res[[temp_var]][i_year] <- sapply(i_year, function(i){ 
        obs[which(res[i, "location"] == obs_loc_vec),
            which(res[i, "obs_tm"] == obs_tm_vec)] })
    }
    
    # Close nc file
    nc_close(obs_nc)
  }
  
  #### Output ####
  # Return data frame
  return(res)
}
