## CIENS: Generation of exemplary data frame

#### Initiation ####
# Path to of CIENS data
data_path <- "F:/W2W_C5/publication_data/"

# Path to Github repository functions
git_path <- "C:/Users/schulz/Documents/GitHub/CIENS/"
  
# Load functions
setwd(git_path)
source(file = paste0(git_path, "functions.R"))

# Load additional data
load(file = paste0(git_path, "ciens_meta_data.RData"))

# Load packages
library(lubridate)
library(dplyr)
library(ncdf4)

#### Data set composition ####
# Define initialization times
tm_vec <- init_vec[year(init_vec) == 2016]

# Define variables of interest
met_vars <- c("VMAX_10M", "VMAX_10M_LS_S", "T_G")
obs_vars <- c("wind_speed_of_gust", "air_temperature")

# Generate data frame for multiple initialization times
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_data$station_id[1:5],
                                                    step_vec = c(12:14),  
                                                    ens_vec = c(1:5))))
df <- lapply(tm_vec, function(x) get_init(tm = x,
                                          dir_path = data_path,
                                          met_vars = met_vars,
                                          obs_vars = obs_vars,
                                          location_vec = loc_data$station_id[1:3],
                                          step_vec = c(11:13)))

#### Ensemble histogram, scores, ensemble mean ####
# Calculate ensemble mean and standard deviation
temp_var <- "VMAX_10M"
df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)

# Make a plot of ensemble with observations

# Generate verification rank histogram
