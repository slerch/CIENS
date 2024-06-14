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
tm_vec <- init_vec[(year(init_vec) == 2015) | (year(init_vec) == 2016)]

# Define variables of interest
met_vars <- c("VMAX_10M", "VMAX_10M_MS", "T_2M")
obs_vars <- c("wind_speed_of_gust", "air_temperature")

# Get locations of interest
loc_vec <- loc_data$station_id[is.element(loc_data$name, c("Rheinstetten", "Offenbach-Wetterpark"))]

# Generate data frame for multiple initialization times
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = c(12:13),  
                                                    ens_vec = ens_vec)))

#### Exemplary applications ####
### Calculate ensemble statistics
# For-Loop over meteorological variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}

### Generate verification rank histogram
# Plot both histograms with two panels
par(mfrow = c(1, 2))

# Get ensemble size
n_ens <- length(ens_vec)

# For-Loop over observational variables
for(temp_var in obs_vars){
  # Get name of corresponding forecast variable
  if(temp_var == "wind_speed_of_gust"){ fc_var <- "VMAX_10M" }
  else if(temp_var == "air_temperature"){ fc_var <- "T_2M" }
  
  # Calculate ranks
  df[[paste0(temp_var, "_rank")]] <- apply(df[,c(temp_var, paste0(fc_var, "_", ens_vec))], 1, 
                                           function(x){ rank(x, ties = "random")[1] })
  
  # Frequency of bins
  rh <- sapply(1:(n_ens+1), function(i){ sum(i == df[[paste0(temp_var, "_rank")]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # Frequencies
          main = temp_var, 
          xlab = "Rank", 
          names.arg = 1:(n_ens + 1),
          space = 0)
  
  # Indicating calibration
  abline(h = 1/(length(ens_vec) + 1), 
         lty = 2, 
         col = "grey")
  
}


### Calculate scores
# Required package for CRPS calculation
library(scoringRules)

# ...


#### Postprocessing application via EMOS ####
