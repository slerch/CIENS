## CIENS: Generation of exemplary data frame
## 00 UTC forecasts with spatial variables

## Note that you require at least the folders "run0", "run0_spatial" and "observations" for this code to work. 

#### Initialization ####

# Path to Github repository functions
git_path <- "C:/CIENS/R Code/"

# Set working directory
setwd(git_path)

# Source R Code file for initializing
source(file = paste0(getwd(), "/init_file.R"))


#### Get data: Construct data.frame from netcdf files according to your need ####

# Restrict to initialization times at 00 UTC
tm_vec <- init_vec[hour(init_vec) == 00]

# Define period of observation/forecast dates that are supposed to be in the created data.frame, 
# in this example consider forecasts/observation dates in the year 2019.
tm_vec <- tm_vec[year(tm_vec) == 2019]

# Define observational and meteorological variables of interest, 
# wind gust forecasts and observations, including a spatial variable (VMAX_10M_MS).
met_vars <- c("VMAX_10M", "VMAX_10M_MS")
obs_vars <- c("wind_speed_of_gust")

# Get locations of interest:
# Karlsruhe-Rheinstetten (10731)
# Offenbach-Wetterpark (10641)
loc_vec <- loc_data$station_id[is.element(loc_data$station_id, c("10731", "10641"))]

# Generate data.frame df for initialization time 00 UTC, 
# and multiple forecast horizons, here for horizons 18h,19h,20h,21h ahead,
# for the 2 stations specified above, with dates in the year 2019.
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = c(18:21),  
                                                    ens_vec = ens_vec)))
                       
## -> Consider to save resulting data.frame for faster access later

# For-Loop over meteorological variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry and
  # add these 2 quantities as new columns to data.frame df. 
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}

                       
#### Calculate scores ####
                       
# # Calculate CRPS values of (raw) station forecast and spatial forecast
# via the sample crps function in scoringRules package. 
crps0 <- crps_sample(y = df[,"wind_speed_of_gust"], 
                     dat = as.matrix(df[,paste0("VMAX_10M_", 1:n_ens)]))
crps_spatial <- crps_sample(y = df[,"wind_speed_of_gust"], 
                            dat = as.matrix(df[,paste0("VMAX_10M_MS_", 1:n_ens)]))

# Summary of results
df_eval <- data.frame("crps_local" = crps0, "crps_spatial" = crps_spatial, 
                      "crps_diff" = crps0 - crps_spatial, 
                      "ens_sd_local" = df[["VMAX_10M_sd"]], "ens_sd_spatial" = df[["VMAX_10M_MS_sd"]], 
                      "ens_sd_diff" = df[["VMAX_10M_sd"]] - df[["VMAX_10M_MS_sd"]])

# Boxplots of the values (forecast cases) of each of the columns of above data.frame df_eval.
par(mfrow = c(1, 1))

# Boxplots without outliers
boxplot(df_eval, outline = FALSE)

# Draw dashed line at 0, indicating cases where the crps/the sd of the station
# forecasts (local) and the spatial forecasts (spatial) are equal.
abline(h = 0, 
       lty = 2, 
       col = "grey")

                       
#### Generate verification rank histograms ####
                       
# Plot histograms for both stations and variables
par(mfrow = c(1, 2))

# For-Loop over meteorological variables (here 2 in the example) in data.frame df
# to extract the ranks corresponding to each met. variable and count how often each rank 1,...,21
# appears over the forecast cases for each of the met. variables. 
for(temp_var in met_vars){ 
  # Calculate ranks of observation within ensemble for all forecast cases considered in the 
  # data.frame df, for the 20 member ensemble in this data, resulting ranks can be in {1,...,21}.
  # Add calculated ranks as new column to data.frame df. 
  df[[paste0(temp_var, "_rank")]] <- apply(df[,c("wind_speed_of_gust", paste0(temp_var, "_", ens_vec))], 1, 
                                           function(x){ rank(x, ties = "random")[1] })
  
  # Frequency of bins, that is, how often does rank 1,...,21 appear over all cases for
  # met. var temp_var
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == df[[paste0(temp_var, "_rank")]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # (Relative) Frequencies
          ylim = c(0, 0.4),
          main = temp_var, 
          xlab = "Rank", 
          names.arg = 1:(n_ens + 1),
          space = 0)
  
  # Indicating calibration (discrete uniform distribution)
  abline(h = 1/(length(ens_vec) + 1), 
         lty = 2, 
         col = "grey")
}
