## CIENS: Generation of exemplary data frame
## 00 UTC forecasts with spatial variables


# Path to Github repository functions
git_path <- "C:/Users/schulz/Documents/GitHub/CIENS/"

# Set working directory
setwd(git_path)

# Initiate
source(file = paste0(getwd(), "/example_initiation.R"))

# Restrict to initialization times at 00 UTC
tm_vec <- init_vec[hour(init_vec) == 00]

# Define period of initialization times
tm_vec <- tm_vec[year(tm_vec) == 2019]

# Define variables of interest including spatial variable
met_vars <- c("VMAX_10M", "VMAX_10M_MS")
obs_vars <- c("wind_speed_of_gust")

# Get locations of interest:
# Karlsruhe-Rheinstetten (10731)
# Offenbach-Wetterpark (10641)
loc_vec <- loc_data$station_id[is.element(loc_data$station_id, c("10731", "10641"))]

# Generate data frame for multiple initialization times
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = c(18:21),  
                                                    ens_vec = ens_vec)))

# For-Loop over meteorological variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}

### Calculate scores ###
# Calculate CRPS values of station forecast and spatial forecast
crps0 <- crps_sample(y = df[,"wind_speed_of_gust"], 
                     dat = as.matrix(df[,paste0("VMAX_10M_", 1:n_ens)]))
crps_spatial <- crps_sample(y = df[,"wind_speed_of_gust"], 
                            dat = as.matrix(df[,paste0("VMAX_10M_MS_", 1:n_ens)]))

# Summary of results
df_eval <- data.frame("crps_local" = crps0, "crps_spatial" = crps_spatial, 
                      "ens_sd_local" = df[["VMAX_10M_sd"]], "ens_sd_spatial" = df[["VMAX_10M_MS_sd"]])

# Boxplots without outliers
boxplot(df_eval, outline = FALSE)

### Generate verification rank histogram ###
# Plot histograms for both stations and variables
par(mfrow = c(1, 2))

# For-Loop over observational variables
for(temp_var in met_vars){ 
  # Calculate ranks
  df[[paste0(temp_var, "_rank")]] <- apply(df[,c("wind_speed_of_gust", paste0(temp_var, "_", ens_vec))], 1, 
                                           function(x){ rank(x, ties = "random")[1] })
  
  # Frequency of bins
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == df[[paste0(temp_var, "_rank")]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # Frequencies
          ylim = c(0, 0.4),
          main = temp_var, 
          xlab = "Rank", 
          names.arg = 1:(n_ens + 1),
          space = 0)
  
  # Indicating calibration
  abline(h = 1/(length(ens_vec) + 1), 
         lty = 2, 
         col = "grey")
}
