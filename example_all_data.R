## CIENS: Generation of exemplary data frame
## Both initialization times with spatial variables


#### Initialization ####
# Path to Github repository functions
git_path <- "C:/Users/schulz/Documents/GitHub/CIENS/"

# Set working directory
setwd(git_path)

# Initiate
source(file = paste0(getwd(), "/init_file.R"))

#### Get data ####
# Define period of initialization times
tm_vec <- init_vec[(year(init_vec) == 2015) | (year(init_vec) == 2016)]

# Define variables of interest including spatial variable
met_vars <- c("VMAX_10M", "VMAX_10M_MS", "T_2M")
obs_vars <- c("wind_speed_of_gust", "air_temperature")

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
                                                    step_vec = c(12:14),  
                                                    ens_vec = ens_vec)))
## -> Save R-data for faster access

# Omit missing values
df <- na.omit(df)

# For-Loop over meteorological variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}

#### Calculate scores ####
# Calculate CRPS values of station forecast and spatial forecast
crps_wg <- crps_sample(y = df[,"wind_speed_of_gust"], 
                       dat = as.matrix(df[,paste0("VMAX_10M_", 1:n_ens)]))
crps_t2m <- crps_sample(y = df[,"air_temperature"], 
                        dat = as.matrix(df[,paste0("T_2M_", 1:n_ens)]))

# Summary of results
summary(data.frame("wg_crps" = crps_wg, "t2m_crps" = crps_t2m, 
                   "wg_sd" = df[["VMAX_10M_sd"]], "wg_t2m" = df[["T_2M_sd"]]))

#### Generate verification rank histograms ####
# Plot verification rank histograms for both variables
par(mfrow = c(1, 2))

# For-Loop over observational variables
for(temp_var in obs_vars){ 
  # Get name of corresponding forecast variable
  if(temp_var == "wind_speed_of_gust"){ fc_var <- "VMAX_10M" }
  else if(temp_var == "air_temperature"){ fc_var <- "T_2M" }
  
  # Calculate ranks
  df[[paste0(temp_var, "_rank")]] <- apply(df[,c(temp_var, paste0(fc_var, "_", ens_vec))], 1, 
                                           function(x){ rank(x, ties = "random")[1] })
  
  # Frequency of bins
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == df[[paste0(temp_var, "_rank")]]) })
  
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

#### Postprocessing via EMOS ####
# Load EMOS functions
source(file = paste0(git_path, "functions_emos.R"))

# Consider only Karlsruhe and 12 hour
df_ka <- subset(df, (location == "10731") & (step == 12))

# Split in training (2015) and test period (2016)
i_train <- which(year(df_ka[["init_tm"]]) == 2015)
i_test <- which(year(df_ka[["init_tm"]]) == 2016)

## Temperature postprocessing
# Train EMOS model using temperature and wind gust predictions
tp_emos_train <- emos_est(train = df_ka[i_train,],
                          loc_pred = "T_2M_mean",
                          scale_pred = "VMAX_10M_mean", # Use gusts as predictor for scale
                          target = "air_temperature",
                          distr = "norm")

# Show EMOS output (Consider parameterization!)
print(tp_emos_train)

# Train EMOS model using wind gust ensemble predictions
tp_emos_pred <- emos_pred(X = df_ka[i_test,],
                     loc_pred = "T_2M_mean",
                     scale_pred = "VMAX_10M_mean",
                     target = "air_temperature",
                     distr = "norm",
                     par_emos = tp_emos_train$par)

# Ensemble CRPS for Karlsruhe
crps_ens <- crps_sample(y = df_ka[i_test, "air_temperature"], 
                        dat = as.matrix(df_ka[i_test, paste0("T_2M_", 1:n_ens)]))

# Summary of CRPS via boxplot of differences
boxplot(crps_ens-tp_emos_pred$scores$crps)
## Positive values indicate improvement

# Equal performance
abline(h = 0, 
       lty = 2, 
       col = "grey")

# Skill in percentage
print(1 - mean(tp_emos_pred$scores$crps)/mean(crps_ens))
## CRPS improvement by ~4%

# Generate PIT histograms
hist(tp_emos_pred$scores$pit,
     freq = FALSE,
     xlab = "PIT",
     main = "EMOS, temperature: Karlsruhe")

# Indicating calibration
abline(h = 1, 
       lty = 2, 
       col = "grey")


