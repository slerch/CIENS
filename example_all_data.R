## CIENS: Generation of exemplary data frame
## Both initialization times with spatial variables

## Note that you require all folders "run0", "run0_spatial", "run12", "run12_spatial", "observations" for this code to work. 

#### Initialization ####

# Path to Github repository functions
git_path <- "C:/CIENS/R Code/"

# Set working directory
setwd(git_path)

# Source R Code file for initializing
source(file = paste0(getwd(), "/init_file.R"))


#### Get data: Construct data.frame from netcdf files according to your needs ####

# Define initialization dates that are supposed to be in the created data.frame, 
# in this example: dates in the years 2016 and 2016.
tm_vec <- init_vec[(year(init_vec) == 2015) | (year(init_vec) == 2016)]

# Define observational and meteorological variables of interest, here
# wind gust forecasts and observations, 2m surfacte temperature forecasts and observations,
# including one spatial variable (VMAX_10M_MS).
met_vars <- c("VMAX_10M", "VMAX_10M_MS", "T_2M")
obs_vars <- c("wind_speed_of_gust", "air_temperature")

# Get locations of interest:
# Karlsruhe-Rheinstetten (10731)
# Offenbach-Wetterpark (10641)
loc_vec <- loc_data$station_id[is.element(loc_data$station_id, c("10731", "10641"))]

# Generate data.frame df containing both initialization times 00 and 12 UTC, 
# and multiple forecast horizons, here horizons 12h, 13h, and 14h ahead,
# for the 2 stations specified above, with dates in the years 2015 and 2016. 
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = c(12:14),  
                                                    ens_vec = ens_vec)))
            
## -> Consider to save resulting data.frame for faster access later

# Omit missing values
df <- na.omit(df)

# For-Loop over meteorological variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry and
  # add these 2 quantities as new columns to data.frame
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}

                       
#### Calculate scores ####
                       
# Calculate CRPS values of (raw) station forecast and spatial forecast
# via the sample crps function in scoringRules package
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

# For-Loop over observational variables (here 2 variables in the example) in data.frame df
# to extract the ranks corresponding to each obs. variable and count how often each rank 1,...,21
# of observation within the corresponding forecast ensemble 
# appears over the forecast cases for the current obs. variable. 
for(temp_var in obs_vars){ 
  # Get name of corresponding forecast variable
  if(temp_var == "wind_speed_of_gust"){ fc_var <- "VMAX_10M" }
  else if(temp_var == "air_temperature"){ fc_var <- "T_2M" }
  
  # Calculate ranks of observation for current obs. variable within corresponding ensemble 
  # for all forecast cases considered in the data.frame, 
  # for the 20 member ensemble in this data, resulting ranks can be in {1,...,21}.
  # Add calculated ranks as new column to data.frame df. 
  df[[paste0(temp_var, "_rank")]] <- apply(df[,c(temp_var, paste0(fc_var, "_", ens_vec))], 1, 
                                           function(x){ rank(x, ties = "random")[1] })
  
  # Frequency of bins, that is, how often does rank 1,...,21 appear over all cases for
  # obs. variable temp_var
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == df[[paste0(temp_var, "_rank")]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # (Relative) Frequencies
          main = temp_var, 
          xlab = "Rank", 
          names.arg = 1:(n_ens + 1),
          space = 0)
  
  # Indicating calibration (discrete uniform distribution)
  abline(h = 1/(length(ens_vec) + 1), 
         lty = 2, 
         col = "grey")
}

                       
#### Postprocessing via EMOS ####
                       
# Load EMOS functions from Github repository. 
# The functions are based on the package crch, however, in the EMOS functions
# available here, only two different distributions are currently implemented, 
# the normal and the truncated logistic distribution. 
# There is a function to fit the EMOS model (based on CRPS minimization), 
# a function to predict with a fitted EMOS model,
# and another function to compute scores.                                         
source(file = paste0(git_path, "functions_emos.R"))

# Consider only Karlsruhe and 12 hour ahead forecasts
df_ka <- subset(df, (location == "10731") & (step == 12))

# Split in training (2015) and test period (2016)
i_train <- which(year(df_ka[["init_tm"]]) == 2015)
i_test <- which(year(df_ka[["init_tm"]]) == 2016)

                       
## Temperature postprocessing ##
                       
# Train EMOS model for postprocessing temperature using temperature and wind gust 
# ensemble forecasts.
# For the EMOS fit function you need to specify the data.frame with training data,
# the predictors used for the location parameter, as well as the predictors used for 
# the scale parameter of the predictive distribution (according to crch functionality).
# Furthermore, you need to specify the response (observational) variable,
# and the parametric distribution family, here normal distribution for temperature.  
tp_emos_train <- emos_est(train = df_ka[i_train,],
                          loc_pred = "T_2M_mean",
                          # Use temp ensemble mean as predictor for location
                          scale_pred = "VMAX_10M_mean", # Use gusts as predictor for scale
                          # Use wind gust ensemble mean as predictor for scale
                          target = "air_temperature",
                          # Use observed temp as target/response
                          distr = "norm")

# Show EMOS output (Consider parameterization!)
print(tp_emos_train)

# Predict with fitted EMOS model from above using temperature and wind gust 
# ensemble forecasts from test data with the EMOS predict function. 
# Additionally this function requires the fitted parameters from above as input. 
tp_emos_pred <- emos_pred(X = df_ka[i_test,],
                     loc_pred = "T_2M_mean",
                     scale_pred = "VMAX_10M_mean",
                     target = "air_temperature",
                     distr = "norm",
                     par_emos = tp_emos_train$par)

# Ensemble CRPS for Karlsruhe computed with scoringRules package
crps_ens <- crps_sample(y = df_ka[i_test, "air_temperature"], 
                        dat = as.matrix(df_ka[i_test, paste0("T_2M_", 1:n_ens)]))

# Summary of CRPS via boxplot of differences
boxplot(crps_ens-tp_emos_pred$scores$crps)
## Positive values indicate improvement

# Equal performance (differences 0)
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

# Indicating calibration (continuous uniform distribution)
abline(h = 1, 
       lty = 2, 
       col = "grey")


