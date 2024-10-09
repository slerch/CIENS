## CIENS: Generation of exemplary data frame
## 00 UTC forecasts

## Note that you require at least the folder "run0" and the folder "observations" for this code to work. 

#### Initialization ####

# Path to Github repository functions
git_path <- "C:/CIENS/R Code/"

# Set working directory
setwd(git_path)

# Source R file for initializing
source(file = paste0(getwd(), "/init_file.R"))


#### Get data: Construct data.frame from netcdf files according to your needs ####

# Restrict to initialization times at 00 UTC
tm_vec <- init_vec[hour(init_vec) == 00]

# Define period of observation/forecast dates that are supposed to be in the created data.frame.
# In this example: forecasts/observation dates in the years 2015 and 2016.
tm_vec <- tm_vec[(year(tm_vec) == 2015) | (year(tm_vec) == 2016)]

# Define observational and meteorological variables of interest, 
# here wind gust forecasts and observations.
met_vars <- c("VMAX_10M")
obs_vars <- c("wind_speed_of_gust")

# Get locations of interest:
# Karlsruhe-Rheinstetten (10731)
# Offenbach-Wetterpark (10641)
loc_vec <- loc_data$station_id[is.element(loc_data$station_id, c("10731", "10641"))]

# Generate data.frame df for initialization time 00 UTC, and multiple forecast horizons, 
# here for horizons 12h and 13h ahead,
# for the 2 stations specified above, with dates in the years 2015 and 2016. 
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = c(12:13),  
                                                    ens_vec = ens_vec)))

## -> Consider to save resulting data.frame for faster access later

# Calculate ensemble mean and standard deviation for each entry and
# add these 2 quantities as new columns to data.frame df. 
df[["VMAX_10M_mean"]] <- rowMeans(df[,paste0("VMAX_10M_", ens_vec)])
df[["VMAX_10M_sd"]] <- apply(df[,paste0("VMAX_10M_", ens_vec)], 1, sd)

                       
#### Generate verification rank histograms ####
                       
# Plot histograms for both stations and variables
par(mfrow = c(1, 2))

# Calculate ranks of observation within ensemble forecasts for all forecast cases considered in the 
# data.frame, for the 20 member ensemble in this data, resulting ranks can be in {1,...,21}.
# Add calculated ranks as new column to data.frame df. 
df[["wind_speed_of_gust_rank"]] <- apply(df[,c("wind_speed_of_gust", paste0("VMAX_10M_", ens_vec))], 1, 
                                       function(x){ rank(x, ties = "random")[1] })

# For-Loop over locations
for(temp_loc in loc_vec){
  # Frequency of bins, that is, how often does rank 1,...,21 appear over all forecast cases for
  # current location temp_loc
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == subset(df, location == temp_loc)[["wind_speed_of_gust_rank"]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # (Relative) Frequencies
          main = paste0("wind_speed_of_gust: ", temp_loc), 
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

# Ensemble CRPS for Karlsruhe computed with package scoringRules
crps_ens <- crps_sample(y = df_ka[i_test, "wind_speed_of_gust"], 
                        dat = as.matrix(df_ka[i_test, paste0("VMAX_10M_", 1:n_ens)]))

# Train EMOS model using wind gust ensemble forecasts. 
# For the EMOS fit function you need to specify the data.frame with training data,
# the predictors used for the location parameter, as well as the predictors used for 
# the scale parameter of the predictive distribution (according to crch functionality).
# Furthermore, you need to specify the response (observational) variable,
# and the parametric distribution family, here truncated logistic for wind speed.
wg_emos_train <- emos_est(train = df_ka[i_train,],
                          loc_pred = "VMAX_10M_mean",
                          scale_pred = "VMAX_10M_sd",
                          target = "wind_speed_of_gust",
                          distr = "tlogis")

# Show EMOS output (Consider parameterization!)
print(wg_emos_train)

# # Predict with fitted EMOS model from above using wind gust ensemble forecasts from test data
# with the EMOS predict function. Additionally this function requires the fitted parameters 
# from above as input. 
wg_emos_pred <- emos_pred(X = df_ka[i_test,],
                     loc_pred = "VMAX_10M_mean",
                     scale_pred = "VMAX_10M_sd",
                     target = "wind_speed_of_gust",
                     distr = "tlogis",
                     par_emos = wg_emos_train$par)

# Summary of CRPS
summary(data.frame("Ensemble CRPS" = crps_ens, 
                   "EMOS CRPS" = wg_emos_pred$scores$crps))

# Skill score in percentage
print(1 - mean(wg_emos_pred$scores$crps)/mean(crps_ens))
## CRPS improvement by ~10%

# Plot only PIT histogram
par(mfrow = c(1, 1))

# Generate PIT histograms
hist(wg_emos_pred$scores$pit,
     freq = FALSE,
     xlab = "PIT",
     main = "EMOS, gusts: Karlsruhe")

# Indicating calibration continuous uniform distribution)
abline(h = 1, 
       lty = 2, 
       col = "grey")
