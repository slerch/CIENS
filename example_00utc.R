## CIENS: Generation of exemplary data frame
## 00 UTC forecasts


# Path to Github repository functions
git_path <- "C:/Users/schulz/Documents/GitHub/CIENS/"

# Set working directory
setwd(git_path)

# Initiate
source(file = paste0(getwd(), "/example_initiation.R"))

# Restrict to initialization times at 00 UTC
tm_vec <- init_vec[hour(init_vec) == 00]

# Define period of initialization times
tm_vec <- tm_vec[(year(tm_vec) == 2015) | (year(tm_vec) == 2016)]

# Define variables of interest
met_vars <- c("VMAX_10M")
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
                                                    step_vec = c(12:13),  
                                                    ens_vec = ens_vec)))

# Calculate ensemble mean and standard deviation for each entry
df[["VMAX_10M_mean"]] <- rowMeans(df[,paste0("VMAX_10M_", ens_vec)])
df[["VMAX_10M_sd"]] <- apply(df[,paste0("VMAX_10M_", ens_vec)], 1, sd)

### Generate verification rank histogram ###
# Plot histograms for both stations and variables
par(mfrow = c(1, 2))

# Calculate ranks
df[["wind_speed_of_gust_rank"]] <- apply(df[,c("wind_speed_of_gust", paste0("VMAX_10M_", ens_vec))], 1, 
                                       function(x){ rank(x, ties = "random")[1] })

# For-Loop over locations
for(temp_loc in loc_vec){
  # Frequency of bins
  rh <- sapply(1:(n_ens+1), function(i){ 
    sum(i == subset(df, location == temp_loc)[["wind_speed_of_gust_rank"]]) })
  
  # Plot histogram via barplot
  barplot(height = rh/sum(rh), # Frequencies
          main = paste0("wind_speed_of_gust: ", temp_loc), 
          xlab = "Rank", 
          names.arg = 1:(n_ens + 1),
          space = 0)
  
  # Indicating calibration
  abline(h = 1/(length(ens_vec) + 1), 
         lty = 2, 
         col = "grey")
}

### Postprocessing application via EMOS ###
# Load EMOS functions
source(file = paste0(git_path, "functions_emos.R"))

# Consider only Karlsruhe and 12 hour
df_ka <- subset(df, (location == "10731") & (step == 12))

# Split in training (2015) and test period (2016)
i_train <- which(year(df_ka[["init_tm"]]) == 2015)
i_test <- which(year(df_ka[["init_tm"]]) == 2016)

# Ensemble CRPS for Karlsruhe
crps_ens <- crps_sample(y = df_ka[i_test, "wind_speed_of_gust"], 
                        dat = as.matrix(df_ka[i_test, paste0("VMAX_10M_", 1:n_ens)]))

# Train EMOS model using wind gust ensemble predictions
wg_emos_train <- emos_est(train = df_ka[i_train,],
                          loc_pred = "VMAX_10M_mean",
                          scale_pred = "VMAX_10M_sd",
                          target = "wind_speed_of_gust",
                          distr = "tlogis")

# Show EMOS output (Consider parameterization!)
print(wg_emos_train)

# Train EMOS model using wind gust ensemble predictions
wg_emos_pred <- emos_pred(X = df_ka[i_test,],
                     loc_pred = "VMAX_10M_mean",
                     scale_pred = "VMAX_10M_sd",
                     target = "wind_speed_of_gust",
                     distr = "tlogis",
                     par_emos = wg_emos_train$par)

# Summary of CRPS
summary(data.frame("Ensemble" = crps_ens, 
                   "EMOS" = wg_emos_pred$scores$crps))

# Skill score in percentage
print(round(100*(1 - mean(wg_emos_pred$scores$crps)/mean(crps_ens)), 2))

# Plot only PIT histogram
par(mfrow = c(1, 1))

# Generate PIT histograms
hist(wg_emos_pred$scores$pit,
     freq = FALSE,
     xlab = "PIT",
     main = "EMOS, gusts: Karlsruhe")

# Indicating calibration
abline(h = 1, 
       lty = 2, 
       col = "grey")
