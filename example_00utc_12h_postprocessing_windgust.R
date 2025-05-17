## CIENS: Generation of exemplary data frame for postprocessing of Wind Gust
## 00 UTC forecasts and 12h ahead lead time
## Station-wise postprocessing with EMOS, EMOS-GB and EMOS-GB-SP

#### Initialization ####

# Path to Github repository example functions
git_path <- "C:/CIENS Data/R Code/"


# Set working directory
setwd(git_path)

# Load R code for initializing
source(file = paste0(getwd(), "/init_file.R"))



#### Construct data.frame from netcdf files ####

# Restrict to initialization times 00 UTC
tm_vec <- init_vec[hour(init_vec) == 00]

# Define period of observation/forecast dates that are supposed to be in the created data.frame, 
# in this example we consider all available forecasts/observation dates in 2010-2023.
tm_vec <- tm_vec[(year(tm_vec) %in% c(2010:2023))]

# Define observational and meteorological variables of interest.
# This example is about postporcessing wind gust forecasts. 
# For Emos-GB all standard and/or spatial forecast variables 
# (contained in fc_var_vec and/or fc_spatvar_vec) will be allowed as predictor set
met_vars <- c(fc_var_vec, fc_spatvar_vec)
obs_vars <- c("wind_speed_of_gust")


# Use all available stations in the dataset
loc_vec <- loc_data$station_id


# Generate data.frame with data from initialization time 00 UTC, 
# here we restrict ourselves to one lead time, namely 12h, we use 
# all available stations (170), with all available dates in the years 2010-2023. 
df <- bind_rows(lapply(tm_vec, function(x) get_init(tm = x,
                                                    dir_path = data_path,
                                                    met_vars = met_vars,
                                                    obs_vars = obs_vars,
                                                    location_vec = loc_vec,
                                                    step_vec = 12,  
                                                    ens_vec = ens_vec)))


# For-Loop over meteorological and spatial variables
for(temp_var in met_vars){
  # Calculate ensemble mean and standard deviation for each entry and
  # add these quantities as new columns to data.frame
  df[[paste0(temp_var, "_mean")]] <- rowMeans(df[,paste0(temp_var, "_", ens_vec)])
  df[[paste0(temp_var, "_sd")]] <- apply(df[,paste0(temp_var, "_", ens_vec)], 1, sd)
}


# For simplicity, remove all non-complete cases (rows with NA) from data.frame
df12 <- df[complete.cases(df),]


# Save data.frame for later use as creating it takes some time
save(df12, file="C:/CIENS Data/R Code/ExampleDataPostprocessingWindGust00UTC12h.RData")




#### Postprocessing via EMOS, EMOS-GB, EMOS-GB-SP ####


# Source EMOS functions from Github repository.
source(file = paste0(git_path, "functions_emos.R"))
source(file = paste0(git_path, "functions_emos_bst.R"))



# Example data set contains mean and sd of standard and of spatial vars,
# however, for EMOS-GB we only allow the standard vars, for EMOS-GB-SP we 
# allow standard and spatial vars, therefore, exract vectors containing
# all vars, only standard vars and only spatial vars
pred_vars_mean <- grep("mean", names(df12), value=TRUE)
pred_vars_sd <- grep("sd", names(df12), value=TRUE)
pred_vars <- c(pred_vars_mean, pred_vars_sd) # in total 270 vars, 55*2 standard, 80*2 spatial

# The spatial (MS and LS) vars  (mean and sd of ensemble forecasts for each var = 2*80=160)
pred_vars_spatial_MS_mean <- grep("_MS_", pred_vars_mean, value=TRUE)
pred_vars_spatial_LS_mean <- grep("_LS_", pred_vars_mean, value=TRUE)
pred_vars_spatial_MS_sd <- grep("_MS_", pred_vars_sd, value=TRUE)
pred_vars_spatial_LS_sd <- grep("_LS_", pred_vars_sd, value=TRUE)
pred_vars_spatial <- c(pred_vars_spatial_MS_mean, pred_vars_spatial_LS_mean,
                       pred_vars_spatial_MS_sd, pred_vars_spatial_LS_sd) # 80 spatial vars

# The standard vars, 55 standard vars (mean and sd of ensemble forecasts for each var = 2*55=110)
std_pred_vars <- pred_vars[!(pred_vars %in% pred_vars_spatial)]
std_pred_vars_emos <- grep("VMAX", std_pred_vars, value=TRUE)




#### Set up data frames to save coefficient values of the postprocessing models for all stations
#### The absolute coef values are later used as a measure of variable/feature importance


# Columns for data frame
df_cols <- c("location", "par", "(Intercept)", pred_vars)

# Set up data frame to save coef values for EMOS
df_emos_fi <- data.frame(matrix(nrow = 2*length(loc_vec), ncol = length(df_cols)))
names(df_emos_fi) <- df_cols

# Set up data frame to save coef values for EMOS-GB
df_emos_bst_fi <- data.frame(matrix(nrow = 2*length(loc_vec), ncol = length(df_cols)))
names(df_emos_bst_fi) <- df_cols

# Set up data frame to save coef values for EMOS-GB-SP
df_emos_bst_allvars_fi <- data.frame(matrix(nrow = 2*length(loc_vec), ncol = length(df_cols)))
names(df_emos_bst_allvars_fi) <- df_cols




#### Set up data frames to save scores for all stations and test dates

crps_ens <- NULL
crps_emos <- NULL
crps_emos_bst <- NULL
crps_emos_bst_allvars <- NULL

logS_ens <- NULL
logS_emos <- NULL
logS_emos_bst <- NULL
logS_emos_bst_allvars <- NULL

mae_ens <- NULL
mae_emos <- NULL
mae_emos_bst <- NULL
mae_emos_bst_allvars <- NULL

mse_ens <- NULL
mse_emos <- NULL
mse_emos_bst <- NULL
mse_emos_bst_allvars <- NULL

rank_ens <- NULL
pit_emos <- NULL
pit_emos_bst <- NULL
pit_emos_bst_allvars <- NULL

lpredint_ens <- NULL
lpredint_emos <- NULL
lpredint_emos_bst <- NULL
lpredint_emos_bst_allvars <- NULL

coverage_ens <- NULL
coverage_emos <- NULL
coverage_emos_bst <- NULL
coverage_emos_bst_allvars <- NULL


# To compute nominal coverage of 1-alpha=(n_ens - 1)/(n_ens+1) prediction interval 
# corresponding to number of ens members
# Here 20 ens members
n_ens <- 20
alpha <- 2/(n_ens + 1) 

# For looping through data frames with variable importance where coef values are saved
i <- 0


# For-Loop over all 170 locations
n.station <- length(loc_vec)
for(k in 1:n.station){
  
  print(k)
  
  df_station <- subset(df12, (location == loc_vec[k]))
  
  # Split in training (2010-2020) and test period (2021-2023)
  i_train <- which(year(df_station[["init_tm"]]) %in% c(2010:2020))
  i_test <- which(year(df_station[["init_tm"]]) %in% c(2021:2023))
  
  
  test_dates <- as.Date(df_station[i_test,]$obs_tm)
  
  
  
  #### Raw Ensemble ######
  
  # Scores for raw ensemble forecasts for station k,
  # some of them computed with package scoringRules
  obs.station <- df_station[i_test, "wind_speed_of_gust"]
  forc.station <- as.matrix(df_station[i_test, paste0("VMAX_10M_", 1:n_ens)])
  crps.ens.all <- crps_sample(y = obs.station, dat = forc.station)
  logS.ens.all <- logs_sample(y = obs.station, dat = forc.station)
  mae.ens.all <- abs(apply(X=forc.station, 1, FUN=median) - obs.station)
  mse.ens.all <- (apply(X=forc.station, 1, FUN=mean) - obs.station)^2
  rank.ens.all <- apply(cbind(obs.station, forc.station), 1, function(x){rank(x, ties = "random")[1]})
  lpredint.ens.all <- apply(t(apply(forc.station, 1, range)), 1, diff) 
  coverage.ens.all <- is.element(rank.ens.all, 2:n_ens)
  
  # Row-bind previous version of data frame with crps values for current station k
  crps_ens <- rbind(crps_ens, data.frame(location=loc_vec[k], date=test_dates, crps=crps.ens.all))
  logS_ens <- rbind(logS_ens, data.frame(location=loc_vec[k], date=test_dates, logS=logS.ens.all))
  mae_ens <- rbind(mae_ens, data.frame(location=loc_vec[k], date=test_dates, mae=mae.ens.all))
  mse_ens <- rbind(mse_ens, data.frame(location=loc_vec[k], date=test_dates, mse=mse.ens.all))
  rank_ens <- rbind(rank_ens, data.frame(location=loc_vec[k], date=test_dates, rank=rank.ens.all))
  lpredint_ens <- rbind(lpredint_ens, data.frame(location=loc_vec[k], date=test_dates, lgt=lpredint.ens.all))
  coverage_ens <- rbind(coverage_ens, data.frame(location=loc_vec[k], date=test_dates, coverage=coverage.ens.all))
  
  
  
  ##### Basic EMOS #######
  
  
  ### Training/Fitting the model ###
  # Train basic EMOS model using summary statistics of wind gust ensemble predictions. 
  # Wind gust observations are the target variable, wind gust ensemble mean and sd
  # are the predictors for linking location and scale to ensemble forecasts.
  # As wind gusts are non-negative, truncated logistic distribution is chosen. 
  wg_emos_train <- emos_est(train = df_station[i_train,],
                            loc_pred = "VMAX_10M_mean",
                            scale_pred = "VMAX_10M_sd",
                            target = "wind_speed_of_gust",
                            distr = "tlogis")
  
  

  ### Collect feature importance information ###
  # Write coefficient values plus further info in data frame
  df_emos_fi[c(k+i, k+i+1), "location"] <- loc_vec[k]
  df_emos_fi[c(k+i, k+i+1), "par"] <- c("loc", "scale")
  # Get coefficients
  df_emos_fi[k+i, c("(Intercept)", std_pred_vars_emos[1])] <- wg_emos_train$par[1:2]
  df_emos_fi[k+i+1, c("(Intercept)", std_pred_vars_emos[2])] <- wg_emos_train$par[3:4]
  
  
  
  #### Predicting with fitted model and evaluation on test data ####
  wg_emos_pred <- emos_pred(X = df_station[i_test,],
                            loc_pred = "VMAX_10M_mean",
                            scale_pred = "VMAX_10M_sd",
                            target = "wind_speed_of_gust",
                            distr = "tlogis",
                            par_emos = wg_emos_train$par)
  
  
  
  
  ### Collect scores for basic EMOS ###
  # Scores directly obtained from output of EMOS function, saved in data frame,
  # previous version of data frame is row-binded with scores for current station
  crps_emos <- rbind(crps_emos, data.frame(location=loc_vec[k], date=test_dates, crps=wg_emos_pred$scores$crps))
  logS_emos <- rbind(logS_emos, data.frame(location=loc_vec[k], date=test_dates, logS=wg_emos_pred$scores$logs))
  mae_emos <- rbind(mae_emos, data.frame(location=loc_vec[k], date=test_dates, mae=abs(wg_emos_pred$scores$e_md)))
  mse_emos <- rbind(mse_emos, data.frame(location=loc_vec[k], date=test_dates, mse=(wg_emos_pred$scores$e_me)^2))
  pit_emos <- rbind(pit_emos, data.frame(location=loc_vec[k], date=test_dates, pit=wg_emos_pred$scores$pit))
  lpredint_emos <- rbind(lpredint_emos, data.frame(location=loc_vec[k], date=test_dates, lgt=wg_emos_pred$scores$lgt))
  coverage1 <- (alpha/2 <= wg_emos_pred$scores$pit) & (wg_emos_pred$scores$pit <= (1 - alpha/2))
  coverage_emos <-rbind(coverage_emos, data.frame(location=loc_vec[k], date=test_dates, coverage=coverage1)) 
  
  
  
  
  ##### EMOS-GB #######
  
  
  ### Training/Fitting the model ###
  # Train boosted EMOS model on training data using all available standard variables.
  # Wind gust observations are the target variable, allow mean and sd of all standard variables
  # as potential predictors for linking location as well as scale to ensemble forecasts.
  # The vector std_pred_vars contains mean and sd of only the standard variables. 
  # As wind gusts are non-negative, truncated logistic distribution is chosen. 
  wg_emos_bst_train <- emos_bst_est(train = df_station[i_train,],
                                    pred_vars = std_pred_vars,
                                    target = "wind_speed_of_gust",
                                    bst_ls = list(nu = 0.05, 
                                    maxit = 1000,
                                    mstop = "aic"),
                                    opt.type = "ml",
                                    dist = "logistic",
                                    trunc=TRUE)
  
  
  ### Collect feature importance information ###
  # Write coefficient values plus further info in data frame
  df_emos_bst_fi[c(k+i, k+i+1), "location"] <- loc_vec[k]
  df_emos_bst_fi[c(k+i, k+i+1), "par"] <- c("loc", "scale")
  # Get coefficients
  df_emos_bst_fi[k+i, c("(Intercept)", std_pred_vars)] <- 
    wg_emos_bst_train$emos_bst_train$coefficients$location[c("(Intercept)", std_pred_vars)]
  df_emos_bst_fi[k+i+1, c("(Intercept)", std_pred_vars)] <- 
    wg_emos_bst_train$emos_bst_train$coefficients$scale[c("(Intercept)", std_pred_vars)]
  
  
  
  #### Predicting with fitted model and evaluation on test data ####
  wg_emos_bst_pred <- emos_bst_pred(X = df_station[i_test,],
                                    emos_bst_train=wg_emos_bst_train$emos_bst_train,
                                    pred_vars= std_pred_vars,
                                    target = "wind_speed_of_gust",
                                    distr = "tlogis")
  
  
  
  ### Collect scores for EMOS-GB ###
  # Scores directly obtained from output of EMOS-GB function, saved in data frame,
  # previous version of data frame is row-binded with scores for current station
  crps_emos_bst <- rbind(crps_emos_bst, data.frame(location=loc_vec[k], date=test_dates, crps=wg_emos_bst_pred$scores$crps))
  logS_emos_bst <- rbind(logS_emos_bst, data.frame(location=loc_vec[k], date=test_dates, logS=wg_emos_bst_pred$scores$logs))
  mae_emos_bst <- rbind(mae_emos_bst, data.frame(location=loc_vec[k], date=test_dates, mae=abs(wg_emos_bst_pred$scores$e_md)))
  mse_emos_bst <- rbind(mse_emos_bst, data.frame(location=loc_vec[k], date=test_dates, mse=(wg_emos_bst_pred$scores$e_me)^2))
  pit_emos_bst <- rbind(pit_emos_bst, data.frame(location=loc_vec[k], date=test_dates, pit=wg_emos_bst_pred$scores$pit))
  lpredint_emos_bst <- rbind(lpredint_emos_bst, data.frame(location=loc_vec[k], date=test_dates, lgt=wg_emos_bst_pred$scores$lgt))
  coverage2 <- (alpha/2 <= wg_emos_bst_pred$scores$pit) & (wg_emos_bst_pred$scores$pit <= (1 - alpha/2))
  coverage_emos_bst <-rbind(coverage_emos_bst, data.frame(location=loc_vec[k], date=test_dates, coverage=coverage2)) 
  
  
  
  
  ##### EMOS-GB-SP #######
  
  
  ### Training/Fitting the model ###
  # Train boosted EMOS model on training data using all available standard AND spatial variables.
  # Wind gust observations are the target variable, allow mean and sd of all standard AND
  # spatial variables as potential predictors for linking location as well as scale to ensemble forecasts.
  # The vector pred_vars contains mean and sd of both, standard and spatial variables. 
  # As wind gusts are non-negative, truncated logistic distribution is chosen. 
  wg_emos_bst_all_train <- emos_bst_est(train = df_station[i_train,],
                                        pred_vars = pred_vars,
                                        target = "wind_speed_of_gust",
                                        bst_ls = list(nu = 0.05, 
                                                      maxit = 1000,
                                                      mstop = "aic"),
                                        opt.type = "ml",
                                        dist = "logistic",
                                        trunc=TRUE)
  
  
  
  ### Collect feature importance information ###
  # Write coefficient values plus further info in data frame
  df_emos_bst_allvars_fi[c(k+i, k+i+1), "location"] <- loc_vec[k]
  df_emos_bst_allvars_fi[c(k+i, k+i+1), "par"] <- c("loc", "scale")
  # Get coefficients
  df_emos_bst_allvars_fi[k+i, c("(Intercept)", pred_vars)] <- 
    wg_emos_bst_all_train$emos_bst_train$coefficients$location[c("(Intercept)", pred_vars)]
  df_emos_bst_allvars_fi[k+i+1, c("(Intercept)", pred_vars)] <- 
    wg_emos_bst_all_train$emos_bst_train$coefficients$scale[c("(Intercept)", pred_vars)]
  
  
  
  #### Predicting with fitted model and evaluation on test data ####
  wg_emos_bst_all_pred <- emos_bst_pred(X = df_station[i_test,],
                                        emos_bst_train=wg_emos_bst_all_train$emos_bst_train,
                                        pred_vars= pred_vars,
                                        target = "wind_speed_of_gust",
                                        distr = "tlogis")
  
  
  
  ### Collect scores for EMOS-GB ###
  # Scores directly obtained from output of EMOS-GB function, saved in data frame,
  # previous version of data frame is row-binded with scores for current station
  crps_emos_bst_allvars <- rbind(crps_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, crps=wg_emos_bst_all_pred$scores$crps))
  logS_emos_bst_allvars <- rbind(logS_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, logS=wg_emos_bst_all_pred$scores$logs))
  mae_emos_bst_allvars <- rbind(mae_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, mae=abs(wg_emos_bst_all_pred$scores$e_md)))
  mse_emos_bst_allvars <- rbind(mse_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, mse=(wg_emos_bst_all_pred$scores$e_me)^2))
  pit_emos_bst_allvars <- rbind(pit_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, pit=wg_emos_bst_all_pred$scores$pit))
  lpredint_emos_bst_allvars <- rbind(lpredint_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, lgt=wg_emos_bst_all_pred$scores$lgt))
  coverage3 <- (alpha/2 <= wg_emos_bst_all_pred$scores$pit) & (wg_emos_bst_all_pred$scores$pit <= (1 - alpha/2))
  coverage_emos_bst_allvars <-rbind(coverage_emos_bst_allvars, data.frame(location=loc_vec[k], date=test_dates, coverage=coverage3)) 
 
  
  # Increase counter for moving through the data frame to save coefficient values
  i <- i+1
   
  
}


# Save all score data frames for later analysis
save(crps_ens, logS_ens, mae_ens, mse_ens,
     rank_ens, lpredint_ens, coverage_ens, 
     crps_emos, logS_emos, mae_emos, mse_emos,
     pit_emos, lpredint_emos, coverage_emos, 
     crps_emos_bst, logS_emos_bst, mae_emos_bst, mse_emos_bst,
     pit_emos_bst, lpredint_emos_bst, coverage_emos_bst,
     crps_emos_bst_allvars, logS_emos_bst_allvars, mae_emos_bst_allvars, mse_emos_bst_allvars,
     pit_emos_bst_allvars, lpredint_emos_bst_allvars, coverage_emos_bst_allvars, 
     file="C:/CIENS Data/R Code/Scores_00utc_12h.RData")


# Save all data frames with coefficient values of EMOS, EMOS-GB, EMOS-GB-SP for later analysis
# of feature importance
save(df_emos_fi, df_emos_bst_fi, df_emos_bst_allvars_fi, 
     file="C:/CIENS Data/R Code/Importances_00utc_12h.RData")
