## File containing functions for postprocessing ensemble forecasts via EMOS-GB

# Distribution: (zero-)truncated logistic distribution
# Estimation: MLE

#### Evaluation of distributional forecasts ####
# Function for prediction based on the distributional parameters #
fn_scores_distr <- function(f, y, distr = "tlogis", n_ens = 20){
  ###-----------------------------------------------------------------------------
  ###Input
  #f.......Parameters of forecast distribution (n x n_par matrix)
  #y.......Observations (n vector)
  #distr...Distributional forecast distribution ("tlogis" or "norm")
  #........Default: "tlogis" -> truncated logistic distribution
  #........Alternative: "norm" -> normal distribution
  #n_ens...Ensemble size (integer)
  #........Used for the calculation of the prediction interval length
  #........Default: 20 -> Ensemble size of CIENS data
  ###-----------------------------------------------------------------------------
  ###Output
  #...scores....Data frames containing (n x 6 data frame):
  #......pit....PIT values of distributional forecasts (n vector)
  #......crps...CRPS of forecasts (n vector)
  #......logs...Log-Score of forecasts (n vector)
  #......lgt....Length of prediction interval (n vector)
  #......e_md...Bias of median forecast (n vector)
  #......e_me...Bias of mean forecast (n vector)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Load packages
  library(scoringRules)
  library(crch)
  
  # Input check
  if(is.element(distr, c("tlogis", "norm")) & any(f[,2] < 0)){ print("Non-positive scale parameter included!") }
  
  #### Data preparation ####
  # Number of predictions
  n <- nrow(f)
  
  # Make data frame
  scores <- data.frame(pit = numeric(length = n),
                       crps = numeric(length = n),
                       logs = numeric(length = n),
                       lgt = numeric(length = n),
                       e_me = numeric(length = n),
                       e_md = numeric(length = n))
  
  #### Prediction and score calculation ####
  # Forecasts depending on distribution
  if(distr == "tlogis"){ # truncated logistic
    # Calculate PIT values
    scores[["pit"]] <- crch::ptlogis(q = y, 
                                     location = f[,1], 
                                     scale = f[,2],
                                     left = 0)
    
    # Calculate CRPS of forecasts
    scores[["crps"]] <- crps_tlogis(y = y, 
                                    location = f[,1], 
                                    scale = f[,2],
                                    lower = 0)
    
    # Calculate Log-Score of forecasts
    scores[["logs"]] <- logs_tlogis(y = y, 
                                    location = f[,1], 
                                    scale = f[,2],
                                    lower = 0)
    
    # Calculate length of ~(n_ens-1)/(n_ens+1) % prediction interval
    scores[["lgt"]] <- crch::qtlogis(p = n_ens/(n_ens + 1), 
                                     location = f[,1], 
                                     scale = f[,2],
                                     left = 0) - crch::qtlogis(p = 1/(n_ens + 1), 
                                                               location = f[,1], 
                                                               scale = f[,2],
                                                               left = 0)
    
    # Calculate bias of median forecast
    scores[["e_md"]] <- crch::qtlogis(p = 0.5, 
                                      location = f[,1], 
                                      scale = f[,2],
                                      left = 0) - y
    
    # Calculate bias of mean forecast
    scores[["e_me"]] <- (f[,1] - f[,2]*log(1 - plogis(- f[,1]/f[,2])))/(1 - plogis(- f[,1]/f[,2])) - y
  }
  else if(distr == "norm"){ # normal
    # Calculate PIT values
    scores[["pit"]] <- pnorm(q = y, 
                             mean = f[,1], 
                             sd = f[,2])
    
    # Calculate CRPS of forecasts
    scores[["crps"]] <- crps_norm(y = y, 
                                  location = f[,1], 
                                  scale = f[,2])
    
    # Calculate Log-Score of forecasts
    scores[["logs"]] <- logs_norm(y = y, 
                                  location = f[,1], 
                                  scale = f[,2])
    
    # Calculate length of ~(n_ens-1)/(n_ens+1) % prediction interval
    scores[["lgt"]] <- qnorm(p = n_ens/(n_ens + 1), 
                             mean = f[,1], 
                             sd = f[,2]) - qnorm(p = 1/(n_ens + 1), 
                                                 mean = f[,1], 
                                                 sd = f[,2])
    
    # Calculate bias of median forecast
    scores[["e_md"]] <- qnorm(p = 0.5, 
                              mean = f[,1], 
                              sd = f[,2]) - y
    
    # Calculate bias of mean forecast
    scores[["e_me"]] <- f[,1] - y
  }
  
  #### Output ####
  # Return
  return(scores)
}

#### Update hyperparameters ####
update_hpar <- function(hpar_ls, in_ls){
  ###-----------------------------------------------------------------------------
  ###Input
  #hpar_ls...Default hyperparameter (list)
  #in_ls.....Selected hyperparameter given by user (list)
  ###-----------------------------------------------------------------------------
  ###Output
  #hpar_ls...All hyperparameters including users selection (list)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Names of hyperparameters
  hpar_names <- names(hpar_ls)
  
  # Names of hyperparameter to update
  in_names <- names(in_ls)
  
  #### Update ####
  # Loop over given names
  for(temp_hpar in in_names){
    # Update list if correct name is given
    if(is.element(temp_hpar, hpar_names)){ hpar_ls[[temp_hpar]] <- in_ls[[temp_hpar]] }
    else{ print(paste0("Wrong hyperparameter given: ", temp_hpar))}
  }
  
  #### Output ####
  # Return list
  return(hpar_ls)
}

#### Remove constant columns ####
# Function that removes constant columns of data-frame
rm_const <- function(data, cols = NULL, t_c = 0){
  ###-----------------------------------------------------------------------------
  ###Input
  #data...Data to check (data frame)
  #cols...Columns to check (String or integer vector)
  #.......Default: NULL -> Check all
  #t_c....Threshold for (almost) constant column (non-negative scalar)
  #.......Default: 0 -> Constant
  ###-----------------------------------------------------------------------------
  ###Output
  #res...Columns of data that are not constant (String or integer vector)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Set cols if not given
  if(is.null(cols)){ cols <- colnames(data) }
  
  # Use only data that is needed
  data <- data[,cols]
  
  #### Remove columns ####
  # Number of samples to check with
  n_check <- min(10, nrow(data))
  
  # Check on sample which rows are candidates (-> computational more feasible)
  bool_res <- (apply(data[sample(1:nrow(data), n_check),], 2, sd) <= t_c)
  
  # Check if any of candidates is constant (Special case: Apply only on matrices)
  if(sum(bool_res) == 1){ bool_res[bool_res] <- (sd(data[,bool_res]) <= t_c) }
  else if(any(bool_res)){ bool_res[bool_res] <- (apply(data[,bool_res], 2, sd) <= t_c) }
  
  #### Output ####
  # Return columns that are not (almost) constant
  return(cols[!bool_res])
}


#### Prediction ####
# Function for prediction based on the EMOS parameters obtained via boosting #
emos_bst_pred <- function(X, emos_bst_train, pred_vars, target, distr = "tlogis"){
  ###-----------------------------------------------------------------------------
  ###Input
  #X................Ensemble data for prediction including predictors (and obs.) (n x n_preds (+ 1) data.frame)
  #emos_bst_train...Output of crch function used for prediction
  #pred_vars........Predictors used for EMOS-bst (vector of strings)
  #target.......Target variable (string)
  #distr........EMOS forecast distribution ("tlogis" or "norm")
  #.............Default: "tlogis" -> truncated logistic distribution
  #.............Alternative: "norm" -> normal distribution (has to be implemented, not yet done)
  ###-----------------------------------------------------------------------------
  ###Output
  #res...List containing:
  #......f...............EMOS forecasts (i.e. location and scale) based on emos_bst_train (n x 2 matrix)
  #......runtime.........Prediction time (numeric)
  #......n_test..........Number of test samples (integer)
  #.........scores...Data frames containing (n x 6 data frame):
  #.........pit.....PIT values of EMOS-bst (n vector)
  #.........crps.........CRPS of EMOS-bst forecasts (n vector)
  #.........logs.........Log-Score of EMOS-bst forecasts (n vector)
  #.........lgt..........Length of EMOS-bst prediction interval (n vector)
  #.........e_md.........Bias of median forecast (n vector)
  #.........e_me.........Bias of mean forecast (n vector)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Load packages
  library(scoringRules)
  library(crch)
  
  # Relevant variables for prediction
  test_vars <- c(pred_vars, target) 
  
  # Input check
  if(any(!is.element(test_vars, names(X)))){ 
    print("EMOS-bst-pred: Data does not include all of the relevant variables.") }
  
  # Cut data to relevant variables
   X <- X[,test_vars] 
  
  # Input check
  if(any(is.na(X))){
    print("EMOS-bst-pred: Data includes missing values! Missing values are left out!")
    X <- na.omit(X)
  }

  
  #### Data preparation ####
  # Number of predictions
  n <- nrow(X)
  
  #### Prediction ####
  # Take time
  start_tm <- Sys.time()
  
  # Calculate forecasts
  f <- predict(object = emos_bst_train,
               newdata = X,
               type = "parameter")
  
  # Take time
  end_tm <- Sys.time()
  
  # Time needed
  runtime <- as.numeric(difftime(end_tm, start_tm, units = "mins"))
  
  
  #### Evaluation ####
  
  # Calculate evaluation measure of EMOS forecasts
  scores <- fn_scores_distr(f = f,
                            y = X[[target]],
                            distr = "tlogis")
  
  #### Output ####
  return(list(f = f, 
              n_test = nrow(X),
              runtim_sec = runtime,
              scores = scores))
}



#### Estimation ####
# Function for estimating EMOS parameters via boosting #
emos_bst_est <- function(train, pred_vars = c("ens_mean", "ens_sd"), 
                         bst_ls = list(), target, dist = "logistic", 
                         opt.type="ml", trunc=TRUE){
  ###-----------------------------------------------------------------------------
  ###Input
  #train...........Training data including predictors and obs. (n_train x (n_preds + 1) data.frame)
  #pred_vars.......Predictors used for EMOS boosting (vector of strings)
  #................Default: c("ens_mean", "ens_sd") -> Use only mean and variance
  #bst_ls..........List that may contain the following variables:
  #...nu...........Step size (positive scalar)
  #................Default: 0.05
  #...maxit........Maximum number of iterations (integer)
  #................Default: 1,000
  #...mstop........Stopping criterion ("max", "aic", "bic", "cv")
  #................Default: AIC (Aikake Information criterion)
  ###-----------------------------------------------------------------------------
  ###Output
  #res...List containing:
  #......emos_bst_train...EMOS-bst estimation (crch-object)
  #......bst_ls...........Hyperparameters (list)
  #......pred_vars........Predictors (string vector)
  #......n_preds..........Number of predictors used (integer)
  #......n_train..........Number of training samples (integer)
  #......runtime..........Estimation time (numeric)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Load packages
  library(crch)
  
  # Relevant variables for training
  train_vars <- c(target, pred_vars)
  
  # Input check
  if(any(!is.element(train_vars, names(train)))){ 
    print("EMOS-bst-est: Training data does not include relevant variables.") }
  
  # Cut data to relevant variables
  train <- train[,train_vars]
  
  # Input check
  if(any(is.na(train))){
    print("EMOS-bst-est: Training data includes missing values! Missing values are left out!")
    train <- na.omit(train)
  }
  
  # Threshold for (almost) constant predictors
  t_c <- 1e-4
  
  #### Hyperparameter ####
  # Hyperparameters and their default values
  hpar_ls <- list(nu = 0.05,
                  maxit = 1000,
                  mstop = "aic")
  
  # Update hyperparameters
  bst_ls <- update_hpar(hpar_ls = hpar_ls,
                        in_ls = bst_ls)
  
  #### Data preparation ####
  # Remove constant predictors
  pred_vars <- rm_const(data = train,
                        cols = pred_vars,
                        t_c = t_c)
  
  #### Estimation ####
  # Use all predictors for both location and scale
  temp_fml <- paste0(target, " ~ ", paste0(pred_vars, collapse = " + "), " | ", 
                     paste0(pred_vars, collapse = " + "))
  
  # Take time
  start_tm <- Sys.time()
  
  # Boosting
  est <- crch(formula = temp_fml,
              data = train,
              link.scale = "log",
              dist = dist,
              truncated = trunc,
              left = 0,
              type = opt.type,
              control = crch.boost(maxit = bst_ls$maxit,
                                   nu = bst_ls$nu,
                                   mstop = bst_ls$mstop))
  
  # Take time
  end_tm <- Sys.time()
  
  # Time needed
  runtime <- as.numeric(difftime(end_tm, start_tm, units = "mins"))
  
  #### Output ####
  return(list(emos_bst_train = est,
              bst_ls = bst_ls,
              pred_vars = pred_vars,
              n_preds = length(pred_vars),
              n_train = nrow(train),
              runtime = runtime))
}

