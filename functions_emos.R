## File containing functions for postprocessing via EMOS

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

#### EMOS ####
## Distribution: (zero-)truncated logistic (for wind speed) or normal distribution (for temperature)
## Estimation: CRPS
## Based on each one predictor variable for location and scale parameter

# Function for prediction based on the EMOS parameters #
emos_pred <- function(X, loc_pred, scale_pred, target, distr = "tlogis", par_emos = NULL){
  ###-----------------------------------------------------------------------------
  ###Input
  #X............Ensemble data for prediction including predictors (and obs.) (n x n_preds (+ 1) data.frame)
  #loc_pred.....Predictor variable for location parameter (string)
  #scale_pred...Predictor variable for scale parameter (string)
  #target.......Target variable (string)
  #distr........EMOS forecast distribution ("tlogis" or "norm")
  #.............Default: "tlogis" -> truncated logistic distribution
  #.............Alternative: "norm" -> normal distribution
  #par_emos.....EMOS parameters a, b, c and d (4 vector)
  #.............Default: NULL -> a = c = 0, b = d = 1, i.e. ensemble mean and variance as location and scale
  ###-----------------------------------------------------------------------------
  ###Output
  #res...List containing:
  #......f.........EMOS forecasts (location and scale) based on par_emos (n x 2 matrix)
  #......runtime...Prediction time (numeric)
  #......n_test....Number of test samples (integer)
  #......scores....Data frames containing (n x 6 data frame):
  #.........pit....PIT values of distributional forecasts (n vector)
  #.........crps...CRPS of forecasts (n vector)
  #.........logs...Log-Score of forecasts (n vector)
  #.........lgt....Length of prediction interval (n vector)
  #.........e_md...Bias of median forecast (n vector)
  #.........e_me...Bias of mean forecast (n vector)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Load packages
  library(scoringRules)
  
  # Relevant variables for prediction
  test_vars <- unique(c(loc_pred, scale_pred, target))
  
  # Input check
  if(any(!is.element(test_vars, names(X)))){ 
    print("EMOS-pred: Data does not include all of the relevant variables.") }
  
  # Cut data to relevant variables
  X <- X[,test_vars,drop = FALSE]
  
  # Input check
  if(any(is.na(X))){
    print("EMOS-pred: Data includes missing values! Missing values are left out!")
    X <- na.omit(X)
  }
  
  # Initiate parameter vector
  if(is.null(par_emos)){ par_emos <- c(0, 0, 0, 1) }
  
  #### Data preparation ####
  # Number of predictions
  n <- nrow(X)
  
  # Read out EMOS parameters
  a <- par_emos[1]
  b <- par_emos[2]
  c <- par_emos[3]
  d <- par_emos[4]
  
  #### Prediction ####
  # Take time
  start_tm <- Sys.time()
  
  # Initiate forecast matrix
  f <- matrix(nrow = n, 
              ncol = 2)
  colnames(f) <- c("location", "scale")
  
  # Calculate location parameter
  f[,1] <- a + exp(b)*X[[loc_pred]]
  
  # Calculate scale parameter
  f[,2] <- exp(c + d*log(X[[scale_pred]]))
  
  # Take time
  end_tm <- Sys.time()
  
  # Time needed
  runtime <- as.numeric(difftime(end_tm, start_tm, units = "secs"))
  
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

# Function for estimating the EMOS parameters #
emos_est <- function(train, loc_pred, scale_pred, target, distr = "tlogis", par_start = NULL){
  ###-----------------------------------------------------------------------------
  ###Input
  #train........Training data including predictors and obs. (n_train x (n_preds + 1) data.frame)
  #loc_pred.....Predictor variable for location parameter (string)
  #scale_pred...Predictor variable for scale parameter (string)
  #target.......Target variable (string)
  #distr........EMOS forecast distribution ("tlogis" or "norm")
  #.............Default: "tlogis" -> truncated logistic distribution
  #.............Alternative: "norm" -> normal distribution
  #par_start....Initial values of optimization (4 vector)
  #.............Default: NULL -> a = c = d = 0, b = c = 1, i.e. ensemble mean as location, constant scale
  ###-----------------------------------------------------------------------------
  ###Output
  #res...List containing:
  #......par.......Estimated EMOS parameters a, b, c, d (4 vector)
  #......n_train...Number of training samples (integer)
  #......runtime...Estimation time (numeric)
  ###-----------------------------------------------------------------------------
  
  #### Initiation ####
  # Load packages
  library(scoringRules)
  
  # Relevant variables for training
  train_vars <- unique(c(loc_pred, scale_pred, target))
  
  # Input check
  if(any(!is.element(train_vars, names(train)))){ 
    print("EMOS-est: Training data does not include relevant variables.") }
  
  # Cut data to relevant variables
  train <- train[,train_vars]
  
  # Input check
  if(any(is.na(train))){
    print("EMOS-est: Training data includes missing values! Missing values are left out!")
    train <- na.omit(train)
  }
  
  # Threshold for Nelder-Mead improvement
  t_nelder <- 1e-3
  
  # Minimum resp. maximum threshold for location and scale
  t_max <- 1e+3
  t_min <- 1e-3
  
  # Threshold for (almost) zero observations
  t_0 <- 1e-2
  
  # Location function
  fn_loc <- function(x, a, b){ pmax(-t_max, pmin(t_max, a + exp(b)*x)) }
  
  # Scale function
  fn_scale <- function(x, c, d){ pmax(t_min, pmin(t_max, exp(c + d*log(x)))) }
  
  # Define CRPS based on distribution
  if(distr == "tlogis"){ 
    fn_sr <- function(y, location, scale){ crps_tlogis(y = y, 
                                                       location = location, 
                                                       scale = scale,
                                                       lower = 0) } }
  else if(distr == "norm"){ 
    fn_sr <- function(y, location, scale){ crps_norm(y = y, 
                                                     location = location, 
                                                     scale = scale) } }
  
  # Define gradient for CRPS estimation
  if(distr == "tlogis"){ 
    fn_grad <- function(y, location, scale){ gradcrps_tlogis(y = y, 
                                                           location = location, 
                                                           scale = scale,
                                                           lower = 0) } }
  else if(distr == "norm"){ 
    fn_grad <- function(y, location, scale){ gradcrps_norm(y = y, 
                                                           location = location, 
                                                           scale = scale) } }
  
  #### Data preparation ####
  # Set (almost) zero-observations to t_0
  if(distr == "tlogis"){ obs_cut <- pmax(t_0, train[[target]]) }
  else{ obs_cut <- train[[target]] }
  
  #### Estimation ####
  # Set initial values (a = c = 0, b/exp(b) = d = 1, i.e. ensemble mean and standard deviation as parameters)
  if(is.null(par_start)){ par_start <- c(0, 0, 0, 1) }
  
  # Define wrapper function
  wrapper <- function(par_emos){
    ###-----------------------------------------------------------------------------
    ###Input
    #par_emos...EMOS parameters a, b, c, d (4 vector)
    ###-----------------------------------------------------------------------------
    ###Output
    #res...Mean score of EMOS forecasts (of a, b, c, d) on training set (scalar)
    ###-----------------------------------------------------------------------------
    
    #### Calculation ####
    # Calculate location and scale parameters
    loc_emos <- fn_loc(x = train[[loc_pred]], 
                       a = par_emos[1], 
                       b = par_emos[2])
    scale_emos <- fn_scale(x = train[[scale_pred]], 
                           c = par_emos[3], 
                           d = par_emos[4])
    
    # Calculate mean scores of training data
    res <- mean(fn_sr(y = obs_cut,
                      location = loc_emos,
                      scale = scale_emos))
    
    # Output
    return(res)
  }
  
  # Define gradient (w.r.t. a, b, c, d)
  grad <- function(par_emos){
    ###-----------------------------------------------------------------------------
    ###Input
    #par_emos...EMOS parameters a, b, c and d (4 vector)
    ###-----------------------------------------------------------------------------
    ###Output
    #res...Gradient of mean score w.r.t. EMOS parameters on training set (4 vector)
    ###-----------------------------------------------------------------------------
    
    #### Initialization ####
    # Initialize resulting gradient
    res <- vector(length = 4)
    
    #### Calculation ####
    # For each ensemble: Calculate location and scale parameters
    loc_emos <- fn_loc(x = train[[loc_pred]], 
                       a = par_emos[1], 
                       b = par_emos[2])
    scale_emos <- fn_scale(x = train[[scale_pred]], 
                           c = par_emos[3], 
                           d = par_emos[4])
    
    # Calculate gradient of crps
    s_grad <- fn_grad(y = obs_cut, 
                      location = loc_emos,
                      scale = scale_emos)
    
    # Derivatives w.r.t. a and b
    res[1] <- mean(s_grad[,"dloc"])
    res[2] <- mean(s_grad[,"dloc"]*exp(par_emos[2])*train[[loc_pred]])
    
    # Derivatives w.r.t. c and d
    res[3] <- mean(s_grad[,"dscale"]*scale_emos)
    res[4] <- mean(s_grad[,"dscale"]*scale_emos*log(train[[scale_pred]])) 
    
    # Output
    return(res)
  }
  
  # Take time
  start_tm <- Sys.time()
  
  # Try optimizing
  try_est <- try(expr = {
    # Optimize ("L-BFGS-B" outperforms "BFGS" (drastically) and "Nelder-Mead")
    est <- optim(par = par_start, 
                 fn = wrapper, 
                 gr = grad,
                 method = "L-BFGS-B")
    
    # Check convergence
    if(est$convergence != 0){
      # Optimize
      temp <- optim(par = par_start,
                    fn = wrapper,
                    method = "Nelder-Mead")
      
      # Check if better fit
      if(temp$value < est$value - t_nelder){ est <- temp }
    }
  },
  silent = TRUE)
  
  # If optimizing did not work, use Nelder-Mead
  if(class(try_est) == "try-error"){
    # Optimize
    est <- optim(par = par_start, 
                 fn = wrapper, 
                 gr = grad,
                 method = "Nelder-Mead")
  }
  
  # Take time
  end_tm <- Sys.time()
  
  # Time needed
  runtime <- as.numeric(difftime(end_tm, start_tm, units = "secs"))
  
  #### Output ####
  # Output
  return(list(par = est$par,
              n_train = nrow(train),
              runtime_sec = runtime))
}


