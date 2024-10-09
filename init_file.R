## CIENS: Initiation for exemplary R-code

# Path to of CIENS data
# Recommendation: Download all 4 folders (run0, run0_spatial, run12, run12_spatial)
# and save them in one joint folder, otherwise not all of the example code will work properly.
data_path <- "C:/CIENS Data/"

# Load helper functions used in the 3 example code files
source(file = paste0(getwd(), "/functions.R"))

# Load additional information for creating a data.frame from the netcdf files
# The .RData file contains names of stations, lon and lat of stations,
# vector with index for ensemble members (1,...,20), vector with forecast horizons (0,1,...,21),
# vector with initialization times, vector with names of variables for which ensemble forecasts
# are available, vector with names of variables for which observations are available, and
# vector with spatial variables for which forecasts are available. 
load(file = paste0(getwd(), "/ciens_info.RData"))

# Load required packages
library(lubridate)
library(dplyr)
library(ncdf4)
library(scoringRules)

# Get ensemble size from the respective object workspace saved in the ciens_info.RData file
n_ens <- length(ens_vec)
