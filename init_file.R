## CIENS: Initiation for exemplary R-code

# Path to of CIENS data
data_path <- "F:/W2W_C5/publication_data/"

# Load functions
source(file = paste0(getwd(), "/functions.R"))

# Load additional data
load(file = paste0(getwd(), "/ciens_info.RData"))

# Load packages
library(lubridate)
library(dplyr)
library(ncdf4)
library(scoringRules)

# Get ensemble size
n_ens <- length(ens_vec)