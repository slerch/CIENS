## CIENS: Generation of exemplary data frame for postprocessing of Wind Gust
## 00 UTC forecasts and 12h ahead lead time
## Analysis and plots of results, scores, variable importance

#### Load packages for creating analysis and figures ####

library(RColorBrewer)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(gridExtra)
library(maps)
library(colorspace)
library(metR)
library(reshape2)
library(viridis)
library(dplyr)



#### Initialization ####

# Path to Github repository example functions
git_path <- "C:/CIENS Data/R Code/"

# Set working directory
setwd(git_path)

# Load R code for initializing
source(file = paste0(getwd(), "/init_file.R"))


# Station info from init file for creating maps later and aggregate scores
dat <- loc_data
ids <- unique(dat$station_id)
n.stations <- length(ids)
stat_info <- unique(dat[, c("station_id", "name", "longitude", "latitude", "height")])
stat_info <- as.data.frame(stat_info)


# You need to create a google API key and register it here to access google maps
register_google(key = "api-key")




##### Load scores and coefficient values from wind gust postprocessing for further analysis #####

load(file="C:/CIENS Data/R Code/Scores_00utc_12h.RData")
load(file="C:/CIENS Data/R Code/Importances_00utc_12h.RData")


############## Compute station-wise mean of scores ########################

#### Here for CRPS, MAE, MSE, length of prediction interval, coverage,
#### which are presented in the paper in tables and/or figures


# Function to compute mean of score over all test days for each location in original data frame.
# For this the data frame df needs to be of the structure as in the score 
# data frames obtained in this postprocessing example, 
# i.e. there is a column called "location" containing the station IDs as defined above in 
# the object "ids", and the third column of the data frame needs to contain the score of interest
station.mean.score <- function(df, x)
{
  dat <- subset(df, subset=(location==ids[x]))
  return(mean(dat[,3], na.rm=TRUE))
}




#### Station-wise mean of CRPS #### 


# Mean CRPS values as vector for all 170 stations, for later use in maps 
crps.ens <- sapply(1:n.stations, station.mean.score, df=crps_ens)
crps.emos <- sapply(1:n.stations, station.mean.score, df=crps_emos)
crps.emos.bst <- sapply(1:n.stations, station.mean.score, df=crps_emos_bst)
crps.emos.bst.allvars <- sapply(1:n.stations, station.mean.score, df=crps_emos_bst_allvars)


# Or as data frame with corresponding station IDs, exemplary for EMOS model 
crps_emos_stations <- data.frame(location=ids, crps=sapply(1:n.stations, station.mean.score, df=crps_emos))



#### Station-wise mean of MAE ####


# Mean absolute error values as vector for all 170 stations 
mae.ens <- sapply(1:n.stations, station.mean.score, df=mae_ens)
mae.emos <- sapply(1:n.stations, station.mean.score, df=mae_emos)
mae.emos.bst <- sapply(1:n.stations, station.mean.score, df=mae_emos_bst)
mae.emos.bst.allvars <- sapply(1:n.stations, station.mean.score, df=mae_emos_bst_allvars)


# Or as data frame with corresponding station IDs, exemplary for EMOS model 
mae_emos_stations <- data.frame(location=ids, crps=sapply(1:n.stations, station.mean.score, df=mae_emos))


# Station-wise mean of MSE

# Mean squared error values as vector for all 170 stations 
mse.ens <- sapply(1:n.stations, station.mean.score, df=mse_ens)
mse.emos <- sapply(1:n.stations, station.mean.score, df=mse_emos)
mse.emos.bst <- sapply(1:n.stations, station.mean.score, df=mse_emos_bst)
mse.emos.bst.allvars <- sapply(1:n.stations, station.mean.score, df=mse_emos_bst_allvars)


# Or as data frame with corresponding station IDs, exemplary for EMOS model 
mse_emos_stations <- data.frame(location=ids, crps=sapply(1:n.stations, station.mean.score, df=mse_emos))



# Station-wise mean of length of central prediction interval

# Mean interval lengths as vector for all 170 stations 
lpredint.ens <- sapply(1:n.stations, station.mean.score, df=lpredint_ens)
lpredint.emos <- sapply(1:n.stations, station.mean.score, df=lpredint_emos)
lpredint.emos.bst <- sapply(1:n.stations, station.mean.score, df=lpredint_emos_bst)
lpredint.emos.bst.allvars <- sapply(1:n.stations, station.mean.score, df=lpredint_emos_bst_allvars)



# Or as data frame with corresponding station IDs, exemplary for EMOS model 
lpredint_emos_stations <- data.frame(location=ids, 
                                     crps=sapply(1:n.stations, station.mean.score, df=lpredint_emos))



# Station-wise mean of coverage 

# Mean coverage values as vector for all 170 stations 
coverage.ens <- sapply(1:n.stations, station.mean.score, df=coverage_ens)
coverage.emos <- sapply(1:n.stations, station.mean.score, df=coverage_emos)
coverage.emos.bst <- sapply(1:n.stations, station.mean.score, df=coverage_emos_bst)
coverage.emos.bst.allvars <- sapply(1:n.stations, station.mean.score, df=coverage_emos_bst_allvars)



# Or as data frame with corresponding station IDs, exemplary for EMOS model 
coverage_emos_stations <- data.frame(location=ids, 
                                     crps=sapply(1:n.stations, station.mean.score, df=coverage_emos))


##### Comment on removing outlier #########

# When looking at the station-wise means of the scores, 
# a strong outlier is found for some of the models for station with index 7, ID "10044" (Lighthouse in Kiel)
# This station is removed for all further analyses, tables, plots in order not to bias the results
# towards the outlier. 


##### Overall mean of scores in Table 3 ######

# Raw ensemble 
mean(crps.ens[-7])
mean(mae.ens[-7])
mean(mse.ens[-7])
mean(lpredint.ens[-7])
mean(coverage.ens[-7])


# EMOS
mean(crps.emos[-7])
mean(mae.emos[-7])
mean(mse.emos[-7])
mean(lpredint.emos[-7])
mean(coverage.emos[-7])


# EMOS-GB
mean(crps.emos.bst[-7])
mean(mae.emos.bst[-7])
mean(mse.emos.bst[-7])
mean(lpredint.emos.bst[-7])
mean(coverage.emos.bst[-7])


# EMOS-GB-SP
mean(crps.emos.bst.allvars[-7])
mean(mae.emos.bst.allvars[-7])
mean(mse.emos.bst.allvars[-7])
mean(lpredint.emos.bst.allvars[-7])
mean(coverage.emos.bst.allvars[-7])





############### Score and Skill Score Maps ########################


###### 3 different Skill Score Maps in one Figure ######
#(1) CRPSS of EMOS vs raw ensemble
#(2) CPRSS of EMOS−GB vs EMOS
#(3) CPRSS of EMOS-GB_SP vs EMOS-GB


# (1) EMOS vs raw ensemble

# Compute skill score of EMOS vs raw ensemble 
crpss_emos <- 1-(crps.emos/crps.ens)

# Remove outlier station and multiply with 100 to have % values
values <- values1 <- crpss_emos[-7]*100

# Censoring of values at −15% and 30% for better visual representation in the maps, 
# values smaller (resp. larger) than -15% (-30%) are set to −15% (resp. 30%).
values1[values>30] <- 30
values1[values< -15] <- -15


# Create data frame with station info for map plotting
df <- data.frame(stat_info[-7,],
                 CRPSS = values1)


# Map
p1 <- ggmap(get_googlemap(
  center = c(longitude = 10.2651, latitude = 51.5), #Long/lat of centre
  zoom = 6,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off",
  maptype = "terrain")) +
  geom_point(data = df, shape=21, aes(x = longitude, y = latitude, fill = CRPSS), size = 4) +
  theme(legend.position = "right", legend.key = element_rect(fill = "white"), 
        axis.text.x = element_text(size=21), 
        axis.text.y=element_text(size=21), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), legend.title=element_text(size=24),
        legend.text=element_text(size=24)) +
  scale_fill_continuous_diverging(palette = "Red-Green", limits=c(-15,30))
p1$labels$x <- "Longitude"
p1$labels$y <- "Latitude"
p1$labels$colour <- "CRPSS (%)"
p1 <- p1 + guides(fill = guide_colorbar(title = "CRPSS (%)")) + theme(text = element_text(size = 24))
p1 <- p1 + ggtitle("EMOS vs Raw Ensemble")
p1


# (2) EMOS−GB vs EMOS

# Compute skill score of EMOS-GB vs EMOS
crpss_emos_bst <- 1-(crps.emos.bst/crps.emos)

# Remove outlier station and multiply with 100 to have % values
values <- values1 <- crpss_emos_bst[-7]*100


# Censoring of values at −15% and 30% for better visual representation in the maps, 
# values smaller (resp. larger) than -15% (-30%) are set to −15% (resp. 30%).
values1[values>30] <- 30
values1[values< -15] <- -15


# Create data frame with station info for map plotting
df <- data.frame(stat_info[-7,],
                 CRPSS = values1)


# Map
p2 <- ggmap(get_googlemap(
  center = c(longitude = 10.2651, latitude = 51.5), #Long/lat of centre
  zoom = 6,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off",
  maptype = "terrain")) +
  geom_point(data = df, shape=21, aes(x = longitude, y = latitude, fill = CRPSS), size = 4) +
  theme(legend.position = "right", legend.key = element_rect(fill = "white"), 
        axis.text.x = element_text(size=21), 
        axis.text.y=element_text(size=21), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), legend.title=element_text(size=24),
        legend.text=element_text(size=24)) +
  scale_fill_continuous_diverging(palette = "Red-Green", limits=c(-15,30))
p2$labels$x <- "Longitude"
p2$labels$y <- "Latitude"
p2$labels$colour <- "CRPSS (%)"
p2 <- p2 + guides(fill = guide_colorbar(title = "CRPSS (%)")) + theme(text = element_text(size = 24))
p2 <- p2 + ggtitle("EMOS-GB vs EMOS")
p2


# (3) EMOS-GB-SP vs EMOS−GB

# Compute skill score of EMOS-GB-SP vs EMOS-GB
crpss_emos_bst_allvars <- 1-(crps.emos.bst.allvars/crps.emos.bst)

# Remove outlier station and multiply with 100 to have % values
values <- values1 <- crpss_emos_bst_allvars[-7]*100


# Censoring of values at −15% and 30% for better visual representation in the maps, 
# values smaller (resp. larger) than -15% (-30%) are set to −15% (resp. 30%).
values1[values>30] <- 30
values1[values< -15] <- -15


# Create data frame with station info for map plotting
df <- data.frame(stat_info[-7,],
                 CRPSS = values1)

# Map
p3 <- ggmap(get_googlemap(
  center = c(longitude = 10.2651, latitude = 51.5), #Long/lat of centre
  zoom = 6,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off",
  maptype = "terrain")) +
  geom_point(data = df, shape=21, aes(x = longitude, y = latitude, fill = CRPSS), size = 4) +
  theme(legend.position = "right", legend.key = element_rect(fill = "white"), 
        axis.text.x = element_text(size=21), 
        axis.text.y=element_text(size=21), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), legend.title=element_text(size=24),
        legend.text=element_text(size=24)) + 
  scale_fill_continuous_diverging(palette = "Red-Green", limits=c(-15,30))
p3$labels$x <- "Longitude"
p3$labels$y <- "Latitude"
p3$labels$colour <- "CRPSS (%)"
p3 <- p3 + guides(fill = guide_colorbar(title = "CRPSS (%)")) + theme(text = element_text(size = 24))
p3 <- p3 + ggtitle("EMOS-GB-SP vs EMOS-GB")
p3


# Save resulting figure as PDF, combine the 3 figures with package patchwork
pdf(file="C:/CIENS Data/R Code/Maps1_windgust_00utc_12h.pdf", 
    width =20, height = 8, pointsize = 30)
p1+p2+p3+ plot_layout(ncol = 3, guides = "collect")  # one joint color bar and legend
dev.off()



############ 2 score maps in one Figure ############

# (1) Mean CRPS of raw ensemble
# (2) CRPSS of all methods vs raw ensemble


# (1) Mean CRPS of raw ensemble

# Create data frame with station info for map plotting
df <- data.frame(stat_info[-7,],
                 CRPS = crps.ens[-7])

# Map
p4 <- ggmap(get_googlemap(
  center = c(longitude = 10.2651, latitude = 51.5), #Long/lat of centre
  zoom = 6,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off",
  maptype = "terrain")) +
  geom_point(data = df, shape=21, aes(x = longitude, y = latitude, fill = CRPS), size = 4) +
  theme(legend.position = "right", legend.key = element_rect(fill = "white"), 
        axis.text.x = element_text(size=21), 
        axis.text.y=element_text(size=21), axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=23), legend.title=element_text(size=23),
        legend.text=element_text(size=23)) + 
  scale_fill_continuous_sequential(palette = "Blues 3", begin=0.2, end=1, p1=2, p2=2,
                                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "white", title.hjust = 0.5))
p4$labels$x <- "Longitude"
p4$labels$y <- "Latitude"
p4$labels$colour <- "CRPS"
p4 <- p4 + guides(fill = guide_colorbar(title = "CRPS")) + theme(text = element_text(size = 23))
p4 <- p4 + ggtitle("Raw Ensemble")
p4



# (2) CRPSS of all methods vs raw ensemble


# Compute CRPSS of EMOS, EMOS-GB, EMOS-GB-SP vs raw ensemble
crpss_emos <- 1-(crps.emos/crps.ens) # was already computed above, for completeness here again
crpss_emos_bst2 <- 1-(crps.emos.bst/crps.ens)
crpss_emos_bst_allvars2 <- 1-(crps.emos.bst.allvars/crps.ens)


# Column-bind the 3 vectors of CRPSS values
values1 <- cbind(crpss_emos, crpss_emos_bst2, crpss_emos_bst_allvars2)

# Remove outlier station and multiply with 100 to have % values
values1 <- values1[-c(7),]*100

# Obtain for each row (corresponding to one station) in which column 
# the CRPSS is maximal, i.e. the corresponding method is the best performing method.
# The result is coded as integer corresponding to the column of the method. 
method <- apply(values1, 1, which.max)

# Now the integer coding in terms of the column number is converted to the actual model name
method <- sapply(1:length(method), function(k) c("EMOS",
                                                 "EMOS-GB",
                                                 "EMOS-GB-SP")[method[k]])

# Table output across all stations in order to get the counts how 
# often, i.e. at how many stations, each method is actually performing best
table(method)


# Assign string with method name and respective count from above table how often it is performing best
# for plotting this information alongside with the map of CRPSS values
method[method == "EMOS"] <- "EMOS (7)"
method[method == "EMOS-GB"] <- "EMOS-GB (16)"
method[method == "EMOS-GB-SP"] <- "EMOS-GB-SP (146)"


# Obtain the actual maximum CRPSS values for each row, i.e. each station
values1 <- apply(values1, 1, max)


# Create data frame with station info and Method names with counts for map plotting
df <- data.frame(stat_info[-c(7),],
                 CRPSS = values1,
                 Method = method)


# Map
p5 <- ggmap(get_googlemap(
  center = c(longitude = 10.2651, latitude = 51.5), #Long/lat of centre
  zoom = 6,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off",
  maptype = "terrain")) +
  geom_point(data = df, aes(x = longitude, y = latitude, shape = Method, group = Method, fill = CRPSS), size = 4) +
  scale_shape_manual(values = c(21, 22, 23)) +
  theme(legend.position = "right", legend.key = element_rect(fill = "white"), 
        axis.text.x = element_text(size=21), 
        axis.text.y=element_text(size=21), axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=23), legend.title=element_text(size=23),
        legend.text=element_text(size=23)) + 
  scale_fill_continuous_diverging(palette = "Red-Green")
p5$labels$x <- "Longitude"
p5$labels$y <- "Latitude"
p5$labels$colour <- "CRPSS (%)"
p5 <- p5 + guides(fill = guide_colorbar(order = 1, title = "CRPSS (%)"),
                  shape = guide_legend(order = 2)) + theme(text = element_text(size = 23))
p5 <- p5 + ggtitle("All Methods vs Raw Ensemble")



# Save resulting figure as PDF, combine the 2 figures  with package patchwork
pdf(file="C:/CIENS Data/R Code/Maps2_windgust_00utc_12h.pdf", 
    width =20, height = 8, pointsize = 30)
p4+p5
dev.off()




############### Boxplots of station-wise Variable Importance ########################
############### Absolute coefficient values are used as importance measure ###########


#### For Location Parameter ####


### EMOS ###

# Define subset with rows corresponding to location parameter
df_emos_loc_fi <- subset(df_emos_fi, par == "loc")

# Compute absolute values for all columns corresponding to a coefficient in the model
# First 2 columns are station ID, parameter type (location or scale)
df_emos_loc_fi[,3:273] <- abs(df_emos_loc_fi[,3:273])
# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_loc_fi$Method <- "EMOS"


# Remove outlier station
df_emos_loc_fi <- subset(df_emos_loc_fi, location != "10044") 


# Change data frame to long format
df_long_emos <- melt(df_emos_loc_fi, id.vars=c("location", "par", "Method"))

# For location parameter in EMOS, only Intercept and mean of wind gusts are allowed,
# therefore, reduce data set to these 2 parameters only and create data frame
# with the variables that are supposed to be shown in the plot
vars.emos <- c("(Intercept)", "VMAX_10M_mean") 
df_emos_loc_fi.plot <- subset(df_long_emos, variable %in% vars.emos)
df_emos_loc_fi.plot$variable <- factor(df_emos_loc_fi.plot$variable, exclude=NA)



### EMOS-GB ###

# Define subset with rows corresponding to location parameter
df_emos_bst_loc_fi <- subset(df_emos_bst_fi, par == "loc")

# Compute absolute values for all columns corresponding to a coefficient in the model
# First 2 columns are station ID, parameter type (location or scale)
df_emos_bst_loc_fi[,3:273] <- abs(df_emos_bst_loc_fi[,3:273])
# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_bst_loc_fi$Method <- "EMOS-GB"

# Remove outlier station
df_emos_bst_loc_fi <- subset(df_emos_bst_loc_fi, location != "10044") 


# Obtain the 7 most important variables (without intercept) for boxplots, for that 
# extract only the variable columns (without intercept), compute mean of absolute 
# coef values across all stations, sort variables by mean and extract the 7 most important ones
df_emos_bst_test <- df_emos_bst_loc_fi[,4:273]
mean.vars <- sort(apply(df_emos_bst_test, 2, mean, na.rm=T), decreasing=TRUE)
plot.vars.emos.bst <- c("(Intercept)", names(mean.vars[1:7]))


# Change data frame to long format
df_long_emos_bst <- melt(df_emos_bst_loc_fi, id.vars=c("location", "par", "Method"))


# Create data frame with the plot vars (7 most important ones) that are supposed to be shown in the plot
df_emos_bst_loc_fi.plot <- subset(df_long_emos_bst, variable %in% plot.vars.emos.bst)
df_emos_bst_loc_fi.plot$variable <- factor(df_emos_bst_loc_fi.plot$variable, exclude=NA)





### EMOS-GB-SP ###

# Define subset with rows corresponding to location parameter
df_emos_bst_allvars_loc_fi <- subset(df_emos_bst_allvars_fi, par == "loc")

# Compute absolute values for all columns corresponding to a coefficients
# First 2 columns are station ID, parameter type (location or scale)
df_emos_bst_allvars_loc_fi[,3:273] <- abs(df_emos_bst_allvars_loc_fi[,3:273])
# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_bst_allvars_loc_fi$Method <- "EMOS-GB-SP"

# Remove outlier station
df_emos_bst_allvars_loc_fi <- subset(df_emos_bst_allvars_loc_fi, location != "10044") 


# Obtain the 7 most important variables (without intercept) for boxplots, for that 
# extract only the variable columns (without intercept), compute mean of absolute 
# coef values across all stations, sort variables by mean and extract the 7 most important ones
df_emos_bst_all_test <- df_emos_bst_allvars_loc_fi[,4:273]
mean.vars.all <- sort(apply(df_emos_bst_all_test, 2, mean,  na.rm=T), decreasing=TRUE)
plot.vars.emos.bst.all <- c("(Intercept)", names(mean.vars.all[1:7]))
# Extract also those of the 7 most important variables of EMOS-GB, which are not a subset 
# of the 7 most important variables of EMOS-GB-SP, anyway, in order to add boxplots of their 
# importances as well (even if they do not belong to the 7 most important variables of EMOS-GB-SP),
# as they are also candidate predictors in EMOS-GB-SP
plot.vars.emos.bst.all2 <- unique(c(plot.vars.emos.bst, plot.vars.emos.bst.all))



# Change data frame to long format
df_long_emos_bst_all <- melt(df_emos_bst_allvars_loc_fi, id.vars=c("location", "par", "Method"))


# Create data frame with the plot vars (7 most important ones) that are supposed to be shown in the plot
df_emos_bst_allvars_loc_fi.plot <- subset(df_long_emos_bst_all, variable %in% plot.vars.emos.bst.all2)
df_emos_bst_allvars_loc_fi.plot$variable <- factor(df_emos_bst_allvars_loc_fi.plot$variable, exclude=NA)


# Combine data frames of the 3 models to produce boxplots of importance values grouped by model
df.loc.all <- rbind(df_emos_loc_fi.plot, df_emos_bst_loc_fi.plot, df_emos_bst_allvars_loc_fi.plot)
df.loc.all$Method <- factor(df.loc.all$Method)

# Divide intercept values by 5 for better representation
df.loc.all[df.loc.all$variable=="(Intercept)",]$value <- df.loc.all[df.loc.all$variable=="(Intercept)",]$value/5




# Boxplots
p1 <- ggplot(df.loc.all, aes(x=variable, y=value, fill=Method, color=Method)) + 
  geom_boxplot(alpha=0.6, position = position_dodge2(preserve = "single") ) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size = 20), 
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20), axis.text=element_text(size=20)) + 
  labs(x="Predictor", y="Absolute Coefficient Value") +
  ggtitle("Location Parameter")

p1




#### For Scale Parameter ####


### EMOS ###

# Define subset with rows corresponding to scale parameter
df_emos_scale_fi <- subset(df_emos_fi, par == "scale")

# Compute absolute values for all columns corresponding to a coefficient in the model
# First 2 columns are station ID, parameter type (location or scale)
df_emos_scale_fi[,3:273] <- abs(df_emos_scale_fi[,3:273])
# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_scale_fi$Method <- "EMOS"

# Remove outlier station
df_emos_scale_fi <- subset(df_emos_scale_fi, location != "10044") 

# Change data frame to long format
df_long_emos_scale <- melt(df_emos_scale_fi, id.vars=c("location", "par", "Method"))


# For scale parameter in EMOS, only Intercept and sd of wind gusts are allowed,
# therefore, reduce data set to these 2 parameters only and create data frame
# with the variables that are supposed to be shown in the plot
vars.emos.scale <- c("(Intercept)", "VMAX_10M_sd")
df_emos_scale_fi.plot <- subset(df_long_emos_scale, variable %in% vars.emos.scale)
df_emos_scale_fi.plot$variable <- factor(df_emos_scale_fi.plot$variable, exclude=NA)



### EMOS-GB ###

# Define subset with rows corresponding to scale parameter
df_emos_bst_scale_fi <- subset(df_emos_bst_fi, par == "scale")

# Compute absolute values for all columns corresponding to a coefficient in the model
# First 2 columns are station ID, parameter type (location or scale)
df_emos_bst_scale_fi[,3:273] <- abs(df_emos_bst_scale_fi[,3:273])

# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_bst_scale_fi$Method <- "EMOS-GB"

# Remove outlier station
df_emos_bst_scale_fi <- subset(df_emos_bst_scale_fi, location != "10044") 



# Obtain the 7 most important variables (without intercept) for boxplots, for that 
# extract only the variable columns (without intercept), compute mean of absolute 
# coef values across all stations, sort variables by mean and extract the 7 most important ones
df_emos_bst_test <- df_emos_bst_scale_fi[,4:273]
mean.vars <- sort(apply(df_emos_bst_test, 2, mean, na.rm=T), decreasing=TRUE)
plot.vars.emos.bst <- c("(Intercept)", names(mean.vars[1:7]))



# Change data frame to long format
df_long_emos_bst_scale <- melt(df_emos_bst_scale_fi, id.vars=c("location", "par", "Method"))


# Create data frame with the plot vars (7 most important ones) that are supposed to be shown in the plot
df_emos_bst_scale_fi.plot <- subset(df_long_emos_bst_scale, variable %in% plot.vars.emos.bst)
df_emos_bst_scale_fi.plot$variable <- factor(df_emos_bst_scale_fi.plot$variable, exclude=NA)




### EMOS-GB-SP ###

# Define subset with rows corresponding to scale parameter
df_emos_bst_allvars_scale_fi <- subset(df_emos_bst_allvars_fi, par == "scale")


# Compute absolute values for all columns corresponding to a coefficient in the model
# First 2 columns are station ID, parameter type (location or scale)
df_emos_bst_allvars_scale_fi[,3:273] <- abs(df_emos_bst_allvars_scale_fi[,3:273])
# Introduce column where the model is coded, as later the data frames for the 3 models will be merged
# so that the results fo reach model can be identified
df_emos_bst_allvars_scale_fi$Method <- "EMOS-GB-SP"

# Remove outlier station
df_emos_bst_allvars_scale_fi <- subset(df_emos_bst_allvars_scale_fi, location != "10044") 



# Obtain the 7 most important variables (without intercept) for boxplots, for that 
# extract only the variable columns (without intercept), compute mean of absolute 
# coef values across all stations, sort variables by mean and extract the 7 most important ones
df_emos_bst_all_test <- df_emos_bst_allvars_scale_fi[,4:273]
mean.vars.all <- sort(apply(df_emos_bst_all_test, 2, mean, na.rm=T), decreasing=TRUE)
plot.vars.emos.bst.all <- c("(Intercept)", names(mean.vars.all[1:7]))
# Extract also those of the 7 most important variables of EMOS-GB, which are not a subset 
# of the 7 most important variables of EMOS-GB-SP, anyway, in order to add boxplots of their 
# importances as well (even if they do not belong to the 7 most important variables of EMOS-GB-SP),
# as they are also candidate predictors in EMOS-GB-SP
plot.vars.emos.bst.all2 <- unique(c(plot.vars.emos.bst, plot.vars.emos.bst.all))



# Change data frame to long format
df_long_emos_bst_all_scale <- melt(df_emos_bst_allvars_scale_fi, id.vars=c("location", "par", "Method"))


# Create data frame with the plot vars (7 most important ones) that are supposed to be shown in the plot
df_emos_bst_allvars_scale_fi.plot <- subset(df_long_emos_bst_all_scale, variable %in% plot.vars.emos.bst.all2)
df_emos_bst_allvars_scale_fi.plot$variable <- factor(df_emos_bst_allvars_scale_fi.plot$variable, exclude=NA)



# Combine data frames of the 3 models to produce boxplots of importance values grouped by model
df.scale.all <- rbind(df_emos_scale_fi.plot, df_emos_bst_scale_fi.plot, df_emos_bst_allvars_scale_fi.plot)
df.scale.all$Method <- factor(df.scale.all$Method)

# Divide intercept values by 5 for better representation
df.scale.all[df.scale.all$variable=="(Intercept)",]$value <- df.scale.all[df.scale.all$variable=="(Intercept)",]$value/5




# Boxplots
p2 <- ggplot(df.scale.all, aes(x=variable, y=value, fill=Method, color=Method)) + 
  geom_boxplot(alpha=0.6, position = position_dodge2(preserve = "single") ) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size = 20), 
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20), axis.text=element_text(size=20)) + 
  labs(x="Predictor", y="Absolute Coefficient Value") +
  ggtitle("Scale Parameter")

p2


# Combine plots for location and scale into one figure with package patchwork
pdf(file="C:/CIENS Data/R Code/BoxplotsImportance_00utc_12h.pdf", 
    width = 22, height = 20, pointsize = 40)
p1/p3
dev.off()
