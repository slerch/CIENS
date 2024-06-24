# Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS)

This repository provides R-code and descriptions accompanying the CIENS data set:

> Schulz, B. and Lerch, S. (2024). 
> Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS). 
>  Karlsruhe Institute of Technology. https://doi.org/10.35097/EOvvQEsgILoXpYTK.

Abstract:

> Abstract...

In this repository, we describe the structure of the data and provide exemplary R-code for working with the data.


## Structure within data platform

The data set divided into an empty head data sets and four data sets, each with its own DOI:

| Name | DOI | Run | Variables | Observations |
| ---- | ----------- | 
| `CIENS - Run 00 UTC` | [10.35097/EOvvQEsgILoXpYTK](https://dx.doi.org/10.35097/zzfEJPxDILXwSNPH) | 00 UTC | Standard | Yes |
| `CIENS - Run 00 UTC - Spatial Variables` | [10.35097/wVDXkDCGnBgFuuGt](https://dx.doi.org/10.35097/wVDXkDCGnBgFuuGt) | 00 UTC | Spatial | No |
| `CIENS - Run 12 UTC` | [10.35097/JKALdQqqLIjGUOBC](https://dx.doi.org/10.35097/JKALdQqqLIjGUOBC) | 12 UTC | Standard | Yes |
| `CIENS - Run 12 UTC - Spatial Variables` | [10.35097/rJZCZYljpSReTWNL](https://dx.doi.org/10.35097/rJZCZYljpSReTWNL) | 12 UTC | Spatial | No |


## Structure of data sets

Each of the four parts has the same directory structure. For each initialization time within the time period of the CIENS data set, one netCDF-file is available, which includes the full ensemble forecasts of all meteorological variables (standard or spatial) for all locations and lead times. The files are grouped in monthly folders, i.e., for each month including an initialization time one folder exists.

Description observational data base.


## Code description

This repository includes code and data accompanying the CIENS data set. The following scripts are included:

| File | Description |
| ---- | ----------- | 
| `example_00utc.R` | Example based on the 00 UTC data including postprocessing application via EMOS. |
| `example_00utc_spatial.R` | Example based on the 00 UTC data with spatial variables. |
| `example_all_data.R` | Example based on all data sets including postprocessing application via EMOS. |
| `functions.R` | Functions for extracting data from netCDF-files in R-dataframes. |
| `functions_emos.R` | Functions for postprocessing via EMOS. |
| `init_file.R` | Initializes paths, packages and standard variables. |
| `ciens_info.R` | Includes informationnal variables on the CIENS data set such as a vector of all available initialization times and location data. |

The location data includes the name of the stations, their coordinates, their height and the height of the closest grid point (referred to as "orog_DE" and "orog_D2" for the corresponding model versions).


## Content of data set

The CIENS data set contains operational convection-permitting COSMO/ICON ensemble predictions of various meteorological variables at German observation sites. An overview is presented in the following table:

| Locations | xxx stations in Germany |
| Resolution | 2.8 km (DE; until xx/xx/xxxx), 2.2 km (D2; afterwards) |
| Variables | xx meteorological variables |
| Time range | 08/12/2010 - 30/06/2023 |
| Initialization hours | 00 and 12 UTC |
| Forecast lead times | 0-21 hours |
| Forecast derivation | Forecast from closest grid cell  |

The data sets are separated between so-called standard and spatial variables. We refer to standard variables as the meteorological variables taken from the closest grid point, while the spatial variables refer to summary statistics of surrounding grid cells. Both for the surrounding 11x11 (referred to via "_MS") and 22x22 (via "_LS" resp.) grid cells the mean and standard deviation (referred to via "_SD") of the meteorological variables are calculated.

Forecasts are available for the following variables:

| Abbreviation | Decription | Spatial? |
| ---- | ----------- | 
| `VMAX` | m/s | Maximum wind, i.e.\ wind gusts (10m) |
| `U` | m/s | U-component of wind (10m, 500--1,000 hPa) |
| `V` | m/s | V-component of wind (10m, 500--1,000 hPa) |
| `OMEGA` | Pa/s | Vertical velocity (Pressure) (500--1,000 hPa) |
| `T` | K | Temperature (Ground-level, 2m, 500--1,000 hPa) |
| `TD` | K | Dew point temperature (2m) |
| `RELHUM` | % | Relative humidity (500--1,000 hPa) |
| `TOT_PREC` | kg/m^2 | Total precipitation (Accumulation) |
| `RAIN_GSP` | kg/m^2 | Large scale rain (Accumulation) |
| `SNOW_GSP` | kg/m^2 | Large scale snowfall - water equivalent (Accumulation) |
| `W_SNOW` | kg/m^2 | Snow depth water equivalent |
| `W_SO` | kg/m^2 | Column integrated soil moisture (multilayers; 1, 2, 6, 18, 54) |
| `CLCT` | % | Total cloud cover |
| `CLCL` | % | Cloud cover (800 hPa - soil) |
| `CLCM` | % | Cloud cover (400 hPa - 800 hPa) |
| `CLCH` | % | Cloud cover (000 hPa - 400 hPa) |
| `HBAS_SC` | m | Cloud base above mean sea level, shallow connection |
| `HTOP_SC` | m | Cloud top above mean sea level, shallow connection |
| `ASOB_S` | W/m^2 | Net short wave radiation flux (at the surface) |
| `ATHB_S` | W/m^2 | Net long wave radiation flux (m) (at the surface) |
| `ALB_RAD` | % | Albedo (in short-wave) |
| `PMSL` | Pa | Pressure reduced to mean sea level |
| `FI` | m^2/s^2 | Geopotential (500--1,000 hPa) |

Observations are available for the following variables:

| Name | Decription |
| ---- | ----------- | 
| `wind_speed_of_gust` | Wind gusts | 
| `wind_speed` | Wind speed | 
| `wind_from_direction` | Wind direction | 
| `precipitation_amount` | Precipitation amount (hourly) | 
| `air_temperature` | Air temperature | 
| `air_pressure` | Air pressure | 