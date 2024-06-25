# Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS)

This repository provides R-code and descriptions accompanying the CIENS data set:

> Schulz, B. and Lerch, S. (2024). 
> Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS). 
>  Karlsruhe Institute of Technology. https://doi.org/10.35097/EOvvQEsgILoXpYTK.

Abstract:

> Abstract...

In this repository, we describe the structure of the data and provide exemplary R-code for using the data.


## Structure within data platform

The data set is divided into four data sets, which are connected by an empty head "data set". Each of the five parts has its own DOI. The four data sets are listed in the following:

- `CIENS - Run 00 UTC`, observational data included; DOI: [10.35097/EOvvQEsgILoXpYTK](https://dx.doi.org/10.35097/zzfEJPxDILXwSNPH)
- `CIENS - Run 00 UTC - Spatial Variables`, DOI: [10.35097/wVDXkDCGnBgFuuGt](https://dx.doi.org/10.35097/wVDXkDCGnBgFuuGt)
- `CIENS - Run 12 UTC`, observational data included; DOI: [10.35097/JKALdQqqLIjGUOBC](https://dx.doi.org/10.35097/JKALdQqqLIjGUOBC)
- `CIENS - Run 12 UTC - Spatial Variables`, DOI: [10.35097/rJZCZYljpSReTWNL](https://dx.doi.org/10.35097/rJZCZYljpSReTWNL)


## Structure of data sets

Each of the four parts has the same directory structure. For each initialization time available, one netCDF-file is given, which includes the full ensemble forecasts of all meteorological variables (standard or spatial) for all locations and lead times. The files are grouped in monthly folders, i.e., for each month including an initialization time one folder exists.

The two standard variable data sets also include the corresponding observations. For each year, one netcdf-file is given, which includes observations of six variables for each hour of the year and the forecast locations. Note that the observational data sets for the 00 and 12 UTC are identical and include observations for all hours of the year.


## Code description

This repository includes code and data accompanying the CIENS data set. The following scripts are included:

| File | Description |
| ---- | ----------- |
| `init_file.R` | Initializes paths, packages and standard variables. |
| `ciens_info.Rdata` | Includes informationnal variables on the CIENS data set such as a vector of all available initialization times and location data. |
| `functions.R` | Functions for extracting data from netCDF-files in R-dataframes. |
| `functions_emos.R` | Functions for postprocessing via EMOS. |
| `example_00utc.R` | Example based on the 00 UTC data including postprocessing application via EMOS. |
| `example_00utc_spatial.R` | Example based on the 00 UTC data with spatial variables. |
| `example_all_data.R` | Example based on all data sets including postprocessing application via EMOS. |

The location data includes the name of the stations, their coordinates, their height and the height of the closest grid point (referred to as "orog_DE" and "orog_D2" for the corresponding model versions).


## Content of data set

The CIENS data set contains operational convection-permitting COSMO/ICON ensemble predictions of various meteorological variables at German observation sites. An overview is presented in the following table:

- 170 SYNOP stations in Germany
- Resolution: 2.8 km until 15/05/2018 (DE), 2.2 km afterwards (D2)
- 55 Meteorological variables
- Time range: 08/12/2010 - 30/06/2023
- Models are initialized daily at 00 and 12 UTC
- Number of initializations: 9150
- Forecast lead times: 0, 1, ..., 21 hours
- Forecast are taken from closest grid cell

The data sets are separated between so-called standard and spatial variables. We refer to standard variables as the meteorological variables taken from the closest grid point, while the spatial variables refer to summary statistics of surrounding grid cells. Both for the surrounding 11x11 (referred to via "_MS") and 22x22 (via "_LS" resp.) grid cells the mean and standard deviation (referred to via "_S") of the meteorological variables are calculated.

Forecasts are available for the following variables:

| Abbreviation | Unit | Full name | Levels | Spatial |
| --- | --- | --- | --- |  --- | 
| `VMAX` | m/s | Maximum wind, i.e., wind gusts | 10m | Yes |
| `U` | m/s | U-component of wind | 10m, 1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa | 10m |
| `V` | m/s | V-component of wind | 10m, 1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa | 10m |
| `OMEGA` | Pa/s | Vertical velocity (Pressure) | 1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa | No |
| `T` | K | Temperature | Ground-level, 2m, 1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa | Ground-level, 2m |
| `TD` | K | Dew point temperature | 2m | Yes |
| `RELHUM` | % | Relative humidity |  1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa | No |
| `TOT_PREC` | kg/m^2 | Total precipitation (Accumulation) | - | Yes |
| `RAIN_GSP` | kg/m^2 | Large scale rain (Accumulation) | - | Yes |
| `SNOW_GSP` | kg/m^2 | Large scale snowfall - water equivalent (Accumulation) | - | Yes |
| `W_SNOW` | kg/m^2 | Snow depth water equivalent | - | Yes |
| `W_SO` | kg/m^2 | Column integrated soil moisture | Multilayers; 1, 2, 6, 18, 54 | No |
| `CLCT` | % | Total cloud cover | `T`: Total, `L`: 800 hPa - soil, `M`: 400 hPa - 800 hPa, `H`: 000 hPa - 400 hPa | Yes |
| `HBAS_SC` | m | Cloud base above mean sea level, shallow connection | - | Yes |
| `HTOP_SC` | m | Cloud top above mean sea level, shallow connection | - | Yes |
| `ASOB_S` | W/m^2 | Net short wave radiation flux | Surface | Yes |
| `ATHB_S` | W/m^2 | Net long wave radiation flux (m) | Surface | Yes |
| `ALB_RAD` | % | Albedo (in short-wave) | - | Yes |
| `PMSL` | Pa | Pressure reduced to mean sea level | - | Yes |
| `FI` | m^2/s^2 | Geopotential |1,000 hPa, 950 hPa, 850 hPa, 700 hPa, 500 hPa |  No |

Observations are available for the following variables:

| Name | Decription |
| ---- | ----------- | 
| `wind_speed_of_gust` | Wind gusts | 
| `wind_speed` | Wind speed | 
| `wind_from_direction` | Wind direction | 
| `precipitation_amount` | Precipitation amount (hourly) | 
| `air_temperature` | Air temperature | 
| `air_pressure` | Air pressure | 
