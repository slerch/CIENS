# Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS)

This repository provides R-code and descriptions accompanying the CIENS data set:

> Schulz, B. and Lerch, S. (2024). 
> Operational convection-permitting COSMO/ICON ensemble predictions at observation sites (CIENS). 
>  Karlsruhe Institute of Technology. https://doi.org/10.35097/EOvvQEsgILoXpYTK.

Abstract:

> Abstract...

In this repository, we describe the structure of the data and provide exemplary R-code for working with the data.


## Content of data set

| Locations | ... |
| Variables | ... |
| Time range | ... |
| Lead times | ... |

Distinction standard and spatial variables.

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