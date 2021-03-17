## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=F, warning=F----------------------------------------------
library(rainfallR)
library(iffitoR)
library(tidyverse)
library(scales)
library(stars)
library(ncdf4)

## -----------------------------------------------------------------------------
# the path to the netcdf das
path_data = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"

# set the day you want the data
day = as.Date("2016-08-16")

# days back
days_back = 2

## -----------------------------------------------------------------------------
# this will return a list of paths. One for each month of data that we need
paths = rainfallR::get_nc_paths(path_data, day, days_back)

## ---- echo=F------------------------------------------------------------------
# open the data as ncdf4 object
ncin = ncdf4::nc_open(paths[[1]])

# read it into a stars object
ncin_stars = read_ncdf(paths[[1]])

## -----------------------------------------------------------------------------
ncin_stars

## ---- message=F---------------------------------------------------------------
# get the dimensions
dimensions = st_dimensions(ncin_stars)

# get the values of the only attribute
# this is a 3d-array 
values = ncin_stars$precipitation

## -----------------------------------------------------------------------------
# get the 16th day
single_day = ncin_stars[,,,16]
st_dimensions(single_day)

## -----------------------------------------------------------------------------
plot(single_day, main="Precipation on the 16th of August 2016")

