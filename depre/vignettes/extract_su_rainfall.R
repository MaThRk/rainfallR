## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----pkgs, message=F----------------------------------------------------------
library(tidyverse)
library(iffitoR)
library(sf)
library(forcats)
library(glue)
library(purrr)
library(stringr)
library(crayon) # for some colours
library(raster)

## -----------------------------------------------------------------------------
# which os to automatically set the paths
os = Sys.info()["sysname"]

if(os == "Linux"){
  path_ncdf = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/"
  su_path = "/mnt/CEPH_PROJECTS/Proslide/Envir_data/SlopeUnits/su_opt_16_TAA/su_16_TAA.shp"
}else if(os == "Windows"){
  path_ncdf = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"
  su_path = "\\\\projectdata.eurac.edu/projects/Proslide/Envir_data/SlopeUnits/su_opt_16_TAA/su_16_TAA.shp"
}else{
  stop(call. = F, "what the hell are you working on...")
}

## -----------------------------------------------------------------------------
# get vector of South Tyrol
st = iffitoR::get_shape_southtyrol()
st = st %>% st_union()
ggplot() +
  geom_sf(data=st) +
  theme_minimal()

## -----------------------------------------------------------------------------
su = st_read(su_path)
# and verify directly if they are in the same crs
st_crs(su) == st_crs(st)

## ----reprok, warning=F--------------------------------------------------------
st_reproj = st_transform(st,st_crs(su))
st_crs(st_reproj) == st_crs(su)

