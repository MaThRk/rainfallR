library(sf)

bozen = read_sf("data/bolzano.gpkg")

usethis::use_data(bozen, overwrite = T)

#' Spatial object of the city of bozen

"bozen"
