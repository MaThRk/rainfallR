#' Mean Annual precipitation per point
#'
#' Get the mean annual rainfall for a spatial dataframe
#'
#' @importFrom stars st_extract
#'
#'
#' @param data Object of class \code{sf}. Eihter POINT or POLYGON
#' @param map_path Path to raster of mean annual rainfall
#' @param fun Function to aggregated in case of POLYGON
#'
#'
#' @export

get_mean_annual_rainfall = function(data = NULL,
                                    map_path = "/mnt/CEPH_PROJECTS/Proslide/mean_annual_precipitation/map.tif",
                                    fun = c("mean"),
                                    crs = 32632) {

  if(!inherits(data, "sf")) {
    stop("The data argument must be provided with an object of class sf")
  }

  map = read_stars(map_path)

  # transform both manually
  data = st_transform(data, crs=st_crs(map))


  # point or polygon?
  pt = ifelse(any(st_geometry_type(data) == "POINT"), TRUE, FALSE)

  if (pt) {

    ex = stars::st_extract(map, data) %>% st_drop_geometry() %>% pull()
    return(ex)

    # data[["map"]]  = ex
    # return(data)

  } else{
    map = raster::raster(map_path)
    ex =  exactextractr::exact_extract(map, data, fun = fun)
    return(ex)

    # data[["map"]] = ex
    # return(data)

  }

}

