#' Extracts the days of the slides for each polygon
#'
#' @importFrom dplyr group_by distinct
#'
#' @param poly Path to the polygon shapefile
#' @param iffi10 Path to the iffi10 shapefile with already precomputed columns in english
#'
#' @export

slide_dates_in_polygon = function(
  poly = NULL,
  iffi10 = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp",
  epsg = 32632
){

  ####### check if the paths exist
  if (is.null(poly)) {
    stop("You need to pass the path to the polygon")
  }

  if (class(poly)[1] == "sf") {
    print("using an sf-object as polygons")
    c_p = FALSE
    poly = poly
  } else{
    if (!is.character(poly)) {
      stop("The poly argument is no object of type sf, neither a path. What is it?")
    }
    if (!file.exists(poly)) {
      stop("The Polygon path is no valid spatial geometry")
    }else{
      c_t = TRUE
    }
  }



  # check if using a path to a spatial object or an object of type sf already
  if (is.character(iffi10)) {
    c = T
    if (!file.exists(iffi10)) {
      stop("The IFFI10 path is not valid")
    }
  }else{
    c = FALSE
    # if you already passed a spatial object
    if(!class(iffi10)[1] == "sf"){
      stop("The iffi parameter must be of class sf")
    }else{
      print("using an sf-object as input to the iffi-argument")
    }
  }


  ####### load the data
  if(c_t) poly = st_read(poly)
  if(c) iffi10 = st_read(iffi10)

  ####### filter the iffi data to only use dates

  # filter only the ones that have day information
  iffi10 = iffi10[iffi10$date_info == "day", ]

  ####### add an id to the iffi-data
  iffi10[["iffi_id"]] = 1:nrow(iffi10)
  poly["poly_id"] = 1:nrow(poly)

  ####### if crs are not equal, reproject everything to 32632
  iffi10 = st_transform(iffi10, epsg)
  poly = st_transform(poly, epsg)

  ####### crop the poly to the bounding box of the iffi10-points
  bb = st_bbox(iffi10) %>% st_as_sfc() %>% st_as_sf()
  poly = st_crop(poly, bb)

  ####### for each polygon check which iffi points are in it
  joined_points_on_poly = sf::st_join(poly, iffi10)

  ####### create the output dataframe
  unique_dates_slides_poly = dates_of_slides_in_poly(joined_points_on_poly)

  ####### return this dataframe
  return(unique_dates_slides_poly)

}
