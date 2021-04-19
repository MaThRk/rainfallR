#' Extracts the days of the slides for each polygon
#'
#' @description  For each Polygon this function selects only the polygons that experienced a landsld. It also reports (in the column \code{slide_per_poly_date})
#' the number of slides on that date in that polygon.shapefile
#'
#' @return An object of class \code{sf} which is (probably) of geometry-type polygon with the additional columns.
#'
#' @importFrom dplyr group_by distinct
#'
#' @param poly Path to the polygon shapefile
#' @param landsld Path to the landsld shapefile or object of class \code{sf}. Needs to have at least a column called \code{date_info}
#' (as in the classic iffi-data from the \code{iffitoR}-package) or column called \code{date} with dates of class \code{date}. If both do not
#' exist, the first column with the class \code{date} will be used to subset only the landslides with day-precise information
#'
#'
#' @export

slide_dates_in_polygon = function(
  poly = NULL,
  landsld = NULL,
  epsg = 32632
){

  ####### check if input provided
  if (is.null(poly)) {
    stop("You need to provide some polygons")
  }

  if (is.null(landsld)) {
    stop("You need to provide some landsldide dataframe")
  }

  ##### Check the inputs

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
      c_p = TRUE
    }
  }

  # check if using a path to a spatial object or an object of type sf already
  if (is.character(landsld)) {
    c = T
    if (!file.exists(landsld)) {
      stop("The landsld path is not valid")
    }
  }else{
    c = FALSE
    # if you already passed a spatial object
    if(!class(landsld)[1] == "sf"){
      stop("The iffi parameter must be of class sf")
    }else{
      print("using an sf-object as input to the iffi-argument")
    }
  }


  ####### load the data
  if(c_p) poly = st_read(poly)
  if(c) landsld = st_read(landsld)

  ####### filter the iffi data to only use dates

  # filter only the ones that have day information
  # get the names
  nams = names(landsld)

  if ("date_info" %in% nams) {
    if (all(grepl("day|month|year|no date", landsld[["date_info"]]))) {
      landsld = landsld[landsld$date_info == "day", ]
    }
  } else if("date" %in% nams){
    if (all(grepl("day|month|year|no date", landsld[["date"]]))) {
      landsld = landsld[landsld$date_info == "day", ]
    } else if(class(landsld[["date"]]) == "Date"){
    cat("Using the column called 'date' to subset the dataframe")
     landsld = landsld[!is.na(landsld[["date"]]), ]
    }
  }else if(any(grepl("Date", sapply(landsld, class)))){
    cat("Using the first column with class 'Date' to subset the dataframe")
    cls = sapply(landsld, class)
    col_idx = grep("Date", cls)[[1]]
    col = landsld[,col_idx] %>% st_drop_geometry()
    bin = !is.na(col)
    landsld = landsld[bin, ]
  }

  ####### add an id to the iffi-data
  landsld[["landsld_id"]] = 1:nrow(landsld)
  poly["poly_id"] = 1:nrow(poly)

  ####### if crs are not equal, reproject everything to 32632
  landsld = st_transform(landsld, epsg)
  poly = st_transform(poly, epsg)

  ####### crop the poly to the bounding box of the landld-points
  bb = st_bbox(landsld) %>% st_as_sfc() %>% st_as_sf()
  poly = st_crop(poly, bb)

  ####### for each polygon check which iffi points are in it
  joined_points_on_poly = sf::st_join(poly, landsld)

  ####### create the output dataframe
  unique_dates_slides_poly = dates_of_slides_in_poly(joined_points_on_poly)

  ####### return this dataframe
  return(unique_dates_slides_poly)

}
