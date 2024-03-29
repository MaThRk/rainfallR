#' Get the summed yearly rainfall
#'
#' This function takes either a polygon, or nothing (than it's pixel based) and calculates the sum of rainfall for each polygon
#' (each pixel) for each year
#'
#'
#' @importFrom stars read_stars st_apply st_set_dimensions
#'
#'@param aggre_fun If you pass a polygon, the values need to be aggregated somehow
#'@param fun How to combine the individual values. E.G. the mean for each pixel for the years 1980, 1981 and 1982 requires the
#' the fun to be "mean". The maximum value for each month in the years 80,81,82 requires the \code{by}-parameter to be \code{month}
#' and the \code{fun}-parameter to be \code{max}
#' @param return.list if \code{True} it will return a list of stars oject. If \code{false} will return one stars object (if \code{by} = \code{year}),
#' and a list of stars-objects if \code{by = 'month'}
#'
#' @export

aggregate_rainfall = function(path_ncdf = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/",
                              polygon = NULL,
                              years = 1980:2018,
                              by = "year",
                              daily_fun = NULL,
                              # how to aggregate the values in one month
                              monthly_fun = NULL,
                              # how to aggregate the monthly values
                              return.list = FALSE,
                              aggre_fun = "mean") {


  # if there is a polygon
  if (!is.null(polygon)) {
    polygon = check_spatial_input(polygon)
  }

  # get the paths to the data
  paths = get_monthly_paths(years, path_ncdf)

  if(is.null(polygon)){
    rasters = make_rasters(paths, years, by, daily_fun, monthly_fun, return.list=FALSE)
    return(rasters)
  }else{
    table = make_aggregated_table(paths, polygon, years, by, daily_fun, monthly_fun, aggre_fun)
    return(table)
  }
}

