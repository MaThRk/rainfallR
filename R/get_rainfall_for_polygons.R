#' Extract the rainfall for a \code{data.frame} of polygons
#'
#' @description This function build on the results of \code{slide_dates_in_polygon} which returns a df
#' where each row is a polygon with a slide that is uniquely indentifiable by its \code{poly_id} and the
#' \code{date}
#'
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom future availableCores
#'
#'
#'
#' @param landslidepoints_in_poly Object of type \code{sf} as returned by \code{slide_dates_in_polygon}
#'
#'
#' @export

get_rainfall_for_polygons = function(landslidepoints_in_poly,
                                     days_back = 5,
                                     funs = "mean",
                                     parallel = T,
                                     ncores = NULL,
                                     data_path = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/") {
  # if parallel
  if (parallel) {

    # the number of cores
    if (is.null(ncores)) {
      nc = availableCores() - 4
    } else{
      nc = ncores
    }

    # setup the workers
    registerDoParallel(nc)


    # run the function
    res = foreach(
      i = 1:nrow(landslidepoints_in_poly),
      .combine = rbind,
      .packages = c("rainfallR",
                    "dplyr",
                    "magrittr",
                    "stringr")
    ) %dopar% {
      # get the date for the slides in that slope unit
      date = landslidepoints_in_poly[i,]$date

      # the spatial object is the slope unit in the for loop
      spatial.obj = landslidepoints_in_poly[i, ]

      # lets look back 5 days
      days_back = days_back

      r = ex_rainfall(
        data_path = data_path,
        spatial.obj = spatial.obj,
        fun = "mean",
        date = date,
        days_back = days_back
      )

      r
    }

    return(res)

  }

}
