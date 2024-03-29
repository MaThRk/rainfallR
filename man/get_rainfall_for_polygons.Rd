% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rainfall_for_polygons.R
\name{get_rainfall_for_polygons}
\alias{get_rainfall_for_polygons}
\title{Extract the rainfall for a \code{data.frame} of polygons}
\usage{
get_rainfall_for_polygons(
  landslidepoints_in_poly,
  days_back = 5,
  funs = c("mean"),
  nc_var = "precipitation",
  parallel = T,
  ncores = NULL,
  data_path = "\\\\\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"
)
}
\arguments{
\item{landslidepoints_in_poly}{Object of type \code{sf} as returned by \code{slide_dates_in_polygon}}
}
\description{
This function builds on the results of \code{slide_dates_in_polygon} which returns a df
where each row is a polygon with a slide that is uniquely indentifiable by its \code{poly_id} and the
\code{date}
}
