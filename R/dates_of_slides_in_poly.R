#' For each polygon with slide it keeps one row per date
#'
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#'
#' @param joined_points_on_poly An object of sf where the points were joined on the polygon
#'
#'
dates_of_slides_in_poly = function(joined_points_on_poly){

  ####### from the spatially joined data only select the polys which have an landsld_id
  polys_with_points = joined_points_on_poly[!is.na(joined_points_on_poly$landsld_id), ]

  # now all polygons with the same poly_id have the same spatial information.
  # only their date and information from the dataframe can change

  slides_per_poly_date = polys_with_points %>%
    group_by(poly_id, date) %>%
    mutate(slides_per_poly_date = n()) %>%
    distinct(poly_id, date, .keep_all = TRUE) %>%
    ungroup()

    return(slides_per_poly_date)

}
