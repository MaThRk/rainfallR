#' Reshape the dataframe and create cumulative count column
#'
#' @importFrom tidyr pivot_longer
#'
#' @param res an object as retured from `get_rainfall`
#'
#' @return a list of `dataframes` for each date with the cumulative amount for each day as column and the dates as another column.
#'
#' @export
get_cumulative_rainfall = function(res, cumsum=T){

  # the output list
  out = vector("list", length=length(res))
  names(out) = names(res)

  # for each element in the res list, get the dataframe
  for (i in seq_along(res)) {

    df = res[[i]]

    # get the geometry column
    geom = df %>% st_geometry()

    df_long = df %>%
      st_drop_geometry() %>%
      pivot_longer(cols = everything(), names_to="dates", values_to="precip") %>%
      mutate(dates = as.Date(dates, "%Y%m%d")) %>%
      # get the cumulative count for the days
      mutate(accumulated = cumsum(precip)) %>%
      mutate(geom = geom) %>%
      st_as_sf()

    # put back in the output list
    out[[i]] = df_long
  }
  return(out)
}
