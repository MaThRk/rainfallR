#' Reshape the dataframe and create cumulative count column
#'
#' @importFrom tidyr pivot_longer
#'
#' @param res an object as retured from `get_rainfall`
#'
#' @return a list of `dataframes` for each date with the cumulative amount for each day as as **one column** (long format)
#'
#' @export

make_cumulative_rainfall = function(res, cumsum=T){

  # if there are more than one points or polygons
  if(nrow(res[[1]]) == 1){

    cat("Extracting data for a single geometry")

    # the output list
    out = vector("list", length=length(res))
    names(out) = names(res)

    # for each element in the res list, get the dataframe
    for (i in seq_along(res)) {

      df = res[[i]]

      # get the geometry column
      geom = df %>% select(iffi)

      # make one df without geometry
      day = df %>% st_drop_geometry()

      # put the dates and the precip in one column
      df_long = day %>% pivot_longer(!iffi, names_to="date", values_to="precip")

      # merge the iffi and the geom back
      df_long = merge(geom, df_long, by="iffi")

      # calculate the cumulated sum
      df_long = df_long %>%
        group_by(iffi) %>%
        mutate(cumsum = cumsum(precip),
               date = as.Date(date, "%Y%m%d"))

      out[[i]] = df_long
    }
  }else{

    out = vector("list", length = length(res))

    # for each date
    for (i in seq_along(res)) {

      # make one df in long format
      geom = res[[i]] %>% select(iffi)
      day = res[[i]] %>% st_drop_geometry()
      df_long = day %>% pivot_longer(!iffi, names_to="date", values_to="precip")

      # match the geom_back
      df_long = merge(df_long, geom, by="iffi")

      # calculate the cumulative sum
      df_long = df_long %>%
        group_by(iffi) %>%
        mutate(cumsum = cumsum(precip),
               date = as.Date(date, "%Y%m%d"))

      out[[i]] = df_long
    }

  }

  return(out)

}
