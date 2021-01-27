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
  }else{

    # how to do it for many?
    cat("Multiple geometries queried..\nOutput will be a list of dataframes (one for each day).\nThe accumulated rainfall is stored a list column")

    # also create an output list beeing the length of res (the number of dates)
    out = vector("list", length = length(res))

    # for each date (which is a dataframe)
    counter = 1 # forgot to use seq_along...
    for (day in res) {

      # get the geom of type sfc_polygon "sfc"
      geom = st_geometry(day)

      # seperate the dataframe
      day = day %>% st_drop_geometry()

      # get the dates
      dates = names(day) %>% as.Date("%Y%m%d")

      # how many days in total are we observing (days_back + 1)
      l = dim(day)[[2]]

      # this is the list that eventually will be the column in the dataframe
      acc_list = vector("list", length = nrow(day))

      # go over the polygons
      for (row in 1:nrow(day)) {

        # for each polygon (row) create a vector of accumulated rainfall
        acc_vector = vector(length = l)

        # for each col minus the geometry
        for (col in 1:l) {
          precip = day[row,col]
          acc_vector[[col]] = round(sum(acc_vector) + precip, 2)
        }

        # name the vector
        names(acc_vector) = dates

        # for each polygon add the acc_vector to the acc_list
        acc_list[[row]] = acc_vector
      }

      # add the list as list column to the dataframe
      day[["accumulated"]] = acc_list

      # add the geom
      day[["geom"]] = geom

      # make it an object of class sf
      day = st_as_sf(day)

      # put it back in the out_list
      out[[counter]] = day
      counter = counter + 1
    }
  }
  return(out)
}

