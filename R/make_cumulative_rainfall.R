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

    # the output list
    out = vector("list", length=length(res))
    names(out) = names(res)

    # for each element in the res list, get the dataframe
    for (i in seq_along(res)) {

      df = res[[i]]

      # iffi in names
      iffi = ifelse("iffi" %in% names(df), TRUE, FALSE)

      # get the geometry column
      if(iffi){
        geom = df %>% dplyr::select(iffi)
      }else{
        geom = st_geometry(df)
      }


      # make one df without geometry
      day = df %>% st_drop_geometry()

      # put the dates and the precip in one column
      if(iffi){
        df_long = day %>% pivot_longer(!iffi, names_to="date", values_to="precip")
      }else{
        df_long = day %>% pivot_longer(everything(), names_to="date", values_to="precip")
      }

      # merge the iffi and the geom back
      if(iffi) df_long = merge(geom, df_long, by="iffi")

      # calculate the cumulated sum
      df_long = df_long %>%
        mutate(cumsum = cumsum(precip),
               date = as.Date(date, "%Y%m%d")) %>%

        # add a column for the days_back (1 = the day we specified)
        mutate(days_before_event = (nrow(.):1))

      out[[i]] = df_long
    }
  }else{

    out = vector("list", length = length(res))
    # set the names
    names(out) = names(res)

    # for each date
    for (i in seq_along(res)) {

      # make one dataframe
      df = res[[i]]

      # iffi in names
      iffi = ifelse("iffi" %in% names(df), TRUE, FALSE)
      # if there is no iffi-identifier we need another unique identifier per row
      # add a unique id
      if(!iffi) df[["id"]] = 1:nrow(df)


      # get the geometry column
      if(iffi){
        geom = df %>% dplyr::select(iffi)
      }else{
        geom = df %>% dplyr::select(id)
      }

      # a df without geom
      day = df %>% st_drop_geometry()


      # put the dates and the precip in one column
      if(iffi){
        df_long = day %>% pivot_longer(!iffi, names_to="date", values_to="precip")
      }else{
        df_long = day %>% pivot_longer(!id, names_to="date", values_to="precip")
      }

      # merge the iffi and the geom back
      if(iffi){
        df_long = merge(geom, df_long, by="iffi")
      } else{
        df_long = merge(geom, df_long, by="id")
      }

      # calculate the cumulative sum per geom
      # we need to group by some idetifying variable
      # this can either be the iffi, or another unique id per sldide

      if(iffi){
        df_long = df_long %>%
          group_by(iffi) %>%
          mutate(cumsum = cumsum(precip),
                 date = as.Date(date, "%Y%m%d"))

        # get an incresing index per group
        df_long$days_before_event = with(df_long, ave(seq_along(iffi), iffi, FUN = seq_along))

        out[[i]] = df_long
      }else{
        df_long = df_long %>%
          group_by(id) %>%
          mutate(cumsum = cumsum(precip),
                 date = as.Date(date, "%Y%m%d"))

        # get an incresing index per group
        df_long$days_before_event = with(df_long, ave(seq_along(id), id, FUN = seq_along))

        out[[i]] = df_long
      }

    }

  }
  return(out)
}

