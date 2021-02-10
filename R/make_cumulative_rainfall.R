#' Reshape the dataframe and create cumulative count column
#'
#' @importFrom tidyr pivot_longer
#'
#' @param res an object as retured from `get_rainfall`
#'
#' @return a list of `dataframes` for each date with the cumulative amount for each day as as **one column** (long format)
#'
#' @export

make_cumulative_rainfall = function(res, days_back, cumsum=T){

  # if there are more than one points or polygons
  if(nrow(res[[1]]) == 1){

    # the output list
    out = vector("list", length=length(res))
    names(out) = names(res)

    # for each element in the res list, get the dataframe
    for (i in seq_along(res)) {

      df = res[[i]]

      # make one column with the date under consideration (probably the day of the slide)
      df[["date_consid"]] = names(out)[[1]]
      # give each slide an id
      df[["id"]] = 1:nrow(df)

      # get the geometry column
      geom = df %>% dplyr::select(date_consid)

      # make one df without geometry
      df_no_geom = df %>% st_drop_geometry()

      # columns to considerate
      cols_dates =  grep("\\d{8}", names(df_no_geom), value=T)
      cols_others = names(df_no_geom)[!names(df_no_geom) %in% cols_dates]

      # put the dates and the precip in one column
      df_long = df_no_geom %>% pivot_longer(-cols_others, names_to="date", values_to="precip", names_repair="minimal")

      # merge the geom back (can be by date_consid or id as they are the same in case of only one landslide)
      df_long = merge(geom, df_long, by="date_consid")

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

      df = res[[i]]

      # make the date of consideration a columns
      df[["date_consid"]] = names(out)[[1]]
      # give each slide (each row) a unique identifier
      df[["id"]] = 1:nrow(df)

      # get the geometry column and the id column to later merge back the geometry uniquely
      geom = df %>% dplyr::select(id)

      # make one df without geometry
      df_no_geom = df %>% st_drop_geometry()

      # columns to considerate
      cols_dates =  grep("\\d{8}", names(df_no_geom), value=T)
      cols_others = names(df_no_geom)[!names(df_no_geom) %in% cols_dates]

      # put the dates and the precip in one column
      df_long = df_no_geom %>% pivot_longer(-cols_others, names_to="date", values_to="precip", names_repair="minimal")

      # merge the geom back by id as differnt rows mean different slides mean different geoms
      df_long = merge(geom, df_long, by="id")

      # calculate the cumulative sum per geom
      # we need to group by some idetifying variable
      # this can either be the iffi, or another unique id per sldide

      df_long = df_long %>%
        group_by(id) %>%
        mutate(cumsum = cumsum(precip),
               date = as.Date(date, "%Y%m%d"))

      # get an incresing index per group
      df_long = df_long %>%
        group_by(id) %>%
        mutate(days_before_event = seq(days_back+1, 1))

        # this is WRONG ORDER
        # mutate(days_before_event = row_number())

      out[[i]] = df_long

    }

  }
  return(out)
}

