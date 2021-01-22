

#'
#'
get_raster_list_one_month = function(day, days_back, dates_nc, paths_to_data) {
  # last day back
  min_day = (day - days_back)
  # the dates to extract in Date format
  dates_to_extract = seq(min_day, day, by = "day")

  # read the brick once
  raster_brick = brick(paths_to_data[[1]])

  # create the output list
  raster_list = vector("list", length = length(dates_to_extract))

  # assign the names
  names(raster_list) = dates_to_extract %>% as.Date()

  # get the rasters and put them into a list
  for (i in seq_along(dates_to_extract)) {

    # year, month, day for the name
    y = format(dates_to_extract[[i]], "%Y")
    m = format(dates_to_extract[[i]], "%m")
    d = format(dates_to_extract[[i]], "%d")
    name = paste0(y,m,d)

    idx = format(dates_to_extract[[i]], "%d") %>% as.integer()
    ras = raster_brick[[idx]]
    names(ras) = name
    raster_list[[i]] = ras
  }

  return(raster_list)
}
