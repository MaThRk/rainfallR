
#' @export
get_raster_list_one_month = function(day, days_back, dates_nc, paths_to_data) {

  # last day back
  min_day = (day - days_back)
  # the dates to extract in Date format
  dates_to_extract = seq(min_day, day, by = "day")

  # read the brick once
  ncdf_file = paths_to_data[[1]] %>% str_split(., "/") %>% .[[1]] %>% .[[length(.)]]
  message(paste0("Connecting to: "), ncdf_file)
  # reading it as one raster-brick
  raster_brick = brick(paths_to_data[[1]])

  # create the output list
  raster_list = vector("list", length = length(dates_to_extract))

  # assign the names
  names(raster_list) = dates_to_extract %>% as.Date()

  # get the rasters and put them into a list
  print(paste0("Extraxting ", days_back, " dates prior to: ", day))
  for (i in seq_along(dates_to_extract)) {

    # year, month, day for the name of the raster
    y = format(dates_to_extract[[i]], "%Y")
    m = format(dates_to_extract[[i]], "%m")
    d = format(dates_to_extract[[i]], "%d")
    name = paste0(y, m, d)


    # print a super informative message
    n = length(dates_to_extract)
    str = paste0(i, "/", n)
    dashes = paste0(replicate(20, "-"), collapse = "")
    cat(paste("------------", str, "(", as.Date(name, "%Y%m%d"), ")", dashes,"\n"))

    # print message for raster extraction
    message(paste0("  Getting the raster for date: "), c(y,m,d))

    # the index (the day) for extracting the right raster
    idx = format(dates_to_extract[[i]], "%d") %>% as.integer()
    # extract it
    ras = raster_brick[[idx]]
    # assign it the name
    names(ras) = name
    raster_list[[i]] = ras
  }

  return(raster_list)
}
