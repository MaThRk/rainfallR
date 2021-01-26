#' @export
get_raster_list_n_month = function(paths_to_data, day, days_back){

  # get the max day back
  min_day = (day - days_back)

  # create a sequence
  dates_to_extract = seq(min_day, day, by="day")

  # one raster list for all dates in the end
  raster_list = vector("list", length=length(dates_to_extract))
  names(raster_list) = dates_to_extract

  # for each date
  changed = T
  for (i in seq_along(dates_to_extract)) {


    # month of the date to extract
    m = format(dates_to_extract[[i]], "%m")
    y = format(dates_to_extract[[i]], "%Y")
    d = format(dates_to_extract[[i]], "%d")

    name = paste0(y,m,d)

    # check based on the month
    right_path = paths_to_data[grep(m, names(paths_to_data))]

    # open the connection
    ncin = ncdf4::nc_open(right_path[[1]])

    # dates for this month
    dates = get_dates_ncdf(ncin)

    # get the index to extract
    idx = format(dates_to_extract[[i]], "%d") %>% as.integer()


    if (changed) {
      print(paste0("read the brick for: ", m, " ", y))
      # read the brick --> this should not be done each time in the loop!!
      raster_brick = raster::brick(right_path)
    }

    # up to the second last one (-1)
    if(i!=length(dates_to_extract)){
      if ((m == format(dates_to_extract[[i + 1]], "%m"))) {
        changed = FALSE
        # print("no change --> dont read the brick")
      } else{
        changed = TRUE
      }
    } else{
      print("Reached last day")
    }

    message(paste0("reading the data for date: "), c(y,m,d), i)

    # extract the date
    ras = raster_brick[[idx]]
    names(ras) = name


    # put into the list
    raster_list[[i]] = ras

  }

  return(raster_list)

}
