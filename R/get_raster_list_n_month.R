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
    # build the name for the raster
    name = paste0(y,m,d)


    # print a super informative message
    n = length(dates_to_extract)
    str = paste0(i, "/", n)
    dashes = paste0(replicate(20, "-"), collapse = "")
    cat(paste("------------", str, "(", as.Date(name, "%Y%m%d"), ")", dashes,"\n"))


    # check based on the month
    # make this a character string not a list
    # grep the more securely!!
    years_in_dates = substr(names(paths_to_data), start=5, stop=7)
    right_path = paths_to_data[grep(m, years_in_dates)][[1]]

    # open the connection
    ncin = ncdf4::nc_open(right_path[[1]])

    # dates for this month
    dates = get_dates_ncdf(ncin)

    # get the index to extract the right raster
    idx = format(dates_to_extract[[i]], "%d") %>% as.integer()


    # abbreviated path to read the brick
    right_path_abb = right_path %>% strsplit(., "/") %>% .[[1]] %>% .[c(length(.) -1, length(.))] %>%
      paste0(., collapse = "/")

    if (changed) {
      # read the brick --> this should not be done each time in the loop!!
      # only if the month changed!
      # right path must be a character string, otherwise it would take much longer
      raster_brick = raster::brick(right_path)
      n_raster = dim(raster_brick)[[3]]
      print(paste0("read ", n_raster, " rasters into brick for ", m, " ", y, " ('", right_path_abb, "')"))
    }

    # look up one fron to see if the month of the next one is the same as the current one
    # do this for all minus the last one
    if(i!=length(dates_to_extract)){
      # if the months are the same, no chage in the path needed
      if ((m == format(dates_to_extract[[i + 1]], "%m"))) {
        changed = FALSE
        # print("no change --> dont read the brick")
      } else{
        changed = TRUE
      }
    } else{
      message("Reached last day")
    }

    # print message for raster extraction
    message(paste0("   Getting the raster for date: "), c(y,m,d))

    # extract the date
    ras = raster_brick[[idx]]
    names(ras) = name


    # put into the list
    raster_list[[i]] = ras

  }

  return(raster_list)

}
