
#'
#'
get_raster_list_one_month = function(d, days_back, dates_nc, paths_to_data){
  # get the indices into the rasterbrick
  d = as.integer(d)
  # sort them to be sure
  days_to_extract = sort(seq(d, d-days_back))
  days_names = dates_nc[as.integer(format(dates_nc, "%d")) %in%
                          as.integer(days_to_extract)]

  raster_brick = brick(paths_to_data[[1]])
  raster_list = vector("list", length=length(days_to_extract))
  names(raster_list) = days_names %>% as.character()

  # get the rasters and put them into a list
  for (i in seq(days_to_extract)) {
    ras = raster_brick[[days_to_extract[[i]]]]
    raster_list[[i]] = ras
  }

  return(raster_list)
}
