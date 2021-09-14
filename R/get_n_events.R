#' Extract n rainfall events
#'
#'@description This is a very inefficient way to extract a  number of rainfall events for each landslide.
#'It is inefficient because it checkes for each month if the number of rainfall events already exceeds the
#'specified number. If not, it will start from the beginning again...
#'Some landslides will have more events than specified, because landslides from the same dates are grouped
#'in order to only load the rasters once into memory. And extraction of rainfall values is done for each landslide
#'on that day until the last landslide exceeeds the specified number of rainfal events
#'
#'@param point.data \code{sf} object of landsldies
#'@param n_dry Number of allowed dry days
#'@param daily_threshold Daily threshold that needs to be exceeded to be considered a rainy day
#'@param n.events how many rainfall events to extract for each landslide
#'@param ncores Number of cores working in parallel
#'@param date_col Character of the column that contains the dates of the landslides
#'@param id_col Character uniquely identifying each landslide
#'@param path_rainfall Character of where the rainfall data is saved
#'
#'
#'
#' @export

get_n_events = function(point.data = NULL,
                        n_dry = NULL,
                        daily_threshold = NULL,
                        n.events = NULL,
                        ncores = 2,
                        date_col = "date",
                        id_col = "PIFF_ID",
                        save = F,
                        savePath = "/dev/null",
                        path_rainfall = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/") {

  # input checks ------------------------------------------------------------

  if (is.null(point.data)) {
    stop("You need to provide some data")
  }
  if(!inherits(point.data, "sf")){
    stop("Only sf objects...")
  }
  if (!dir.exists(path_rainfall)) {
    stop("The rainfall directory does not exist")
  }



  # get n events for each slide ---------------------------------------------

  # use the future backend
  # plan(multisession, nworkers = ncores)

  # get all the slides on the same day
  slides_same_day = rainfallR::iffi10_same_day(point.data, "date")

  # use apply here, but potentially use future.lapply
  res_list = future_lapply(slides_same_day, function(x) {
    # each x is a dataframe of landslides of the same day
    # we do not know how many raster to load as the number of days back in not defined

    # how many days to the end of the month?
    get_all_events = function(y, date, days_back) {

      res_this_month = rainfallR::ex_rainfall(
        data_path = path_rainfall,
        spatial.obj = y,
        date = date,
        days_back = days_back
      ) %>% st_drop_geometry

      events = rainfallR::reconstruct_daily_rainfall_events(res_this_month, n = n_dry, daily_thresh = daily_threshold)

      events %>%
        group_by(.data[[id_col]]) %>%
        summarise(m = max(event, na.rm = T)) %>%
        pull(m) %>% max(.) -> max_event_this_month

      # recursion not super power
      if (max_event_this_month <= n.events) {
        # get the last day of the next month back
        last_day_of_prev_month = as.Date(format(events$date.x[[1]], '%Y-%m-01')) - 1
        first_day_of_prev_month = as.Date(format(events$date.x[[1]] - 30, '%Y-%m-01'))
        days_back = date - first_day_of_prev_month# mday(last_day_of_month)
        get_all_events(y, date, days_back)
      } else{
        print("Return")
        return(events)
      }

    }

    date = x[[date_col]][[1]]
    days_back = mday(date) - 1

    # get the events for this month
    res = get_all_events(x, date, days_back)
    res

  })

  return(res_list)

}






















