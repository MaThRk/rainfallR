#' Get the rainfall-(events) for point data

#' @description Wrapper-funtion to calculate the rainfall, the cumulated sum, and
#' the events for point-data for specific dates (e.g. dates of landslides)
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter group_by
#' @importFrom future plan
#' @importFrom doSNOW registerDoSNOW
#' @importFrom snow makeCluster
#' @importFrom here here
#'
#' @export
get_rainfall_point_data = function(point.data = NULL,
                                   days_back = 10,
                                   daily_thresh = 0,
                                   n_dry = 0,
                                   parallel = TRUE,
                                   ncores = 8,
                                   last_event = FALSE,
                                   nle = FALSE,
                                   save = F,
                                   base_path = NULL,
                                   path_rainfall = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/") {


  # -------------------------------------------------------------------------
  # Some input checks


  if (is.null(point.data)) {
    stop("You need to provide some data")
  }

  if (is.null(days_back)) {
    stop("You need to specify the number of days you want to extract the rainfall")
  }

  if (!dir.exists(path_rainfall)) {
    stop("The rainfall directory does not exist")
  }

  # -------------------------------------------------------------------------
  # cant have last_event and nle as last_event would imply only the last rainfall
  # event before the slide
  if (last_event & nle) {
    stop(
      red("Either you want the last rainfall-event before the landlside or all the
triggering and non-triggering rainfall-events in the specified time-period"
    ))
  }

  # when we want to save
  if (save) {
    if (is.null(base_path)) {
      stop("You want to save the data, but you need to provide a base path")
    }
    if(!dir.exists(base_path)){
      cat(paste0("Creating Directory: ", base_path))
      dir.create(base_path, recursive = T)
    }
  }

  # create the path to the data ---------------------------------------------

  n = nrow(point.data)

  data_out = ifelse(!last_event, here(
    paste0(
      base_path,
      "/",
      n,
      "_",
      "days_back",
      days_back,
      "_daily_thresh",
      daily_thresh,
      "_n_dry",
      n_dry,
      "_NLE_",
      nle,
      ".Rdata"
    )
  ),
  here(
    paste0(
      base_path,
      "/LASTEVENT_n",
      n,
      "_",
      "days_back",
      days_back,
      "_daily_thresh",
      daily_thresh,
      "_n_dry",
      n_dry,
      "_NLE_",
      nle,
      ".Rdata"
    )
  ))


  if (!file.exists(data_out)) {
    ######################################
    # Get the rainfall data
    ######################################

    # list of the slides on the same day --------------------------------------
    slides_same_day = rainfallR::iffi10_same_day(point.data)

    # extract the rainfall data in parallel
    if (parallel) {
      # extract in parallel -----------------------------------------------------

      # use snow backend
      cl <- makeCluster(ncores)
      registerDoSNOW(cl)

      # progressbar
      pb = txtProgressBar(max = length(slides_same_day), style = 3)
      progress = function(n) setTxtProgressBar(pb, n)
      opts = list(progress = progress)

      res = foreach(
        i = 1:length(slides_same_day),
        .combine = rbind,
        .packages = c("rainfallR",
                      "magrittr",
                      "stringr",
                      "dplyr"),
        .options.snow = opts
      ) %dopar% {
        # progressbar
        # setTxtProgressBar(pb, i)

        # get the date of the slides
        date = names(slides_same_day)[[i]] %>% as.Date(., "%Y%m%d")

        # the spatial object. All slides from the same day in South Tyrol
        # Like this we only need to load the NetCDF once into memory
        spatial.obj = slides_same_day[[i]]

        # this returns a dataframe
        rf = rainfallR::ex_rainfall(
          data_path = path_rainfall,
          spatial.obj = spatial.obj,
          fun = NULL,
          # as we are using points
          date = date,
          days_back = days_back
        )
      }
    }


    # Reconstruct the rainfall events -----------------------------------------

    # create a list for each landslide
    slides_list = split(res, res$PIFF_ID)

    # using the foreach approach with the future backend
    registerDoParallel(ncores)
    slides_with_rainfall_events = foreach(i = 1:length(slides_list)) %dopar% {
      r = rainfallR::reconstruct_daily_rainfall_events(slides_list[[i]],
                                                       n = n_dry,
                                                       daily_thresh = daily_thresh,
                                                       quiet = T)
    }

    # put the results back into one dataframe ---------------------------------
    df_slides = data.table::rbindlist(slides_with_rainfall_events) %>% st_as_sf()





    if (last_event) {
      # E
      df_rainfall = df_slides %>%
        group_by(PIFF_ID) %>%
        filter(event != is.na(event)) %>%
        filter(event == last(event)) %>% # this will get the last event
        mutate(
          diff_rain_slide = last(date.y) - last(date.x),
          diff_rain_slide = as.numeric(diff_rain_slide)
        ) %>%
        ungroup() %>%
        group_by(PIFF_ID, second_level, area, diff_rain_slide) %>% # which columns to i want to keep? piff_id, second_level and area are all the same for each of the n days per slide
        summarise(sum_rainfall = sum(precip))

      # D
      df_duration = df_slides %>%
        group_by(PIFF_ID) %>%
        filter(event != is.na(event)) %>%
        filter(event == last(event)) %>% # this will get the last eventj
        ungroup() %>%
        group_by(PIFF_ID, second_level, area) %>%
        summarise(duration = n(),
                  intensity = sum(precip) / n()) %>%
        st_drop_geometry()

      # merge the two
      df_slides = merge(df_rainfall,
                        df_duration,
                        by = "PIFF_ID",
                        all.y = F)
    }

    if(nle){

      df_slides = df_slides %>%
        dplyr::filter(!is.na(event)) %>%
        dplyr::group_by(PIFF_ID, event) %>%
        mutate(
          class = ifelse(any(date.x == dol), "trigger", "notrigger")
        )

    }

    # save it
    if (save) {
      cat("saving...")
      saveRDS(df_slides, data_out)
    }

  } else{
    warning("The File aready exists. Reading from disk...")
    df_slides = readRDS(data_out)
  }

  # return the object
  return(df_slides)

}
