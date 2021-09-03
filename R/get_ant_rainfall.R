#' Extract antecedent rainfall variables
#'
#' @importFrom sf st_drop_geometry
#'
#' @importFrom dplyr bind_cols
#' @param date_landslide The name of the column with date information about the landslides.
#'  Cells can either be NA or of class Date

#' @export
get_ant_rainfall = function(
                            data = NULL,
                            id = "eidee",
                            date_landslide = "date",
                            vars = c(0, 1, 3, 5),
                            dates_for_vars = NULL,
                            parallel = T,
                            rainfall_path = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/") {

  # check if the object is an object of type sf
  if (!inherits(data, "sf")) {
    stop("Only sf objects. Please....")
  }

  # check if the date column is present and actually contains dates
  col_names = colnames(data)
  if (!any(grepl(date_landslide, col_names))) {
    stop("The name of the date column doesnt seem to exist")
  } else{
    # get the index
    idx_date_col = which(grepl(date_landslide, col_names))
    col = st_drop_geometry(data[, idx_date_col])
    vals = col[, 1, drop = T]
    # vals = vals[!is.na(vals)]

    # check if either NA or date
    if (!inherits(vals, "Date")) {
      stop("The date column does contain values other than Date and NA")
    }
  }

  if (is.null(dates_for_vars)) {
    stop("No dates specified for the extraction of rainfall variables")
  }


  # for each day, for each landslide get each variable ----------------------

  # set some names
  vars = setNames(vars, as.character(vars))
  dates_for_vars = setNames(dates_for_vars, gsub("-", "", dates_for_vars))

  # get the max of the vars
  # When extracting these many days for each day and slide one can get the other vars
  max_var = max(vars)

  # for each day get the rainfall of max days back
  if (parallel) {

    future::plan(multisession)

    LIST_OF_VARS = future.apply::future_lapply(dates_for_vars, function(x) {

      cat(paste0("Getting the variables for: ", x, " ...\n"))

      # get the paths to the data
      paths_to_data = get_nc_paths(rainfall_path, x, max_var)

      # get the raster for the days back
      ras = suppressWarnings(get_raster_list_n_month(paths_to_data, x, max_var, quiet =
                                                       T))

      # extract the the rainfall for each point for eachd day
      res = lapply(ras, function(y) {
        # this extract the rainfall value
        vals = raster::extract(y, data, sp = T)
        vals = as.data.frame(vals)
        idx = grep(glue("{id}|X.*"), names(vals))
        vals_df = vals[, idx]

      })

      # get the variables for each entry
      res_df = suppressMessages(bind_cols(res))

      # remove all ids minus the first and rename this
      idx_ids = grep(id, names(res_df))
      idx_ids_remove = idx_ids[2:length(idx_ids)]
      idx_id_rename = idx_ids[[1]]
      res_df_clean = res_df[,-idx_ids_remove] %>%
        as_tibble() %>% # why is the tibble important here??
        rename("{id}" := {
          {
            idx_id_rename
          }
        }) %>%
        pivot_longer(
          cols = matches("X\\d{8}$"),
          names_to = "date",
          values_to = "rain"
        ) %>%
        mutate(date = str_remove(date, "X"),
               date = as.Date(date, format = "%Y%m%d")) %>%
        group_by(.data[[id]]) %>%
        arrange(date, .by_group = T)

      res_vars = lapply(vars, function(v) {
        # should still be grouped
        res_df_clean %>%
          summarise("p_{v}" := sum(rain[1:v]))
      })

    })
  } else{
    list_of_vars = lapply(dates_for_vars, function(x) {
      cat(paste0("Getting the variables for: ", x, " ...\n"))

      # get the paths to the data
      paths_to_data = get_nc_paths(rainfall_path, x, max_var)

      # get the raster for the days back
      ras = suppressWarnings(get_raster_list_n_month(paths_to_data, x, max_var, quiet =
                                                       T))

      # extract the the rainfall for each point for eachd day
      res = lapply(ras, function(y) {
        # this extract the rainfall value
        vals = raster::extract(y, data, sp = T)
        vals = as.data.frame(vals)
        idx = grep(glue("{id}|X.*"), names(vals))
        vals_df = vals[, idx]

      })

      # get the variables for each entry
      res_df = suppressMessages(bind_cols(res))

      # remove all ids minus the first and rename this
      idx_ids = grep(id, names(res_df))
      idx_ids_remove = idx_ids[2:length(idx_ids)]
      idx_id_rename = idx_ids[[1]]
      res_df_clean = res_df[,-idx_ids_remove] %>%
        as_tibble() %>% # why is the tibble important here??
        rename("{id}" := {
          {
            idx_id_rename
          }
        }) %>%
        pivot_longer(
          cols = matches("X\\d{8}$"),
          names_to = "date",
          values_to = "rain"
        ) %>%
        mutate(date = str_remove(date, "X"),
               date = as.Date(date, format = "%Y%m%d")) %>%
        group_by(.data[[id]]) %>%
        arrange(date, .by_group = T)

      res_vars = lapply(vars, function(v) {
        # should still be grouped
        res_df_clean %>%
          summarise("p_{v}" := sum(rain[1:v]))
      })

    }
    )


    # bind them together
    res_vars_df = suppressMessages(bind_cols(res_vars))

    idx_ids = grep(id, names(res_vars_df))
    idx_ids_remove = idx_ids[2:length(idx_ids)]
    idx_id_rename = idx_ids[[1]]
    res_vars_clean = res_vars_df[, -idx_ids_remove] %>%
      as_tibble() %>% # why is the tibble important here??
      rename("{id}" := {
        {
          idx_id_rename
        }
      })

  }

  # bind them all together
  ret = data.table::rbindlist(list_of_vars, idcol = "date")
  ret[[d]] = as.Date(ret[[d]], format = "%Y%m%d")

  return(ret)
}



