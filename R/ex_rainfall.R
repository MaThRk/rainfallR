#' Extract rainfall data
#'
#' @importFrom raster raster extract brick crs
#' @importFrom  dplyr select rename_with mutate bind_cols pull
#' @importFrom  ncdf4 nc_open
#' @import ncdf4.helpers
#' @importFrom sf st_geometry_type st_transform st_as_sf st_drop_geometry
#' @importFrom assertthat assert_that
#'
#'
#' @param data_path Path to the gridded rainall data
#' @param sptial.obj An object of type \code{sf}. Either a point (or MULTIPOINT) or a polygon (or MULTIPOLYGON))
#' @param fun A function as character that the \code{exactext_extract}-function accepts
#' @param date A character that can be coerced directly to a date or an object of class \code{date}
#' @param nc_var The variable in the NetCDF you want to extract
#' @param days_back \code{integer vector} of days of antecedent rainfall you want to extract

#' @return a dataframe with either point extractions or columns of the aggregated values
#' @export

#' @export
ex_rainfall = function(data_path="\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/",
                        spatial.obj = NULL,
                        fun = NULL,
                        date = NULL,
                        nc_var = "precipitation",
                        days_back = 2){

  # get the geometry type of the sf object
  gtype = st_geometry_type(spatial.obj, by_geometry = FALSE) %>% as.character()
  if(!gtype=="POINT"){
    gtype="poly"
  }  else{
    gtype="point"
  }

  # if polygon and no function to aggregate the data throw error
  if(gtype == "poly" & is.null(fun)){
    warning("You can't use polygons and not provide a function to aggreate the data")
    print("Setting the aggreation automatically to: 'mean'")
  }

  # if the function is not of type character
  if (!is.null(fun)) {
    if (!is.character(fun)) {
      stop("The function-argument must be a character")
    }

  }


  ##----------------------
  ##  WORK WITH NETCDF  --
  ##----------------------

  # convert the date to an object of type date
  day = as.Date(date)

  # get one example raster
  y = format(day, "%Y")
  m = format(day, "%m")
  d = format(day, "%d")

  # example path
  ex_path = paste0(data_path, y, "/", "DAILYPCP_", y, formatC(m, flag = 0, width = 2), ".nc")

  # if not path exists --> there is no nc file for this path
  if(!file.exists(ex_path)){
    stop("No file foud. Maybe you used days that are not available...")
  }
  # example raster
  grd = suppressWarnings(raster(ex_path)[[1]])
  grd[] = NA

  # if not equal to crs of sf object stop/reproject here!
  crs_nc = suppressWarnings(grd %>% crs() %>% attributes() %>% .[["projargs"]])
  crs_shape = suppressWarnings(crs(spatial.obj) %>% crs() %>% attributes() %>% .[["projargs"]])

  # if not equal, reproject the shape...
  if(!crs_nc == crs_shape){
    warning(sprintf("The CRS of the spatial object and the NetCDF are not equal Reprojecting the shape to: %s", crs_nc))
    spatial.obj = st_transform(spatial.obj, crs_nc)
  }






# get the paths to the data -----------------------------------------------

  paths_to_data = get_nc_paths(data_path, day, days_back)


# create list of rasters --------------------------------------------------
  if (length(paths_to_data) == 1) {
    cat("\nGet the", days_back + 1, "rasters for ", as.character(day), "\n")

    # open the file
    ncin = ncdf4::nc_open(paths_to_data[[1]])

    # get a character string of the dates
    dates_nc = get_dates_ncdf(ncin, return_date_object=T)

    raster_list = get_raster_list_one_month(day, days_back, dates_nc, paths_to_data, surpressWarnings = T)

  }else{ # if mulitple paths (multiple months)
    cat("Read data from at least two month for", days_back + 1, "days", "\n\n")

    # get the rasters from different months
    raster_list = get_raster_list_n_month(paths_to_data, day, days_back)
  }



# extracting the raster values --------------------------------------------

  # if working with points
  if(is.null(fun)) {
    # print("Input are points, thus not using any function")
    extracted_days_list = lapply(raster_list, function(x) {
      d = names(x) %>% substr(., start = 2, stop = nchar(.)) %>% as.Date(., "%Y%m%d")
      # assign each point an id
      spatial.obj$id = 1:nrow(spatial.obj)
      print(paste0("Extracting data for: ", d))
      raster::extract(x, spatial.obj, sp = T) %>% st_as_sf()
    })
  } else{
    # if working with polygons

    # if not working with polygons but some function is defined
    if (!gtype == "poly") {
      stop("You have some function defined but want data for points. \n Set: 'fun=NULL'")
    }

    extracted_days_list = lapply(raster_list, function(x) {
      d = names(x) %>% substr(., start = 2, stop = nchar(.)) %>% as.Date(., "%Y%m%d")
      print(paste0("Extracting data for: ", d))
      # make id column to merge after extraction
      spatial.obj$id = 1:nrow(spatial.obj)
      cols = names(spatial.obj)
      ex = exactextractr::exact_extract(x, spatial.obj, fun, append_cols = c("id"))

      # renaming the mean column to the same format raster::extract returns it x...
      new_name = paste0("X", str_replace_all(d, "-", "")) %>% paste0(., "_", fun)

      # replace the columns in ex with the new names
      names_ex = names(ex)
      for (i in seq_along(names_ex)) {

        # the names are the aggregated functions
        n = names_ex[[i]]

        # the id column should not give any match
        if(any(grepl(n, new_name))){
          n = grep(n, new_name, value = T)
        }

        names(ex)[[i]] = n
      }

      # merge back the spatial info
      res = merge(ex, spatial.obj, by="id", ) %>% st_as_sf()
      return(res)
    })
  }


# build one dataframe -----------------------------------------------------

  # each objec in the list has data for different dates

  # get the geometry of the spatial object(s)
  # same for each of the list objects
  geom = extracted_days_list[[1]] %>% select(id)

  # the colums with the actual values
  cols_with_vals = lapply(extracted_days_list, function(x) {
    str_subset(names(x), "\\d{8}|id")
  })

  # the dataframe to append at the end
  # all the rows and columns are the same minus the ones from the aggregated values
  df_to_append = extracted_days_list[[1]] %>%
    select(-!!cols_with_vals[[1]])

  # for each date (list object) extract only these columns
  list_only_vals = vector("list", length = length(extracted_days_list))
  names(list_only_vals) = names(extracted_days_list)
  for (i in seq_along(list_only_vals)) {

    # for the repective day extract only the values (the only thing that differs)
    df = extracted_days_list[[i]] %>%
      select(!!cols_with_vals[[i]]) %>%
      st_drop_geometry()


    # put it in the only vals list
    list_only_vals[[i]] = df
  }

  # now bind the rows for each day
  one_df = dplyr::bind_cols(list_only_vals) %>%
    select(-contains("...")) %>%
    mutate(
      id = 1:nrow(.)
    )

  # bind everything together and append the columns that did not change
  df_all = dplyr::bind_cols(one_df, df_to_append) %>% select(-contains("geom"))


# calculate the evolution over time --------------------------------------
  res = make_cumsum(df_all, fun, days_back)
  res = merge(res, geom, by="id") %>% st_as_sf()

  return(res)

}
