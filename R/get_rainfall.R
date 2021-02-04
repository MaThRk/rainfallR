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
#' @param fun A function to aggregate the data in case a polygon is passed as \code{spatial.obj}
#' @param dts Either a single object of type \code{Date} or a vector of at least two dates
#' @param seqq \code{logical} to create a sequence of days between two dates passed in \code{dts}
#' @param days_back \code{integer vector} of days of antecedent raindall you want to extract

# data_path="\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS/"
# path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp"
# path2 = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"
# spatial.obj = st_read(path)
# dts = c(as.Date("2016-01-12"), as.Date("2016-01-16"))

#' @export
get_rainfall = function(data_path="\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/",
                        spatial.obj = NULL,
                        fun = NULL,
                        dts = NULL,
                        nc_var = "precipitation",
                        seqq = TRUE, # do you want all dates in this sequence or only specific dates?
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

  # for creating a sequence between two dates
  if(length(dts) == 1){
    seqq = FALSE
  }
  if (seqq) {
    dts = seq(dts[[1]], dts[[2]], by = "day")
  }

  # output will be a list of datafames
  out = vector("list", length = length(dts))

  # name the output
  for (i in seq_along(out)) {
    n = as.character(dts[[i]]) %>% str_replace_all(., "-", "")
    names(out)[[i]] = n
  }


  ##----------------------
  ##  WORK WITH NETCDF  --
  ##----------------------
  # this assumes that the structure of the rainfall data will stay the same
  # put the dates into a list as iterating over a vector loses class information..
  dts = as.list(dts)

  # get one example raster
  y = format(dts[[1]], "%Y")
  m = format(dts[[1]], "%m")
  d = format(dts[[1]], "%d")

  # example path
  ex_path = paste0(data_path, y, "/", "DAILYPCP_", y, formatC(m, flag = 0, width = 2), ".nc")
  # if not path exists --> there is no nc file for this path
  if(!file.exists(ex_path)){
    stop("No file foud. Maybe you used days that are not available...")
  }
  # example raster
  grd = raster(ex_path)[[1]]
  grd[] = NA

  # if not equal to crs of sf object stop/reproject here!
  crs_nc = suppressWarnings(grd %>% crs() %>% attributes() %>% .[["projargs"]])
  crs_shape = suppressWarnings(crs(spatial.obj) %>% crs() %>% attributes() %>% .[["projargs"]])

  # if not equal, reproject the shape...
  if(!crs_nc == crs_shape){
    warning(sprintf("The CRS of the spatial object and the NetCDF are not equal Reprojecting the shape to: %s", crs_nc))
    spatial.obj = st_transform(spatial.obj, crs_nc)
  }


  # for each day create a dataframe
  counter = 1
  for (day in dts) {

    # # print a super informative message
    # n = length(dts)
    # str = paste0(i, "/", n)
    # dashes = paste0(replicate(40, "-"), collapse = "")
    # cat(paste(str, dashes), "\n")

    # get the year the month and the day
    y = format(day, "%Y")
    m = format(day, "%m")
    d = format(day, "%d")

    # create the path to the data for one month
    paths_to_data = get_nc_paths(data_path, day, days_back)
    # message("Accessing these NetCDF-files: ", paths_to_data)

    # if all the data we want comes from the same month
    if(length(paths_to_data) == 1){
      cat("\nGet the", days_back + 1, "rasters for ", as.character(day), "\n")

      # open the file
      ncin = ncdf4::nc_open(paths_to_data[[1]])

      # get a character string of the dates
      dates_nc = get_dates_ncdf(ncin, return_date_object=T)

      raster_list = get_raster_list_one_month(day, days_back, dates_nc, paths_to_data)

    } else{
      cat("Read data from at least two month for", days_back + 1, "days", "\n\n")

      # we need a loop in order to extract them
      # if the back days reach into the last month..
      # for each month
      # opening the file and getting the dates is done for ach NetCDF connection within the funtion
      raster_list = get_raster_list_n_month(paths_to_data, day, days_back)
    }

    # ectract the spatial data
    if(is.null(fun)) {
      # print("Input are points, thus not using any function")
      extracted_days_list = lapply(raster_list, function(x) {
        d = names(x) %>% substr(., start=2, stop=nchar(.)) %>% as.Date(., "%Y%m%d")
        print(paste0("Extracting data for: ", d))
        raster::extract(x, spatial.obj, sp = T) %>% st_as_sf()
      })
    } else{

      # if fun is not null but data are points --> does not work...
      if(!gtype == "poly"){
        stop("You have some function defined but want data for points. \n Set: 'fun=NULL'")
      }

      # message("using polygons and the function: ", fun@generic[[1]])
      extracted_days_list = lapply(raster_list, function(x){
        d = names(x) %>% substr(., start=2, stop=nchar(.)) %>% as.Date(., "%Y%m%d")
        print(paste0("Extracting data for: ", d))
        # make id column to merge after extraction
        spatial.obj$id = 1:nrow(spatial.obj)
        cols = names(spatial.obj)
        ex = exactextractr::exact_extract(x, spatial.obj, "mean", append_cols = cols)
        # merge back the spatial info
        res = merge(ex, spatial.obj, by="id") %>% st_as_sf()
        return(res)
      })
    }

    # make in one dataframe where each column is one date
    b = dplyr::bind_cols(extracted_days_list)
    geom = b %>% dplyr::select(matches("geom")) %>% pull(1)

    # if we have a spatial object with iffi-kodex
    if("PIFF_ID" %in% names(spatial.obj)){
      iffi_kodex = b %>% dplyr::select(matches("PIFF_ID")) %>% pull(1)
      b = b %>% dplyr::select(matches("^x")) %>%
        dplyr::rename_with(., ~stringr::str_replace_all(., pattern = "X", ""))
      b[["geometry"]] = geom
      b[["iffi"]] = iffi_kodex
      b = st_as_sf(b)

    }else{
      b = b %>% dplyr::select(matches("^x")) %>%
        dplyr::rename_with(., ~stringr::str_replace_all(., pattern = "X", ""))
      b[["geometry"]] = geom
      b = st_as_sf(b)
    }


    # assign it to out
    out[[counter]] = b
    counter = counter + 1

  }

  return(out)

}
