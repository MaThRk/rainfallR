#' Extract rainfall data
#'
#'
#' @import dplyr
#' @import ncdf4
#' @import ncdf4.helpers
#' @import sf
#' @import raster
#' @importFrom assertthat assert_that
#'
#'
#'
#' @param data_path Path to the gridded rainall data
#' @param sptial.obj An object of type \code{sf}. Either a point (or MULTIPOINT) or a polygon (or MULTIPOLYGON))
#' @param fun A function to aggregate the data in case a polygon is passed as \code{spatial.obj}
#' @param dts Either a single object of type \code{Date} or a vector of at least two dates
#' @param seqq \code{logical} to create a sequence of days between two dates passed in \code{dts}
#' @param days_back \code{integer vector} of days of antecedent raindall you want to extract
#'
#' @return either a dataframe (for one point and date) or a list of (lists) for multiple polygons and dates
#' @examples
#' \dontrun{
#' path = "...."
#' shape = st_read(path)
#' path_nc = "...
#' }

#' @export
get_rainfall = function(data_path="\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS/",
                        spatial.obj = NULL,
                        fun = NULL,
                        dts = NULL,
                        nc_var = "precipitation",
                        seqq = TRUE, # do you want all dates in this sequence or only specific dates?
                        days_back = 1:1){

  # check the inputs are provided at all
  if(is.null(data_path)){
    stop("You need to provide a path to the netcdf data")
  }

  if(is.null(spatial.obj)){
    stop("You need to provide a spatial object from where to extract the data")
  }


  # verify its an object of type sf
  assert_that(class(spatial.obj)[[1]] == "sf", msg="The spatial data is not of class sf")
  # assert that dates are actually dates
  assert_that(assertthat::is.date(dts), msg = "dts must be an object of type date")
  # integer range of dates
  assert_that(is.integer(days_back), msg = "The date range must be of type integer")


  # get the geometry type of the sf object
  gtype = st_geometry_type(spatial.obj, by_geometry = FALSE) %>% as.character()
  if(!gtype=="POINT"){
    gtype="poly"
  }  else{
    gtype="point"
  }

  # what kind of dates
  # if we have more than one date than we will return a list of dataframes
  if(length(dts) > 1) {
    if (seqq) {
      dts = seq(dts[[1]], dts[[2]], by = "day")
      n_days = length(dts)
      message(paste("Extracting data for a sequential range of", n_days,  "dates"))
      message(paste("with", max(days_back), "day(s) in antecedence"))

      # output will be a list of datafames
      out = vector("list", length = length(dts))

    } else{
      # only specifc dates
      message(paste("Extracting data for", length(dts), "specific dates"))
      message(paste("with", max(days_back), "day(s) in antecedence"))
      dts = dts

      # output will be a list of datafames
      out = vector("list", length = length(dts))

    }
  } else{
    # only one specific date
    message(paste("Extracting data for one date:", dts, "\n",
                  "with", max(days_back), "day(s) in antecedence" ))
    dts = dts

    # output will be only one dataframe
    out = vector("list", length = length(dts))

  }

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

  # for each day create a dataframe
  for (day in dts) {

    # # where to put the output data
    # # length of the list is the size of the spatial object
    # out_data = vector("list", length=nrow(spatial.obj))

    # get the year the month and the day
    y = format(day, "%Y")
    m = format(day, "%m")
    d = format(day, "%d")

    # create the path to the data for one month
    path_year_month = paste0(data_path, y, "/", "DAILYPCP_", y, formatC(m, flag = 0, width = 2), ".nc")

    # # maybe better to do it this way
    # all_files = list.files(paste0(data_path, y))
    # file_nc_path = all_files[str_detect(all_files, paste0(y,m))]
    # file_nc_path = paste0(data_path, y, "/", file_nc_path)
    # nc_open(file_nc_path)

    # open the file
    ncin = ncdf4::nc_open(path_year_month)

    # get a character string of the dates
    dates_nc = get_dates_ncdf(ncin, return_date_object=T)

    # read the precipitation values
    precip_nc = ncvar_get(ncin, nc_var)

    # read the projection info --> should be done outside the loop as it is always the same
    proj = nc.get.proj4.string(ncin, nc_var)

    # read an exmaple raster and create the empty grid --> do outside the loop
    grd = raster(path_year_month)[[1]]
    grd[] = NA

    # if not equal to crs of sf object stop/reproject here!
    crs_nc = suppressWarnings(grd %>% crs() %>% attributes() %>% .[["projargs"]])
    crs_shape = suppressWarnings(crs(shape) %>% crs() %>% attributes() %>% .[["projargs"]])

    # if not equal, reproject the shape...
    if(!crs_nc == crs_shape){
      warning(sprintf("The CRS of the spatial object and the NetCDF are not equal Reprojecting the shape to: %s", crs_nc))
      shape = st_transform(shape, crs_nc)
    }

    # extract all the values
    rasters = get_rasters(precip_nc, proj, day, days_back)

  }

  return(out)

}
