#' Some helper functions
#'
#' @importFrom  stringr str_replace_all
#' @importFrom chron chron
#' @importFrom  ncdf4 ncvar_get ncatt_get
#'
#' @param ncin An object of class \code{ncdf4}
#' @param return_date_object \code{logical} If true returns a vector of
#' \code{date}-object, else vector of characterstring
#' @export
get_dates_ncdf = function(ncin, return_date_object=TRUE){

  # get the DATE vars
  dates = ncdf4::ncvar_get(ncin, "DATE")
  dunits = ncdf4::ncatt_get(ncin, "DATE", "units")

  # convert the time variable
  tustr = strsplit(dunits$value, " ")
  tdstr = strsplit(unlist(tustr)[[3]], "-")
  tmonth = as.integer(unlist(tdstr)[[2]])
  tday = as.integer(unlist(tdstr)[[3]])
  tyear = as.integer(unlist(tdstr)[[1]])
  all_dates = chron::chron(dates, origin = c(tmonth, tday, tyear))

  # dates as strings
  all_dates_chr = all_dates %>% str_replace_all(., "\\(", "") %>% substr(., 1, 8) %>% str_replace_all(., "\\/", ".")
  # dates as posix datatype
  all_dates_pos = all_dates %>% str_replace_all(., "\\(", "") %>% substr(., 1, 8) %>% as.Date(., "%m/%d/%y")

  if(return_date_object){
    return(all_dates_pos)
  }else{
    return(all_dates_chr)
  }

}

#' get the paths to the nc file
#' @export
get_nc_paths = function(data_path, day, days_back){

  # day we want to extract
  y = format(day, "%Y")
  m = format(day, "%m")
  d = format(day, "%d")

  # precip or temp?
  var = ifelse(grepl("TEMP", data_path), "DTMEAN_", "DAILYPCP_")

  # build the path
  path_day = paste0(data_path, y, "/", var, y, formatC(m, flag = 0, width = 2), ".nc")

  # last day to extract
  days_back = max(days_back)
  max_day_back = min(day - days_back)
  y_b = format(max_day_back, "%Y")
  m_b = format(max_day_back, "%m")
  d_b = format(max_day_back, "%d")
  last_month_path = paste0(data_path, y_b, "/", var, y_b, formatC(m_b, flag = 0, width = 2), ".nc")

  # if the current day has a different month than the last day back we need two paths
  if(!m == m_b){
    warning("Your first day of extraction is in another month. Pray and hope this function works")
    paths = list()
    this_month = paste0(y,m)
    last_month  = paste0(y_b, m_b)
    paths[[this_month]] = path_day
    paths[[last_month]] = last_month_path
    return(paths)
  }else{
    paths = list()
    this_month = paste0(y,m)
    paths[[this_month]] = path_day
    return(paths)
  }
}



#' verify the input of the polyogn

check_spatial_input = function(polygon) {
  #if polygon is path to file
  if (class(polygon)[[1]] == "character") {
    if(!file.exists(polygon)) {
      stop("The file you provided does not exist")
    } else{
      poly =  read_sf(polygon)
      return(poly)
    }

  } else{
    if (class(polygon)[[1]] == "sf") {
      return(polygon)
    }
  }

}



#' Get the paths to the monthly NetCDFS
#'

get_monthly_paths = function(years, path_ncdf){

  # how many years
  n_years = length(years)

  # create the monthly data
  all_years_month = vector("list", length=n_years)
  names(all_years_month) = years %>% as.character()

  # for each year
  for (i in seq_along(1:n_years)) {

    # create a vector of the months
    months = vector(length = 12)

    # for each month
    for (j in seq_along(1:12)) {
      base = 1979
      current_year = base + i
      year_month = as.Date(paste0(current_year, "-", j, "-01" ))
      months[[j]] = year_month
    }

    months = as.Date.numeric(months, origin = "1970-01-01")
    all_years_month[[i]] = months
  }


  paths = vector("list", length = n_years)
  names(paths) = years %>% as.character()

  # for all the years
  for (i in seq_along(1:n_years)) {

    # all the paths for one year
    months = vector(length = 12)

    # get all the months
    for (j in seq_along(1:length(all_years_month[[i]]))) {

      # get one month
      d = all_years_month[[i]][[j]]

      # get the path
      path = rainfallR::get_nc_paths(path_ncdf, d, 0) %>% .[[1]]

      # add the path to that year
      months[[j]] = path
    }

    # add this year to all years outer list
    paths[[i]] = months

  }

  return(paths)

}

