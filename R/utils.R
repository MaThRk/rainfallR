#' Some helper functions
#'
#' @import stringr
#' @importFrom chron chron
#' @impo ncdf4



#' get the dates from an nc object
#'
#' @param ncin An object of class \code{ncdf4}
#' @param return_date_object \code{logical} If true returns a vector of
#' \code{date}-object, else vector of characterstring
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


get_nc_paths = function(data_path, day, days_back){

  # day we want to extract
  y = format(day, "%Y")
  m = format(day, "%m")
  d = format(day, "%d")
  path_day = paste0(data_path, y, "/", "DAILYPCP_", y, formatC(m, flag = 0, width = 2), ".nc")

  # last day to extract
  days_back = max(days_back)
  max_day_back = min(day - days_back)
  y_b = format(max_day_back, "%Y")
  m_b = format(max_day_back, "%m")
  d_b = format(max_day_back, "%d")
  last_month_path = paste0(data_path, y_b, "/", "DAILYPCP_", y_b, formatC(m_b, flag = 0, width = 2), ".nc")

  # if the current day has a different month than the last day back we need two paths
  if(!m == m_b){
    warning("Your first day of extraction is in another month. Pray and hope this function works")
    return(c(path_day, last_month_path))
  }else{
    return(path_day)
  }
}
