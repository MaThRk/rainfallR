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
