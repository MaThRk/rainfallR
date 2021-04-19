#' Calulate the aggregated Rasters
#'
#'
#' This function will return a list of length n_years. With Either 12 (by = \code{month}) or 1 (by = \code{year}) elements per element
#'
#'

make_rasters = function(paths, years, by, daily_fun, monthly_fun, return.list = FALSE){


  if(by == "year"){

    yearly_values = vector("list", length=length(years))

    # for each year
    for (y in seq_along(paths)) {

      ye = names(paths)[[y]]
      cat(rep("-", 20), ye)

      monthly_matrices = vector("list", length=12)

      # for each month
      for (m in seq_along(paths[[y]])) {

        cat("\r Reading: ", m, names(paths)[[y]])

          # get the monthly_raster
          stars_one_month = read_stars(paths[[y]][[m]])

          # sum up all the pixel-values in that month
          fun_one_month = st_apply(stars_one_month, c("x", "y"), daily_fun)
          # get only the values
          fun_one_month_matrix = fun_one_month[[1]]

          monthly_matrices[[m]] = fun_one_month


      }

      # apply the monthly function
      if(monthly_fun == "sum"){
        yearly_fun = Reduce("+", monthly_matrices)
      }else if(monthly_fun == "mean"){
        yearly_fun = Reduce("+", monthly_matrices) / length(monthly_matrices)
      }else if (monthly_fun == "max"){
       yearly_fun = pmax(monthly_matrices)
      }

      # put the yearly stars object into the list
      yearly_values[[y]] = yearly_fun

    }


    # cast the list to one stars object
    if(!return.list){
      combined = c(do.call("c", yearly_values))
      merged = merge(combined, years = seq_along(years))
      names = paste0(daily_fun, "_", monthly_fun, "_", years)
      yearly_values = st_set_dimensions(merged, which="years", values=names)

    }
    return(yearly_values)


  }else if(by == "month"){

    yearly_values = vector("list", length=length(years))

    # for each year
    for (y in seq_along(paths)) {

      ye = names(paths)[[y]]
      cat(rep("-", 20), ye)

      monthly_stars = vector("list", length=12)

      # for each month
      for (m in seq_along(paths[[y]])) {

        cat("\r Reading: ", m, names(paths)[[y]])

          # get the monthly_raster
          stars_one_month = read_stars(paths[[y]][[m]])

          # sum up all the pixel-values in that month
          fun_one_month = st_apply(stars_one_month, c("x", "y"), daily_fun)

          # put the monthly stars object in the list
          monthly_stars[[m]] = fun_one_month

      }

      # combine the monthly stars into one stars object
      if (!return.list) {
        combined_stars = c(do.call("c", monthly_stars))
        merged = merge(combined_stars, months = 1:12)
        months = seq(as.Date(paste0(names(paths)[[y]], "-01-01")), as.Date(paste0(names(paths)[[y]], "-12-1")), by =
                       "month")
        names = paste0(daily_fun, " ", months)
        combined = st_set_dimensions(merged, which = "months", values = names)
      }else{
        combined = monthly_stars
      }


      # put them in the list
      yearly_values[[y]] = combined

    }

    return(yearly_values)

  }

}
