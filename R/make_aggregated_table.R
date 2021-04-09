#' Calulate the the aggregated values for each polygon
#'
#'
#' This function will return a list of length n_years. With Either 12 (by = \code{month}) or 1 (by = \code{year}) elements per element
#'
#' @param by either "year" or "month". If "year", the raster with the values for one year will be calculated and than the spatial
#' aggregation (with the function \code{aggre_fun}) will be applied. If "month", the spatial aggregation will be applied after each month
#'
#' @importFrom exactextract exact_extract
#'

make_aggregated_table = function(paths, polygon, years, by, daily_fun, monthly_fun, return.list = FALSE){


  # give each polygon an internal id
  rows = nrow(polygon)
  ids = paste0("x",1:rows)
  polygon[["xid"]] = ids


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

      # cast the yearly-fun stars object to an object of class raster
      yearly_raster = as(yearly_fun, "Raster")

      # extract the values for each polygon
      per_poly_year = exact_extract(yearly_raster, polygon, fun=aggre_fun, force_df=T, append_cols=c("xid"))
      names(per_poly_year) = c("xid", paste0(daily_fun, "_", monthly_fun, "_", years[[y]]))

      polygon = merge(polygon, per_poly_year, by="xid")

      # put the yearly stars object into the list
      yearly_values[[y]] = polygon

    }


    return(yearly_values)


  }else if(by == "month"){

    yearly_values = vector("list", length=length(years))
    names(yearly_values) = years

    # for each year
    for (y in seq_along(paths)) {

      ye = names(paths)[[y]]
      cat(rep("-", 20), ye)

      yearly_df = data.frame(xid = polygon[["xid"]])

      # for each month
      for (m in seq_along(paths[[y]])) {

        cat("\r Reading: ", m, names(paths)[[y]])

          # get the monthly_raster
          stars_one_month = read_stars(paths[[y]][[m]])

          # sum up all the pixel-values in that month
          fun_one_month = st_apply(stars_one_month, c("x", "y"), daily_fun)

          # coerce it to a raster
          raster_one_month = as(fun_one_month, "Raster")

          # apply the aggregation here
          monthly_df = exact_extract(raster_one_month, polygon, fun=aggre_fun, force_df=T, append_cols=c("xid"))
          names(monthly_df)[[2]] = paste0(daily_fun, "_", monthly_fun, "_", month.abb[m], "_", names(paths)[[y]])

          yearly_df = merge(yearly_df, monthly_df, by="xid")

      }

      # merge back the geometry
      p = merge(polygon, yearly_df, by="xid")

      # put them in the list
      yearly_values[[y]] = p

    }


    return(yearly_values)

  }

}
