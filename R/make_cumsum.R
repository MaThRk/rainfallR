#' Will calculate the cumulative sum of the measurements over time
#'
#' @description This function is called by the "parent"-function ex_rainfall in order to get the
#' Create the cumulative values. This function is not exported.
#'
#' @importFrom tidyr pivot_longer
#'
#' @export

 make_cumsum = function(df_wide, fun, days_back){


    #if using functions for polygons
    if(!is.null(fun)){

# divide into one df for each function ------------------------------------

   # only the columns with the values that dont change over time
   cols_all_same = names(df_wide) %>% str_detect(., fun, negate = T)
   cols_all_same = df_wide[, cols_all_same]

   # the values that change over time
   cols_vals = df_wide %>% select(contains(fun))


   # for each function now create one long df
   out_list = vector("list", length=length(fun))

   # for each aggreation function
   for (i in seq_along(fun)) {

      # get the colums (days) for only that function
      # and add the id. Should be valid to do that here
      df = cols_vals %>% select(contains(fun[[i]])) %>%
        mutate(id = 1:nrow(.))

      # reshape it
      df_long = df %>% pivot_longer(cols=-id, values_to = "precip", names_to = "measurement") %>%
         mutate(
            days_before_event = seq(days_back, 0)
         )

      # merge back the cols all same
      res = merge(df_long, cols_all_same, by="id")

      # seperate the the measurement column
      res = res %>%
        tidyr::extract(col=measurement, into=c("date", "fun"), regex="(\\d{8})_([[:alnum:]]+)") %>%
        mutate(
          date = as.Date(date, format = "%Y%m%d")
        )

      # calculate the cumulative sum
      # cumsum_colname = paste0("cumsum_", fun[[i]])
      # res = res %>%
      #   group_by(id) %>%
      #   mutate(!!cumsum_colname := cumsum(precip))

      # as we already have a fun-column we only need one cumusm-column for
      # all functions in order to define one row uniquely
      res = res %>%
         group_by(id) %>%
         mutate(cumsum = cumsum(precip))

      out_list[[i]] = res

   }


# stack the different functions -------------------------------------------

   cumsum_all_funs = dplyr::bind_rows(out_list)
   return(cumsum_all_funs)


    }else{

       # get all the columns with dates plus the id
       cols_with_vals = names(df_wide) %>% str_subset(., "\\d{8}|id")

       # subset the datafame
       cols_with_vals = df_wide %>% select(!!cols_with_vals)

       # get the static columns
       only_vals = grep("X\\d{8}", names(df_wide), value = T)
       static_df = df_wide %>% select(-!!only_vals)

       # make it long
       df_long_vals = pivot_longer(cols_with_vals, cols = -id, names_to = "date",
                                   values_to = "precip") %>%
          mutate(date = as.Date(str_extract(date, "\\d{8}"), format="%Y%m%d")) %>%
          group_by(id) %>%
          mutate(cumsum = cumsum(precip),
                 days_before_event = seq(days_back, 0))


       # merge back the rest of the values that didnt change over time
       res = merge(df_long_vals, static_df, by="id")

       return(res)
    }

}
