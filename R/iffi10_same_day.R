#' What slides happened on the same day?
#'
#' @description  This function will group all of the points that happened on the
#' same day into a \code{data.frame} and put this dataframe into a list. Each
#' element of the list thus can be considered to be a day. This requires that the input has a column called date
#'
#'
#' @return A list where each element is a dataframe with all the slides from
#' that day
#'
#' @param iffi10 An object of type \code{sf} which has a column called date!!
#'
#' @export

iffi10_same_day = function(iffi10){

  # make sure there are only records with dates
  iffi10 = iffi10 %>% filter(date_info == "day")

  if(! any(grepl("date", names(iffi10)))){
    stop("There is no column called `date` in your object")
  }

  # the slide dates for all slides
  dates_of_slides = iffi10$date

  # turn them into a character
  dates_of_slides = as.character(dates_of_slides) %>% str_replace_all(., "-", "")

  # create output list with unique slide_dates
  unique_dates = unique(dates_of_slides)

  # create the output list of the length of the unique dates
  slides_same_day = vector("list", length=length(unique_dates))
  names(slides_same_day) = unique_dates

  # for each element check the date and create the dataframes per day
  for (i in 1:nrow(iffi10)) {

    # get the date of the slide under consideration
    date_of_slide = iffi10[i, ]$date %>% str_replace_all(., "-", "")

    # get the slide itself
    slide = iffi10[i,]

    # put it into the list
    slides_same_day[[date_of_slide]] = rbind(slides_same_day[[date_of_slide]], slide)

  }

  # return the list with the slides from the same day
  return(slides_same_day)
}
