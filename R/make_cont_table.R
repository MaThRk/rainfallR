#' Create Contigency Values
#'
#' Provide a model, newdata and some other values and this function hopefully
#' creates a contigency matrix like dataframe
#'
#' @param model The Model
#' @param new_data A dataframe with the newdata to apply the model (must have the predictors from the model)
#' as columns. Otherwise will hopefully throw an error
#' @param cm If \code{TRUE} will output a "short" daatframe and only contain the count of the contigency values.
#' If \code{FALSE} will add to each obsevation the contigency value
#'
#' @param x_var The name of the column of the Predictor in our model
#' @param y_var The name of the column of the dependent variables (cumulated rainfall, intensity)
#' @param true_class Name of the column that hold the binary "true" values
#' @param trigger  The representation of the true (postive) value in the \code{true_class} column
#' @param notrigger  The representation of the false (negative) value in the \code{true_class} column
#' @param quantiles ...
#'
#' @importFrom dplyr case_when summarise
#' @importFrom tidyr pivot_longer
#'
#' @export

make_cont_table = function(model,
                     new_data,
                     cm = TRUE,
                     x_var = "duration",
                     y_var = "sum_rain",
                     true_class = "class",
                     trigger = "trigger",
                     notrigger = "notrigger",
                     quantiles = seq(0, 1, by = 0.01)) {
  # get the predictors from the model
  predictors = attr(terms(formula(model)), which = "term.labels")

  # if predictors are not in the new dataset, cant predict
  cols = names(new_data)
  for (p in predictors) {
    print(paste0("Predictor: ", p))
    if (!any(grepl(p, cols))) {
      stop(paste0("Predictor: ", p, " is not in the new dataset"))
    }
  }

  # predict the y_var for the new_data
  pred_y_var = paste0("pred_", y_var)
  new_data[[pred_y_var]] = predict(model, newdata = new_data)

  # check if the model predicted a landslide or not
  new_data[["Prediction"]] = ifelse(new_data[[pred_y_var]] < new_data[[y_var]], "trigger", "notrigger")
  new_data[["Reference"]] = ifelse(new_data[[true_class]] == trigger, "trigger", "notrigger")

  # Create the confusion Matrix
  new_data %>%
    mutate(
      tp = case_when(Reference == trigger &
                       Prediction  == "trigger" ~ TRUE,
                     TRUE ~ FALSE),
      fp = case_when(
        Reference == notrigger & Prediction == "trigger" ~ TRUE,
        TRUE ~ FALSE
      ),
      tn = case_when(
        Reference == notrigger & Prediction == "notrigger" ~ TRUE,
        TRUE ~ FALSE
      ),
      fn = case_when(
        Reference == trigger & Prediction == "notrigger" ~ TRUE,
        TRUE ~ FALSE
      )
    ) -> dat

  if (cm) {
    dat %>%
      pivot_longer(
        cols = matches("^[tf][pn]$"),
        names_to = "label",
        values_to = "count"
      ) %>%
      group_by(label) %>%
      summarise(n = sum(count)) %>%
      mutate(
        Reference = case_when(
          label == "tp" ~ 1,
          label == "tn" ~ 0,
          label == "fp" ~ 0,
          label == "fn" ~ 1
        ),
        Prediction = case_when(
          label == "tp" ~ 1,
          label == "tn" ~ 0,
          label == "fp" ~ 1,
          label == "fn" ~ 0
        )
      ) -> dat
  }

  return(dat)

}


