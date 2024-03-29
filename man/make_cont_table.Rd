% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_cont_table.R
\name{make_cont_table}
\alias{make_cont_table}
\title{Create Contigency Values}
\usage{
make_cont_table(
  model,
  new_data,
  cm = TRUE,
  x_var = "duration",
  y_var = "sum_rain",
  true_class = "class",
  trigger = "trigger",
  notrigger = "notrigger",
  quantiles = seq(0, 1, by = 0.01)
)
}
\arguments{
\item{model}{The Model}

\item{new_data}{A dataframe with the newdata to apply the model (must have the predictors from the model)
as columns. Otherwise will hopefully throw an error}

\item{cm}{If \code{TRUE} will output a "short" daatframe and only contain the count of the contigency values.
If \code{FALSE} will add to each obsevation the contigency value}

\item{x_var}{The name of the column of the Predictor in our model}

\item{y_var}{The name of the column of the dependent variables (cumulated rainfall, intensity)}

\item{true_class}{Name of the column that hold the binary "true" values}

\item{trigger}{The representation of the true (postive) value in the \code{true_class} column}

\item{notrigger}{The representation of the false (negative) value in the \code{true_class} column}

\item{quantiles}{...}
}
\description{
Provide a model, newdata and some other values and this function hopefully
creates a contigency matrix like dataframe
}
