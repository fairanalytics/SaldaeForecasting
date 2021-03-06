% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prophet_model_v01.R
\name{prophet_forecast_f}
\alias{prophet_forecast_f}
\title{Saldae Engine using prophet package.}
\usage{
prophet_forecast_f(prophet_object = NULL, asurif_zdat = 10, ukud_unit = NULL)
}
\arguments{
\item{prophet_object}{: fitted object from Prophet}

\item{asurif_zdat}{: steps ahead (forecast horizon)}

\item{ukud_unit}{: time units}
}
\value{
forecast object (fitted value, forecast ,actuals , upper and lower bounds)
}
\description{
Saldae Engine using prophet package.
}
\author{
Farid Azouaou
}
