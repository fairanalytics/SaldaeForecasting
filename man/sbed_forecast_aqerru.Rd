% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcast_summary_module.R
\name{sbed_forecast_aqerru}
\alias{sbed_forecast_aqerru}
\title{Saldae convert fcast  object into data frame}
\usage{
sbed_forecast_aqerru(
  fcast_object = NULL,
  asurif_arzdat = NULL,
  TimeZone = "CET"
)
}
\arguments{
\item{fcast_object}{forecast object}

\item{asurif_arzdat}{forecast horizon}
}
\value{
tibble object containing actuals , corrected, forecast and prediction bands(upper and lower)
}
\description{
t.b.d
}
\author{
Farid Azouaou
}
