% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prophet_model_v01.R
\name{Saldae_prophet_multiple}
\alias{Saldae_prophet_multiple}
\title{Saldae Engine(main) using prophet package.}
\usage{
Saldae_prophet_multiple(
  tisefka_list = NULL,
  sald_holidays = NULL,
  asurif_zdat = 10
)
}
\arguments{
\item{tisefka_list}{training data (list of tibble objects (target variable corrected data date))}

\item{sald_holidays}{information regarding holdays(based on country)}

\item{asurif_zdat}{steps ahead (forecast horizon)}

\item{target_variables}{target variables}
}
\value{
forecast object.
}
\description{
Saldae Engine(main) using prophet package.
}
\author{
Farid Azouaou
}
