% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alarm.R
\name{get_alarm}
\alias{get_alarm}
\title{Get ALARM Dataset}
\usage{
get_alarm(state, year = 2020, geometry = TRUE, epsg = 3857)
}
\arguments{
\item{state}{two letter state abbreviation}

\item{year}{year to get data for. Either \code{2020} or \code{2010}}

\item{geometry}{Default is TRUE. Add geometry to the data?}

\item{epsg}{numeric EPSG code to planarize to. Default is 3857.}
}
\value{
tibble with election data and optional geometry
}
\description{
Gets a dataset from the Algorithm-Assisted Redistricting Methodology Project.
The current supported data is the 2020 retabulations of the VEST data, which
can be downloaded with \code{get_vest}.
}
\details{
See the full available data at \url{https://github.com/alarm-redist/census-2020}.
}
\examples{
ak <- get_alarm('AK', geometry = FALSE)
}
\concept{datasets}
