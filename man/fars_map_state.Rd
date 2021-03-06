% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Draw a map with FARS data for a given US state and year}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{integer: the number of the state from the FARS data}

\item{year}{an R object. Must be an integer or a character vector of length 1 or more, containing any year}
}
\description{
For a given year and US state this function plots a map with the distribution of accidents
regarding fatal injuries suffered in motor vehicle traffic crashes across the state using the
US Fatality Analysis Reporting System data.
}
\details{
The function will stop running of invalid state number was provided or
no event with given conditions were found
}
\examples{
fars_map_state(1, 2013)

}
