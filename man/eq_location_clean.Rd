% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquake.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Function to Clean the Name of the Location of Earthquake.}
\usage{
eq_location_clean(raw_data)
}
\arguments{
\item{raw_data}{Data file on earthquakes from U.S. National Oceanographic and Atmospheric Administration (NOAA).}
}
\value{
An object of class data.frame or tbl_df or tibble.
}
\description{
Function that cleans the LOCATION_NAME column of the data set by stripping out the
country name (including the colon) and converts names to title case (as opposed to all caps).
The country names are stored in a different column.The cleaned location will be used for annotation
purposes of maps that showcase the locations of earthquakes in the world based on the data.
}
\examples{
eq_location_clean(eaq_data)

}
