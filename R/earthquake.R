

#' @title Function to Clean the Name of the Location of Earthquake.
#'
#' @description Function that cleans the LOCATION_NAME column of the data set by stripping out the
#' country name (including the colon) and converts names to title case (as opposed to all caps).
#' The country names are stored in a different column.The cleaned location will be used for annotation
#' purposes of maps that showcase the locations of earthquakes in the world based on the data.
#'
#' @param raw_data Data file on earthquakes from U.S. National Oceanographic and Atmospheric Administration (NOAA).
#'
#' @importFrom stringr str_to_title
#' @importFrom stringi stri_trim
#'
#'
#' @return An object of class data.frame or tbl_df or tibble.
#' @export
#'
#' @examples
#' eq_location_clean(eaq_data)
#'
eq_location_clean <- function(raw_data) {
  for (i in 1:nrow(raw_data)) {
    x <- unlist(strsplit(as.character(raw_data[i, "Location Name"]), ":"))
    raw_data[i, "Location Name"] <- stringr::str_to_title(stringi::stri_trim(x[length(x)]))
    raw_data[i, "Country"] <- x[1]
  }
  return(raw_data)
}

#' @title Function to Create Date of Occurrence of Earthquake.
#'
#' @description The goal of the function is to The clean data frame so that it has the following:
#' (i) A date column created by uniting the year, month, day and converting it to the Date class.
#' (ii) LATITUDE and LONGITUDE columns converted to numeric class.
#'
#' @param raw_data Data file on earthquakes from U.S. National Oceanographic and Atmospheric Administration (NOAA).
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#'
#' @return An object of class data.frame or tbl_df or tibble.
#' @export
#'
#' @examples
#' eq_clean_data(eaq_data)
#'
eq_clean_data <- function(raw_data) {
  data <- raw_data %>%
    dplyr::mutate(Date = lubridate::ymd(paste(Year, Mo, Dy, sep = "-"))) %>%
    dplyr::mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
    eq_location_clean()
  return(data)
}



