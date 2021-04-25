#' @title Function to Display a World Map that Features Earthquake Locations.
#'
#' @description Function takes in the cleaned data and column to annotate on the map (if any) and returns
#' an interactive world map with points plotted on it which denote the location of earthquakes. In case
#' `annot_col` is given a particular non null argument, the map features pop-ups on every pointthat contain
#' information about that earthquake in elation to the argument passed through `annot_col`.
#'
#' @param dat An object of class data.frame or tibble or tbl_df that contains data on earthquakes.
#' @param annot_col The name of the feature of the earthquakes to be annoted. For instance, Date, Deaths,
#' MAgnitude, etc. The default is Date.
#'
#' @note The `annot_col` argument can only take valid column names from the Data provided as input.
#' In any other case, the feature should be first concatenated withe the data frame and then then the data
#' and the new feature should be passed to the function. Check documentation of the dataset used in this
#' package to get an idea of the features already present in the data set. For further queries refer to the
#' source https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data
#'
#' @importFrom dplyr select
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @return A html widget called leaflet that features an interactive world map with earthquake locations.
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' data(eaq_data)
#' eaq_data%>%
#'   eq_clean_data()%>%
#'   dplyr::filter(`Country` == "MEXICO" & lubridate::year(`Date`) >= 2000) %>%
#'   eq_map(annot_col = "Date")
#' }
eq_map <- function(dat, annot_col = "Date") {
  annot_col <- dat %>%
    dplyr::select(as.name(annot_col))
  dat["annot_col"] <- annot_col
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = dat, radius = ~Mag, lng = ~Longitude, lat = ~Latitude, popup = ~annot_col)
}

#' @title Function to Create Labels for Annotation.
#'
#' @description A function to create labels that comprises total deaths from a earthquake, its location and
#' magnitude. These labels appear on the pop ups that appear on the map of earthquakes created by eq_map function.
#'
#' @param data An object of class data.frame or tibble or tbl_df that contains data on earthquakes.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#'
#' @return An object of class tibble that comprises the labels.
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' data(eaq_data)
#' eaq_data%>%
#'   eq_clean_data()%>%
#'   dplyr::filter(`Country` == "MEXICO" & lubridate::year(Date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
eq_create_label <- function(data) {

  data <- data %>%
    dplyr::mutate(popup_text = dplyr::if_else(!is.na(`Location Name`),
                                              paste("<b>Location: </b>", `Location Name`, "</br>"),
                                              NA_character_),
                  popup_text = dplyr::if_else(!is.na(`Mag`),
                                              paste(popup_text, "<b>Magnitude: </b>", `Mag`, "</br>"),
                                              popup_text),
                  popup_text = dplyr::if_else(!is.na(`Total Deaths`),
                                              paste(popup_text, "<b>Total Deaths: </b>", `Total Deaths`),
                                              popup_text))

  return(data$popup_text)
}
