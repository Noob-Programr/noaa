% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{eaq_data}
\alias{eaq_data}
\title{Data on Significant and Destructive Earthquakes around the world since 2150 B.C.}
\format{
A data frame with 6228 rows and 38 variables:
\describe{
  \item{Year}{Year of occurrence of the earthquake}
  \item{Mo}{Month of occurrence of the earthquake}
  \item{Dy}{Day of occurrence of the earthquake}
  \item{Hr}{Hour of occurrence of the earthquake}
  \item{Mn}{Minute of occurrence of the earthquake}
  \item{Sec}{Second of occurrence of the earthquake}
  \item{Tsu}{Tsunami associated or not}
  \item{Vol}{Volcanic Erruption associated or not}
  \item{Location Name}{Location of earthquake}
  \item{Latitude}{Latitude coordinate of the earthquake}
  \item{Longitude}{Longitude coordinate of the earthquake}
  \item{Focal Depth (km)}{Depth of the earthquake hypocenter in kilometers}
  \item{Mag}{MAgnitude of the Earthquake}
  \item{MMI Int}{Intensity of the earthquake in Modified Mercalli Intensity}
  \item{Deaths}{Number of deaths in the earthquake}
  \item{Death Description}{Desciption of deaths in the earthquake}
  \item{Missing}{Number of persons reported missing after the earthquake}
  \item{Missing Description}{Description of missing persons}
  \item{Injuries}{Number of persons reported injured after the earthquake}
  \item{Injuries Description}{Description of injured persons}
  \item{Damage ($Mil)}{Damage in million dollars from the earthquake}
  \item{Damage Description}{Description of damage from the earthquake}
  \item{Houses Destroyed}{Number of houses reportedly destroyed after the earthquake}
  \item{Houses Destroyed Description}{Description of houses destroyed by the earthquake}
  \item{Houses Damaged}{Number of houses reportedly damaged after the earthquake}
  \item{Houses Damaged Description}{Description of houses damaged by the earthquake}
  \item{Total Deaths}{Total number of Deaths from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Death Description}{Decription of the total number of deaths from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Missing}{Total number of missing persons from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Missing Description}{Decription of the total number of missing persons from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Injuries}{Total number of injured persons from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Injuries Description}{Decription of the total number of injured persons from earthquakes and secondary effects like volcanic erruption and tsunami}
  \item{Total Damage ($Mil)}{Damage in million dollars from the earthquake and secondary effects like volcanic erruption and tsunami}
  \item{Total Damage Description}{Description of damage from the earthquake and secondary effects like volcanic erruption and tsunami}
  \item{Total Houses Destroyed}{Number of houses reportedly destroyed after the earthquake and secondary effects like volcanic erruption and tsunami}
  \item{Total Houses Destroyed Description}{Description of houses destroyed by the earthquake and secondary effects like volcanic erruption and tsunami}
  \item{Total Houses Damaged}{Number of houses reportedly damaged after the earthquake and secondary effects like volcanic erruption and tsunami}
  \item{Total Houses Damaged Description}{Description of houses damaged by the earthquake and secondary effects like volcanic erruption and tsunami}
}
}
\source{
\url{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data}
}
\usage{
eaq_data
}
\description{
A dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA) on
significant earthquakes around the world.The Significant Earthquake Database contains information
on destructive earthquakes from 2150 B.C. to the present that meet at least one of the following criteria:
Moderate damage (approximately $1 million or more), 10 or more deaths, Magnitude 7.5 or greater, Modified
Mercalli Intensity X or greater, or the earthquake generated a tsunami.
}
\keyword{datasets}
