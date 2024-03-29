#' @name carcassDistance
#'
#' @title Carcass distance example data set
#'
#' @description Example data set of carcass distance found during searches under wind turbines.
#'
#' @usage data(carcassDistance)
#'
#' @details
#'
#' The fictitious wind farm has 100 turbines.
#' For 90 turbines the search plot consists of the turbine pad and the road out to 100 meters (RP = road and pad).
#' The remaining 10 turbines had full plots (FULL) that are circular with a radius of 100 meters.
#' The wind farm was searched during two seasons: Spring and Fall.
#'
#' The example carcasses are all the same size class (bats).
#'
#' Bias trials are also done to account for searcher efficiency and carcass removal.
#' These results are summarized as a probability of detection in the data set.
#'
#' @format A data frame with 56 rows and 6 variables:
#' \describe{
#'   \item{season}{The season in which that carcass was found.}
#'   \item{plotType}{The plot type (either FULL or RP) that was being searched when the carcass was found.}
#' \item{proportionTurbineType}{ The proportion of turbines that have that plot type. For \code{plotType=FULL} the value is 0.1 and 0.9 for \code{plotType=RP}}.
#' \item{distanceFromTurbine}{ Meters, carcass distance from the turbine.}
#' \item{compassBearingDegree}{Degrees, the compass bearing from the turbine to the carcass location. Zero degrees is north, 90 degrees is east, etc.}
#' \item{probabilityDetection}{The summarized probability of detection from the bias trial information.}
#' }
#'
#' @references \url{https://www.fws.gov/ecological-services/es-library/pdfs/WEG_final.pdf}
#'
"carcassDistance"
