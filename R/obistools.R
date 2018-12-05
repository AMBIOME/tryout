#' auto_qc: Automatic quality control of physical, chemical and marine biological data
#'
#' Control data sets submitted to the Swedish Oceanographic data center SHARK (<https://sharkweb.smhi.se/>). It can also be used for checking the quality of a set of marine species distribution records.
#'
#' @docType package
#' @name obistools
#' @import dplyr
#' @import ggplot2
#' @import sp
#' @importFrom rgeos gDistance
#' @importFrom rgeos readWKT
#' @importFrom leaflet leaflet
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom stringr str_match
#' @importFrom stringr str_split
#' @importFrom geosphere distm
#' @import rmarkdown
#' @import knitr
#' @import xml2
#' @import tidyr
#' @import data.tree
#' @import digest
#' @importFrom stats complete.cases
#' @importFrom stats na.omit
#' modified from https://iobis.github.io/obistools
#' Provoost P and Bosch S (2018). “obistools: Tools for data enhancement and quality control.” Ocean Biogeographic Information System. Intergovernmental Oceanographic Commission of UNESCO. https://cran.r-project.org/package=obistools.

NULL
