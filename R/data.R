#' A made-up survey data set for use in examples and vignettes
#'
#' Tasmanian Tigers (Thylacinus cynocephalus) are rumoured to still roam the
#' wilds of Tasmania. This is a 'mock' dataset of a survey of Tasmanian Tigers.
#' Sadly no tigers were caught!
#'
#' @format A data frame with 8 rows and 13 variables:
#' \describe{
#'   \item{site}{name given to each site}
#'   \item{trap}{there were two traps at each site, 1 and 2}
#'   \item{date_trap_setup}{the date time the trap was setup}
#'   \item{date_trap_collected}{the date time the trap was collected}
#'   \item{longitude_dd}{longitude of the trap in decimal degrees}
#'   \item{latitude_dd}{latitude of the trap in decimal degrees}
#'   \item{longitude_dms}{longitude of the trap in degrees minutes seconds}
#'   \item{latitude_dms}{latitude of the trap in degrees minutes seconds}
#'   \item{longitude_ddm}{longitude of the trap in degrees decimal minutes}
#'   \item{latitude_ddm}{latitude of the trap in degrees decimal minutes}
#'   \item{gps_uncertainty}{the uncertainty of the coordinates given by the GPS}
#'   \item{species}{the species being identified / counted}
#'   \item{count}{the number of individuals found in a trap}
#' }
"thylacine_data"

#' sf POLYGON object of local government areas on the main Tasmanian island
#'
#' Polygons and labels are modified from shape data provided by the Australian
#' Bureau of Statistics showing local government areas (the Australian
#' Darwin Core 'county' equivalent). Note that the polygons have been simplified
#' to reduce the file size. The data is for example use only.
#'
#' @format An sf object with 27 features (rows) and 4 fields (variables):
#' \describe{
#'   \item{country}{\url{http://rs.tdwg.org/dwc/terms/country}}
#'   \item{countryCode}{\url{http://rs.tdwg.org/dwc/terms/countryCode}}
#'   \item{stateProvince}{\url{http://rs.tdwg.org/dwc/terms/stateProvince}}
#'   \item{county}{\url{http://rs.tdwg.org/dwc/terms/county}}
#'   \item{geometry}{simple feature geometries}
#' }
"county_tas"


#' sf POINT object of Australian localities
#'
#' Data is from "Elvis - Place Names - Foundation Spatial Data - Locality Data",
#' sourced from Commonwealth of Australia (The Intergovernmental Committee on
#' Surveying and Mapping, ICSM) 2018, under a Creative Commons Attribution 4.0
#' International license. See \url{https://placenames.fsdf.org.au/}. Note that
#' data received has been subset so that only the NAME field and geometry
#' column are retained in the sf object. NAME has been renamed to locality_name.
#'
#' @format An sf object with 16197 features (rows) and 1 fields (variables):
#' \describe{
#'   \item{locality_name}{name of the locality}
#'   \item{geometry}{simple feature geometries}
#' }
"locality_data_aus"


#' Data for semi-major axis and ellipsoid flattening for EPSG codes
#'
#' Calculating coordinate precision uncertainty requires the values for the
#' semi-major axis and the ellipsoid flattening for the specified coordinate
#' reference system. This data uses \code{\link[sf]{st_crs}} to extract the
#' values from the EPSG codes.
#'
#' @format A data frame with 869 rows and 4 variables:
#' \describe{
#'   \item{epsg_code}{EPSG code for the coordinate reference system}
#'   \item{name}{Name of the coordinate reference system}
#'   \item{semi_major_axis}{radius at the equator for the given datum}
#'   \item{inverse_flattening}{inverse of the flattening component of the datum}
#' }
"crs_data"


