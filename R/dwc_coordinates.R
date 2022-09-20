#' Prepare geographic coordinates and convert to decimal degrees if needed
#'
#' This function prepares the raw (verbatim) coordinate information and if
#' necessary, converts coordinates from 'degrees minutes seconds' or 'degrees
#' decimal minutes' to 'decimal degrees' using the R package
#' [parzer](https://docs.ropensci.org/parzer/).
#'
#' Note that where longitude and latitude are provided in decimal degrees, this
#' function will round these to a maximum of seven decimal places.
#'
#' @param locationID The location identifier. See:
#' \url{http://rs.tdwg.org/dwc/terms/locationID}
#' @param lon Longitude. This will be assigned to "verbatimLongitude" in the
#' returned tibble. See: \url{http://rs.tdwg.org/dwc/terms/verbatimLongitude}.
#' Note that this must be a character value if \code{lon} is in "degrees decimal
#' minutes" or "degrees minutes seconds".
#' @param lat Latitude. This will be assigned to "verbatimLatitude" in the
#' returned tibble. See: \url{http://rs.tdwg.org/dwc/terms/verbatimLatitude}.
#' Note that this must be a character value if \code{lat} is in "degrees decimal
#' minutes" or "degrees minutes seconds".
#' @param verbatimCoordinateSystem Supported terms are "decimal degrees",
#' "degrees decimal minutes" or "degrees minutes seconds". See:
#' \url{http://rs.tdwg.org/dwc/terms/verbatimCoordinateSystem}
#' @param verbatimSRS The spatial reference system associated with the
#' coordinates. Given as an EPSG code as in the example: "EPSG:4326". See:
#' \url{http://rs.tdwg.org/dwc/terms/verbatimSRS}.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with Darwin Core terms decimalLongitude,
#' decimalLatitude, geodeticDatum, verbatimLongitude, verbatimLatitude,
#' verbatimCoordinateSystem and verbatimSRS.
#' @export
#'
#' @examples
#' data("thylacine_data")
#' thylacine_data |>
#' dplyr::mutate(dwc_coordinates(lon = longitude_dms, lat = latitude_dms,
#' verbatimCoordinateSystem = "degrees minutes seconds",
#' verbatimSRS = "EPSG:4326"))
dwc_coordinates <- function(
    locationID,
    lon,
    lat,
    verbatimCoordinateSystem,
    verbatimSRS = "unknown"){

  purrr::pmap_df(.l = list(locationID,
                         lon,
                         lat,
                         verbatimCoordinateSystem,
                         verbatimSRS),
              .f = dwc_coordinates_scalar)

}

dwc_coordinates_scalar <- function(
    locationID,
    lon,
    lat,
    verbatimCoordinateSystem,
    verbatimSRS){

  # Initial checks - throw a warning and stops function if conditions aren't met

  # Check locationID is given
  if(base::missing(locationID)){
    base::stop(
      "locationID missing. Please provide a location identifier."
    )
  }

  # Checks of lon/lat arguments

  # Check that lon/lat are given
  if(base::missing(lon)){
    base::stop(
      '"lon" missing, please provide the longitude', call. = FALSE)
  }

  if(base::missing(lat)){
    base::stop(
      '"lat" missing, please provide the latitude', call. = FALSE)
  }

  # Check that lon and lat are in the correct form
  if(verbatimCoordinateSystem %in%
     c("degrees decimal minutes", "degrees minutes seconds") &&
     (!base::is.character(lon) ||
      !base::is.character(lat))) {
    base::stop(
      "lon and lat must be a character",
      call. = FALSE)
  }

  if(verbatimCoordinateSystem == "decimal degrees" &&
     (!base::is.numeric(lon) ||
      !base::is.numeric(lat))) {
    base::stop(
      "lon and lat must be numeric",
      call. = FALSE)
  }

  # Throw a message to prompt checks of output if necessary
  if(verbatimCoordinateSystem == "degrees decimal minutes" &&
     (base::any(!stringr::str_detect(lon,
                                     "\\d+\\s\\d+\\.\\d+\\s[:alpha:]")) ||
      base::any(!stringr::str_detect(lat,
                                     "\\d+\\s\\d+\\.\\d+\\s[:alpha:]")) )) {
    base::message(
      'The recommended format for coordinates in degrees decimal minutes is as
    per this example: "121 10.566667 W". If the output decimalLatitude and
    decimalLongitude appear incorrect, try changing the longitude and
    latitude to this format and then re-run the function.')
  }

  if(verbatimCoordinateSystem == "degrees minutes seconds" &&
     (base::any(!stringr::str_detect(lon,
                                     "\\d+\\s\\d+\\s\\d+\\s[:alpha:]")) ||
      base::any(!stringr::str_detect(lat,
                                     "\\d+\\s\\d+\\s\\d+\\s[:alpha:]")) )) {
    base::message(
      'The recommended format for coordinates in degrees minutes seconds is as
    per this example: "121 10 34 W". If the output decimalLatitude and
    decimalLongitude appear incorrect, try changing the longitude and
    latitude to this format and then re-run the function.')
  }

  # Check lon/lat when in decimal degrees (parzer does checks for other formats)
  if(verbatimCoordinateSystem == "decimal degrees"){

    if(base::any(!(lon >= -180  ||  lon <= 180))) {
      base::stop(
        "longitude must be between -180 and 180",
        call. = FALSE)
    }

    if(base::any(!(lat >= -90  ||  lat <= 90))) {
      base::stop(
        "latitude must be between -90 and 90",
        call. = FALSE)
    }

  }

  # Check supplied value of verbatimCoordinateSystem
  if(!(verbatimCoordinateSystem %in%
       c("decimal degrees", "degrees decimal minutes",
         "degrees minutes seconds"))) {
    base::stop(
      'verbatimCoordinateSystem must be one of "decimal degrees",
      "degrees decimal minutes" or "degrees minutes seconds"',
      call. = FALSE)
  }



  # Check that verbatimSRS EPSG is found in crs_data
  crs_data <- dwcPrepare::crs_data
  if(!verbatimSRS %in% c("unknown", crs_data |> dplyr::pull(epsg_code))){
    base::stop(stringr::str_c(
      verbatimSRS, ' is not a currently supported verbatimSRS. For a list of
      supported EPSG codes please see the epsg_code column in data("crs_data").'
    ), call. = FALSE)
  }


  # Convert coordinates to decimal degrees if needed
  if(verbatimCoordinateSystem  %in%
     c("degrees decimal minutes", "degrees minutes seconds")){

    decimalLongitude =
      base::format(parzer::parse_lon(lon), nsmall = 7)

    decimalLatitude =
      base::format(parzer::parse_lat(lat), nsmall = 7)


  } else {

    # If coordinates are already in decimal degrees, round to 7 decimal places
    decimalLongitude =
      base::format(base::round(lon, digits = 7), nsmall = 7)

    decimalLatitude =
      base::format(base::round(lat, digits = 7), nsmall = 7)
  }


  # Record geodeticDatum - update this when verbatimSRS allows for more than
  # just EPSG codes
  geodeticDatum = verbatimSRS

  return(tibble::tibble(
    locationID = locationID,
    decimalLatitude = base::as.numeric(decimalLatitude),
    decimalLongitude = base::as.numeric(decimalLongitude),
    geodeticDatum = geodeticDatum,
    verbatimLatitude = lat,
    verbatimLongitude = lon,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = verbatimSRS))

}
