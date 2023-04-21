#' Prepare geographic coordinates and convert to decimal degrees if needed
#'
#' This function prepares the raw (verbatim) coordinate information and if
#' necessary, converts coordinates from 'degrees minutes seconds' or 'degrees
#' decimal minutes' to 'decimal degrees' using the R package
#' [parzer](https://docs.ropensci.org/parzer/). Where longitude and latitude are
#' provided in decimal degrees, this function will round these to a maximum of
#' seven decimal places.
#'
#' @inheritParams dwc_Location
#'
#' @return
#' A \code{\link[tibble]{tibble}} with Darwin Core terms
#' \href{http://rs.tdwg.org/dwc/terms/decimalLatitude}{decimalLatitude},
#' \href{http://rs.tdwg.org/dwc/terms/decimalLongitude}{decimalLongitude},
#' \href{http://rs.tdwg.org/dwc/terms/geodeticDatum}{geodeticDatum},
#' \href{http://rs.tdwg.org/dwc/terms/verbatimLatitude}{verbatimLatitude},
#' \href{http://rs.tdwg.org/dwc/terms/verbatimLongitude}{verbatimLongitude},
#' \href{http://rs.tdwg.org/dwc/terms/verbatimCoordinateSystem}{verbatimCoordinateSystem}
#' and \href{http://rs.tdwg.org/dwc/terms/verbatimSRS}{verbatimSRS}.
#'
#' @export
#'
#' @examples
#' data("thylacine_data")
#' thylacine_data |>
#'   dplyr::mutate(
#'     dwc_coordinates(
#'       longitude = longitude_dms,
#'       latitude = latitude_dms,
#'       verbatimCoordinateSystem = "degrees minutes seconds",
#'       verbatimSRS = "EPSG:4326"
#'     )
#'   )
dwc_coordinates <- function(
    longitude,
    latitude,
    verbatimCoordinateSystem,
    verbatimSRS) {
  purrr::pmap_df(
    .l = list(
      longitude,
      latitude,
      verbatimCoordinateSystem,
      verbatimSRS
    ),
    .f = dwc_coordinates_scalar
  )
}

dwc_coordinates_scalar <- function(
    longitude,
    latitude,
    verbatimCoordinateSystem,
    verbatimSRS) {
  # Initial checks - throw a warning and stops function if conditions aren't met

  # Longitude/latitude check
  dwc_check_longitude_latitude(
    longitude = longitude,
    latitude = latitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem
  )

  # Check that verbatimSRS EPSG is found in crs_data
  crs_data <- dwcPrepare::crs_data
  if (!verbatimSRS %in% c("unknown", crs_data |> dplyr::pull(.data$epsg_code))) {
    base::stop(stringr::str_c(
      verbatimSRS, ' is not a currently supported verbatimSRS. For a list of
      supported EPSG codes please see the epsg_code column in data("crs_data").'
    ), call. = FALSE)
  }


  # Convert coordinates to decimal degrees if needed
  if (verbatimCoordinateSystem %in%
    c("degrees decimal minutes", "degrees minutes seconds")) {
    decimalLongitude <-
      base::format(parzer::parse_lon(longitude), nsmall = 7)

    decimalLatitude <-
      base::format(parzer::parse_lat(latitude), nsmall = 7)
  } else {
    # If coordinates are already in decimal degrees, round to 7 decimal places
    decimalLongitude <-
      base::round(longitude, digits = 7)

    decimalLatitude <-
      base::round(latitude, digits = 7)
  }


  # Record geodeticDatum - update this when verbatimSRS allows for more than
  # just EPSG codes
  geodeticDatum <- verbatimSRS

  return(tibble::tibble(
    decimalLatitude = base::as.numeric(decimalLatitude),
    decimalLongitude = base::as.numeric(decimalLongitude),
    geodeticDatum = geodeticDatum,
    verbatimLatitude = latitude,
    verbatimLongitude = longitude,
    verbatimCoordinateSystem = verbatimCoordinateSystem,
    verbatimSRS = verbatimSRS
  ))
}
