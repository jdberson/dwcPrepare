#' Calculate the coordinateUncertaintyInMeters
#'
#' This function calculates the uncertainty in meters associated with a given
#' set of geographic coordinates. The uncertainty is the sum of two parts:
#' 1. the uncertainty associated with the coordinate precision given a datum and
#' latitude. This is calculated following the equations found at:
#' \url{http://georeferencing.org/georefcalculator/docs/GeorefGuide.html#imprecision_in_coordinates}.
#' If the coordinate reference system is unknown, the default value of 5,359
#' meters is assigned for this component. See Details.
#' 2. the uncertainty associated with the GPS recording device. By default this
#' is given as 30 meters.
#'
#' Please see, Wieczorek C, J Wieczorek (2021) Georeferencing Calculator.
#' Available:  \url{http://georeferencing.org/georefcalculator/gc.html} for more
#' options and further details.
#'
#' The uncertainty associated with geographic coordinates from an unknown datum
#' is taken from
#' \url{https://docs.gbif-uat.org/georeferencing-best-practices/1.0/en/#uncertainty-from-unknown-datum}.
#' Note that this is a worst-case scenario whilst this function lacks the
#' functionality to correctly determine the uncertainty of an unknown datum. If
#' you have only a small number of locations and an unknown datum, we recommend
#' using the Wieczorek C, J Wieczorek (2021) Georeferencing Calculator. The
#' output of the calculator can be copied into R using
#' \code{\link[dwcPrepare]{dwc_format_gco}}.
#'
#' @param decimalLatitude Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' Latitude in decimal degrees. See: \url{http://rs.tdwg.org/dwc/terms/decimalLatitude}.
#' @param coordinatePrecision Output of \code{\link[dwcPrepare]{dwc_coordinatePrecision}}.
#' The precision of the coordinates in degrees. Value must be between 0.00001
#' and 1. See: \url{http://rs.tdwg.org/dwc/terms/coordinatePrecision}.
#' @param geodeticDatum Output of \code{\link[dwcPrepare]{dwc_coordinates}}. The
#' spatial reference system associated with the coordinates. Given as an EPSG
#' code as in the example: "EPSG:4326". See: \url{http://rs.tdwg.org/dwc/terms/geodeticDatum}.
#' @inheritParams dwc_Location
#'
#' @return
#' A numeric. A value that can be used in the Darwin Core
#' \href{http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters}{coordinateUncertaintyInMeters}
#' field.
#'
#' @export
#'
#' @examples
#' dwc_coordinateUncertaintyInMeters(-41.17472, 0.01, "EPSG:4326", 30)
dwc_coordinateUncertaintyInMeters <-
  function(decimalLatitude,
           coordinatePrecision,
           geodeticDatum,
           gps_uncertainty = 30){

    purrr::pmap_dbl(.l = list(decimalLatitude, coordinatePrecision,
                    geodeticDatum,  gps_uncertainty),
                    .f = dwc_coordinateUncertaintyInMeters_scalar)

  }

# Note: the above function vectorises dwc_coordinateUncertaintyInMeters_scalar

# Workhorse - function that calculates the coordinateUncertaintyInMeters
# Not exported.
dwc_coordinateUncertaintyInMeters_scalar <- function(decimalLatitude,
                                              coordinatePrecision,
                                              geodeticDatum,
                                              gps_uncertainty){

  # We first calculate the uncertainty associated with the coordinatePrecision

  # Extract semi_major_axis ("a" in equations) and  flattening ("f" in
  # equations) for the given EPSG code using the crs_data

  if(geodeticDatum != "unknown"){

    # Check coordinatePrecision is betweeen 0.00001 and 1
    if(base::any(coordinatePrecision > 1 || coordinatePrecision < 0.00001)){
      base::stop(
        "coordinatePrecision must "
      )
    }

    # Load data
    crs_data <- dwcPrepare::crs_data

    # Check that given EPSG is in crs_data
    if(!geodeticDatum %in% c(crs_data |> dplyr::pull(.data$epsg_code))){
      base::stop(stringr::str_c(
        geodeticDatum, ' is not a currently supported geodeticDatum. For a list of
      supported EPSG codes please see the epsg_code column in data("crs_data").'
      ), call. = FALSE)
    }


    # Extract semi_major_axis for EPSG
    a <-
      crs_data |>
      dplyr::filter(.data$epsg_code == geodeticDatum) |>
      dplyr::pull(.data$semi_major_axis)

    # Extract flattenting for EPSG
    f <-
      1 /
      crs_data |>
      dplyr::filter(.data$epsg_code == geodeticDatum) |>
      dplyr::pull(.data$inverse_flattening)

    # Calculate coordinatePrecisionUncertainty
    latitude_radians <-
      base::pi * decimalLatitude / 180

    e_squared <-
      2*f - f^2

    N <-
      a / base::sqrt(1 - e_squared * base::sin(latitude_radians)^2)

    X <- base::abs(N * cos(latitude_radians))

    R <-
      a * (1 - e_squared) / (1 - e_squared * base::sin(latitude_radians)^2 )^(3/2)

    longitude_error <-
      base::pi*X*coordinatePrecision / 180

    latitude_error <-
      base::pi*R*coordinatePrecision / 180

    coordinatePrecisionUncertainty <-
      base::sqrt(latitude_error^2 + longitude_error^2)
  } else

    # Assign coordinatePrecisionUncertainty of 1,000m if datum unknown
    coordinatePrecisionUncertainty <- 5359

  # Calculate coordinateUncertaintyInMeters as sum of gps_uncertainty and
  # coordinatePrecisionUncertainty
  coordinateUncertaintyInMeters <-
    base::round(coordinatePrecisionUncertainty + gps_uncertainty, 0)

  return(coordinateUncertaintyInMeters)


}
