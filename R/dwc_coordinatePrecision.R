#' Calculate the coordinatePrecision in degrees for geographic coordinates
#'
#' This function calculates the precision of geographic coordinates.
#'
#' @param verbatimLongitude Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/verbatimLongitude}.
#' @param verbatimLatitude Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/verbatimLatitude}.
#' @inheritParams dwc_Location
#'
#' @return
#' A numeric. A value that can be used in the Darwin Core coordinatePrecision
#' field. See: \url{http://rs.tdwg.org/dwc/terms/coordinatePrecision}. The
#' maximum precision returned is 0.00001 degrees, or approximately 1.11 m at the
#' equator.
#'
#' @export
#' @examples
#' # For a single point
#' dwc_coordinatePrecision(144.9786, -41.17472,
#' verbatimCoordinateSystem = "decimal degrees")
#'
#' # For a set of points
#' data("thylacine_data")
#' thylacine_data |>
#' dplyr::mutate(coordinatePrecision =
#' dwc_coordinatePrecision(verbatimLatitude = latitude_dd,
#'                         verbatimLongitude = longitude_dd,
#'                         verbatimCoordinateSystem = "decimal degrees"))
#'
dwc_coordinatePrecision <- function(
    verbatimLongitude,
    verbatimLatitude,
    verbatimCoordinateSystem){

  purrr::pmap_dbl(.l = list(verbatimLongitude,
                            verbatimLatitude,
                            verbatimCoordinateSystem),
                  .f = dwc_coordinatePrecision_scalar)

}

# Note: the above function vectorises dwc_coordinatePrecision_scalar

# Workhorse - function that calculates the coordinatePrecision
# Not exported.
dwc_coordinatePrecision_scalar <-
  function(verbatimLongitude, verbatimLatitude, verbatimCoordinateSystem){

    # Longitude/latitude check
    dwc_check_longitude_latitude(
      longitude = verbatimLongitude,
      latitude = verbatimLatitude,
      verbatimCoordinateSystem = verbatimCoordinateSystem)



    if(verbatimCoordinateSystem == "decimal degrees"){

      lat_precision <- dwc_precision_extractor(verbatimLatitude)
      lon_precision <- dwc_precision_extractor(verbatimLongitude)

      coordinatePrecision <- min(lat_precision, lon_precision)


    }

    if(verbatimCoordinateSystem == "degrees decimal minutes"){

      lat_precision = dwc_precision_extractor(
        base::as.numeric(
          stringr::str_split_i(verbatimLatitude, pattern = " ", 2))
      ) / 60

      lon_precision = dwc_precision_extractor(
        base::as.numeric(
          stringr::str_split_i(verbatimLongitude, pattern = " ", 2))
      ) / 60

      coordinatePrecision <- min(lat_precision, lon_precision)

    }

    if(verbatimCoordinateSystem == "degrees minutes seconds"){

      if(base::as.numeric(stringr::str_split_i(verbatimLatitude, " ", 3)) > 0){
        lat_precision <-
          dwc_precision_extractor(
            base::as.numeric(stringr::str_split_i(verbatimLatitude, " ", 3))
          ) /3600
      } else if(base::as.numeric(stringr::str_split_i(verbatimLatitude, " ", 2)) > 0) {
        lat_precision <- 1/60
      } else {lat_precision <- 1}

      if(base::as.numeric(stringr::str_split_i(verbatimLongitude, " ", 3)) > 0){
        lon_precision <-
          dwc_precision_extractor(
            base::as.numeric(stringr::str_split_i(verbatimLongitude, " ", 3))
          ) /3600
      } else if(base::as.numeric(stringr::str_split_i(verbatimLongitude, " ", 2)) > 0) {
        lon_precision <- 1/60
      } else {lon_precision <- 1}

      coordinatePrecision <- min(lat_precision, lon_precision)

    }


    return(base::max(0.00001, coordinatePrecision))


  }
