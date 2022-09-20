#' Calculate the coordinatePrecision in degrees for geographic coordinates
#'
#' If verbatimLongitude and verbatimLatitude are in decimal degrees, this
#' function will use the coordinate values to calculate coordinate precision.
#' Alternatively, the coordinate precision can be assigned by specifying both
#' the precision and unit arguments. The precision and unit arguments must be
#' supplied if the verbatim coordinates are in "degrees minutes seconds" or
#' "degrees decimal minutes".
#'
#' @param verbatimLongitude Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/verbatimLongitude}.
#' @param verbatimLatitude Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/verbatimLatitude}.
#' @param precision The precision of the coordinates. Only provide a value if
#' verbatimLongitude and verbatimLatitude are in "degrees minutes seconds" or
#' "degrees decimal minutes", or if you wish to override the function and
#' specify the precision for all coordinates. Default is NULL.
#' @param unit The unit of precision (one of either "degrees", "minutes" or
#' "seconds"). Only provide a value if precision is given.
#'
#' @return
#' A numeric. A value that can be used in the Darwin Core coordinatePrecision
#' field. See: \url{http://rs.tdwg.org/dwc/terms/coordinatePrecision}. The
#' maximum precision returned is 0.00001 degrees, or approximately 1.11 m at the
#' equator.
#'
#' @export
#' @examples
#' dwc_coordinatePrecision(144.9786, -41.17472)
dwc_coordinatePrecision <- function(
    verbatimLongitude,
    verbatimLatitude,
    precision = NULL,
    unit = NULL){

  purrr::pmap_dbl(.l = list(verbatimLongitude,
                            verbatimLatitude,
                            precision,
                            unit),
                  .f = dwc_coordinatePrecision_scalar)

}

# Note: the above function vectorises dwc_coordinatePrecision_scalar

# Workhorse - function that calculates the coordinatePrecision if needed
# Not exported.
dwc_coordinatePrecision_scalar <-
  function(verbatimLongitude, verbatimLatitude, precision, unit){

    if(!base::is.null(precision)){

      if(is.null(unit) || !unit %in% c("degrees", "minutes", "seconds")){
        stop(
          'Please provide the unit for the given precision as either "degrees",
          "minutes" or "seconds".', call. = FALSE
        )
      }

    if(unit == "degrees"){
      coordinatePrecision = dwc_precision_extractor(precision)
    }

    if(unit == "minutes"){
      coordinatePrecision = precision/60
    }

    if(unit == "seconds"){
      coordinatePrecision = precision/3600
    }


      return(base::max(0.00001, coordinatePrecision))


    } else

    # Check numerics
    if(!base::is.numeric(verbatimLongitude) ||
       !base::is.numeric(verbatimLatitude)) {
      stop(
        'verbatimLongitude and verbatimLatitude must be numerics. This function
        calculates coordinatePrecision for coordinates in decimal degrees only.
        If the verbatim coordinates are in "degrees minutes seconds" or "degrees
        decimal minutes" please supply the precision and unit arguments to this
        function. Note that these will apply to all given coordinates',
        call. = FALSE
      )
    }

    lat_precision = dwc_precision_extractor(verbatimLatitude)
    lon_precision = dwc_precision_extractor(verbatimLongitude)

    lat_lon_precision = min(lat_precision, lon_precision)

    return(base::max(0.00001, lat_lon_precision))

  }


# Function to extract precision from decimal degrees coordinates.
# Not exported.
dwc_precision_extractor <- function(x){

  if(stringr::str_detect(base::as.character(x), "\\.")) {
    10^-(base::nchar(stringr::str_split_i(base::as.character(x), "\\.", 2)))
  } else

    1
}
