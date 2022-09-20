#' Locality descriptor for given longitude/latitude coordinates
#'
#' This function finds the closest location from a set of candidate locations to
#' the given longitude/latitude, as well as the name of the closest location,
#' and the direction of the closest location from the focal longitude/latitude.
#'
#' @param decimalLongitude The longitude of the focal point
#' @param decimalLatitude The latitude of the focal point
#' @param localityPointLocations_sf An \code{sf} POINT object. Points must be in
#' longitude / latitude. If no object is given the function uses internal data
#' that contains Australian locality information.
#' @param locationName The column name in
#' \code{localityPointLocations_sf} that gives the location name. If using the
#' default data this should be left at the default value "localityName".
#' @param ... Additional arguments passed to \code{\link[sf]{st_distance}}
#'
#' @return
#' A string that could be used in the Darwin Core locality field,
#' \url{https://dwc.tdwg.org/terms/#location}.
#'
#' @export
#'
#' @examples
#'localityPointLocations_sf <-
#'sf::st_as_sf(data.frame(
#'  name = c("A", "B", "C"),
#'  lon = c(117.093225, 127.502052, 115.674972),
#'  lat = c(-33.110168, -25.128663, -33.982473)),
#'  coords = c("lon", "lat"),
#'  crs = sf::st_crs("EPSG:4326"))
#'
#'
#'dwc_locality(
#'  decimalLongitude = 115.949465, decimalLatitude = -32.029655,
#'  localityPointLocations_sf = localityPointLocations_sf, locationName = "name"
#'  )
#'
# Vectorise the (not exported) dwc_locality_scalar function (see following
# function)
dwc_locality <-
  function(decimalLongitude,
           decimalLatitude,
           localityPointLocations_sf = dwcPrepare:::locality_data_aus,
           locationName = "localityName",
           ...){

    purrr::map2_chr(decimalLongitude, decimalLatitude, dwc_locality_scalar,
                    localityPointLocations_sf, locationName, ...)

  }

# This function will return a scalar. dwc_locality provides a wrapper using
# purrr::map2_chr() to vectorise dwc_locality_scalar
dwc_locality_scalar <-
  function(decimalLongitude,
           decimalLatitude,
           localityPointLocations_sf,
           locationName,
           ...) {


    # Check localityPointLocations_sf object is in lon/lat
    if(!sf::st_is_longlat(localityPointLocations_sf)){
      stop("localityPointLocations_sf must be in longitude latitude",
           call. = FALSE)
    }

    # Convert lon/lat coordinates to sfc object using the coordinate reference
    # system of localityPointLocations_sf
    x <- sf::st_sfc(sf::st_point(c(decimalLongitude, decimalLatitude)),
                    crs = sf::st_crs(localityPointLocations_sf))

    # Calculate the distance between the point and all points in
    # localityPointLocations_sf
    dist_matrix <- sf::st_distance(x, localityPointLocations_sf, ...)

    # Find the index of the nearest point in localityPointLocations_sf
    nearest_point_index <- base::apply(dist_matrix, 1, base::which.min)

    # Extract the distance to the nearest point in localityPointLocations_sf
    nearest_point_distance_km <-
      base::round(base::apply(dist_matrix, 1, base::min)/1000, 0)

    # Extract the label of the nearest point in localityPointLocations_sf
    nearest_point <-
      localityPointLocations_sf[nearest_point_index, locationName]
    nearest_point_label <-
      base::as.character(sf::st_drop_geometry(nearest_point))

    # Extract the coordinates of the nearest point in localityPointLocations_sf
    nearest_point_coordinates <- sf::st_coordinates(nearest_point)

    # Find the compass bearing from the given point to the nearest point in
    # localityPointLocations_sf
    b <- geosphere::bearing(c(decimalLongitude, decimalLatitude),
                            nearest_point_coordinates)

    compass_dir_deg <-
      stats::setNames(
        base::seq(0, 360, by=22.5),
        c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", "S",
          "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
      )

    if (b < 0) b <- b + 360
    bearing <- base::names(base::which.min(base::abs(compass_dir_deg - b)))

    # Return a string showing the distance and direction to, and the name of,
    # the nearest point in localityPointLocations_sf
    return(
      stringr::str_c(nearest_point_distance_km, " km ", bearing, " ",
                     nearest_point_label)
    )


  }
