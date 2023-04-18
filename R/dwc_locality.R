#' Locality descriptor for given longitude/latitude coordinates
#'
#' This function finds the closest locality to the given point location, as well
#' as its distance and direction from the focal location. The returned string
#' could be used in the Darwin Core
#' \href{http://rs.tdwg.org/dwc/terms/locality}{locality} field.
#'
#' @param decimalLongitude The longitude of the focal point in decimal degrees.
#' Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/decimalLongitude}.
#' @param decimalLatitude The latitude of the focal point in decimal degrees.
#' Output of \code{\link[dwcPrepare]{dwc_coordinates}}.
#' See: \url{http://rs.tdwg.org/dwc/terms/decimalLatitude}.
#' @param localities_sf An \code{sf POINT} object that provides
#' locality names as well as their longitude and latitude for all localities
#' within the area of interest. The package includes the locality information
#' for Australia. See \code{\link[dwcPrepare]{locality_data_aus}}
#' @param localities_names The column name in \code{localities_sf} that gives
#' the locality name.
#' @param ... Additional arguments passed to \code{\link[sf]{st_distance}}
#'
#' @return
#' A string that could be used in the Darwin Core locality field,
#' \url{http://rs.tdwg.org/dwc/terms/locality}.
#'
#' @export
#'
#' @examples
#'localities_sf <-
#'sf::st_as_sf(data.frame(
#'  name = c("A", "B", "C"),
#'  lon = c(117.093225, 127.502052, 115.674972),
#'  lat = c(-33.110168, -25.128663, -33.982473)),
#'  coords = c("lon", "lat"),
#'  crs = sf::st_crs("EPSG:4326"))
#'
#'
#'dwc_locality(
#'  decimalLongitude = 115.949465,
#'  decimalLatitude = -32.029655,
#'  localities_sf = localities_sf,
#'  localities_names = "name"
#'  )
#'
# Vectorise the (not exported) dwc_locality_scalar function (see following
# function)
dwc_locality <-
  function(decimalLongitude,
           decimalLatitude,
           localities_sf,
           localities_names,
           ...){

    purrr::map2_chr(decimalLongitude, decimalLatitude, dwc_locality_scalar,
                    localities_sf, localities_names, ...)

  }

# This function will return a scalar. dwc_locality provides a wrapper using
# purrr::map2_chr() to vectorise dwc_locality_scalar
dwc_locality_scalar <-
  function(decimalLongitude,
           decimalLatitude,
           localities_sf,
           localities_names,
           ...) {


    # Check localities_sf object is in lon/lat
    if(!sf::st_is_longlat(localities_sf)){
      stop("localities_sf must be in longitude latitude",
           call. = FALSE)
    }

    # Check that localities_names is a column heading in localities_sf
    if(!localities_names %in% c(base::colnames(localities_sf))){
      stop(
        stringr::str_c(localities_names,
                       " not found in the localities_sf object"),
        call. = FALSE)
    }

    # Convert lon/lat coordinates to sfc object using the coordinate reference
    # system of localities_sf
    x <- sf::st_sfc(sf::st_point(c(decimalLongitude, decimalLatitude)),
                    crs = sf::st_crs(localities_sf))

    # Calculate the distance between the point and all points in
    # localities_sf
    dist_matrix <- sf::st_distance(x, localities_sf, ...)

    # Find the index of the nearest point in localities_sf
    nearest_point_index <- base::apply(dist_matrix, 1, base::which.min)

    # Extract the distance to the nearest point in localities_sf
    nearest_point_distance_km <-
      base::round(base::apply(dist_matrix, 1, base::min)/1000, 0)

    # Extract the label of the nearest point in localities_sf
    nearest_point <-
      localities_sf[nearest_point_index, localities_names]
    nearest_point_label <-
      base::as.character(sf::st_drop_geometry(nearest_point))

    # Extract the coordinates of the nearest point in localities_sf
    nearest_point_coordinates <- sf::st_coordinates(nearest_point)

    # Find the compass bearing from the given point to the nearest point in
    # localities_sf
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
    # the nearest point in localities_sf
    return(
      stringr::str_c(nearest_point_distance_km, " km ", bearing, " ",
                     nearest_point_label)
    )
  }
