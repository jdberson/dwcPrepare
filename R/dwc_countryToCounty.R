#' Assign country, countryCode, stateProvince and county to point locations
#'
#' Takes point locations as an \code{sf} POINT object and county boundaries as
#' an \code{sf} POLYGON object and uses \code{\link[sf]{st_join}} to assign
#' country, countryCode, stateProvince and county Darwin Core terms to each point
#' location.
#'
#' Note that the function expects county to be nested within stateProvince which
#' is in turn nested within country. The \code{sf} POLYGON object with the
#' county boundaries should contain features describing these higher level
#' geographies.
#' @param df A data frame containing the decimal longitude and latitude
#' coordinates
#' @param decimalLongitude The longitude of the focal point
#' @param decimalLatitude The latitude of the focal point
#' @param pointLocations_sf An \code{sf} POINT object to which the country,
#' countryCode, stateProvince and county Darwin Core terms will be assigned.
#' @param county_sf An \code{sf} POLYGON object that includes the county
#' boundaries along with the higher geography features. If no object is given
#' the function uses internal data that contains Australian local government
#' shape data with higher level geographies.
#' @param country_columnName The column name in \code{county_sf} that gives the
#' name of the country in which the county is found. If using the default data
#' this should be left at the default value "country".
#' @param countryCode_columnName The column name in \code{county_sf} that gives
#' the countryCode of the country in which the county is found. If using the
#' default data this should be left at the default value "countryCode".
#' @param stateProvince_columnName The column name in \code{county_sf} that
#' gives the State or Province in which the county is found. If using the
#' default data this should be left at the default value "stateProvince".
#' @param county_columnName The column name in \code{county_sf} that gives the
#' county name. If using the default data this should be left at the default
#' value "stateProvince".
#'
#' @return
#' The original \code{df} object with the additional columns country,
#' countryCode, stateProvince and county.
#'
#' @export
#'
#' @examples
#' pointLocations <- data.frame(
#'   name = c("A", "B", "C"),
#'   lon = c(117.093225, 127.502052, 115.674972),
#'   lat = c(-33.110168, -25.128663, -33.982473))
#'
#' county_sf <-
#'   sf::st_buffer(pointLocations, dist = 1000) |>
#'   dplyr::mutate(
#'     country_raw = c("Country 1", "Country 2", "Country 3"),
#'     countryCode_raw = c("C1", "C2", "C3"),
#'     stateProvince_raw = c("State 1", "State 2", "State 3"),
#'     county_raw = c("County 1", "County 2", "County 3")
#' )
#'
#' dwc_countryToCounty(pointLocations,
#' decimalLongitude = "lon",
#' decimalLatitude = "lat",
#' county_sf = county_sf,
#' country_columnName = "country_raw",
#' countryCode_columnName = "countryCode_raw",
#' stateProvince_columnName = "stateProvince_raw",
#' county_columnName = "county_raw")
#'
dwc_countryToCounty <-
  function(df,
           decimalLongitude,
           decimalLatitude,
           county_sf = dwcPrepare:::county_stateProvince_aus,
           country_columnName = "country",
           countryCode_columnName = "countryCode",
           stateProvince_columnName = "stateProvince",
           county_columnName = "county") {

    pointLocations_sf <-
      sf::st_as_sf(df, coords = c(decimalLongitude, decimalLatitude), remove = FALSE,
                   crs = sf::st_crs("EPSG:4326"))

    # Make sure county_sf and pointLocations_sf have the same crs
    county_sf <-
      sf::st_transform(county_sf, crs = sf::st_crs(pointLocations_sf))

    sf::st_join(
      pointLocations_sf,
      county_sf |>
        dplyr::select(country = !!country_columnName,
                      countryCode = !!countryCode_columnName,
                      stateProvince = !!stateProvince_columnName,
                      county = !!county_columnName)
    ) |>
      sf::st_drop_geometry()
  }
