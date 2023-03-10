#' Assign country, countryCode, stateProvince and county to point locations
#'
#' Takes point locations as an \code{sf} POINT object and county boundaries as
#' an \code{sf} POLYGON object and uses \code{\link[sf]{st_join}} to assign
#' country, countryCode, stateProvince and county Darwin Core terms to each
#' point location.
#'
#' Note that the function expects county to be nested within stateProvince which
#' is in turn nested within country. The \code{sf} POLYGON object with the
#' county boundaries should contain features describing these higher level
#' geographies.
#'
#' @param df A data frame containing the decimal longitude and latitude
#' coordinates
#' @param county_sf An \code{sf POLYGON} object that includes the county
#' boundaries along with the higher geography features.
#' @param country_column_name The column name in \code{county_sf} that gives the
#' name of the country in which the county is found. The default value is
#' "country".
#' @param countryCode_column_name The column name in \code{county_sf} that gives
#' the countryCode of the country in which the county is found. The default
#' value is "countryCode".
#' @param stateProvince_column_name The column name in \code{county_sf} that
#' gives the State or Province in which the county is found.The default value is
#' "stateProvince".
#' @param county_column_name The column name in \code{county_sf} that gives the
#' county name. The default value is "county".
#' @inheritParams dwc_locality
#'
#' @return
#' The original \code{df} object with the additional columns
#' \href{http://rs.tdwg.org/dwc/terms/country}{country},
#' \href{http://rs.tdwg.org/dwc/terms/countryCode}{countryCode},
#' \href{http://rs.tdwg.org/dwc/terms/stateProvince}{stateProvince} and
#' \href{http://rs.tdwg.org/dwc/terms/county}{county}.
#'
#' @export
#'
#' @examples
#' # Make up some point data
#' point_locations <- data.frame(
#'   name = c("A", "B", "C"),
#'   lon = c(117.093225, 127.502052, 115.674972),
#'   lat = c(-33.110168, -25.128663, -33.982473))
#'
#' # Make up some county polygon data nested within higher geographies
#' county_sf <-
#'   sf::st_buffer(sf::st_as_sf(point_locations,
#'                              coords = c("lon", "lat"),
#'                              remove = FALSE,
#'                              crs = sf::st_crs("EPSG:4326")), dist = 1000) |>
#'   dplyr::mutate(
#'     country_raw = c("Country 1", "Country 2", "Country 3"),
#'     countryCode_raw = c("C1", "C2", "C3"),
#'     stateProvince_raw = c("State 1", "State 2", "State 3"),
#'     county_raw = c("County 1", "County 2", "County 3")
#'   )
#'
#' # Run the function
#' dwc_country_to_county(point_locations,
#'                     decimalLongitude = "lon",
#'                     decimalLatitude = "lat",
#'                     county_sf = county_sf,
#'                     country_column_name = "country_raw",
#'                     countryCode_column_name = "countryCode_raw",
#'                     stateProvince_column_name = "stateProvince_raw",
#'                     county_column_name = "county_raw")
dwc_country_to_county <-
  function(df,
           decimalLongitude,
           decimalLatitude,
           county_sf,
           country_column_name = "country",
           countryCode_column_name = "countryCode",
           stateProvince_column_name = "stateProvince",
           county_column_name = "county") {

    pointLocations_sf <-
      sf::st_as_sf(df, coords = c(decimalLongitude, decimalLatitude),
                   remove = FALSE,
                   crs = sf::st_crs("EPSG:4326"))

    # Make sure county_sf and pointLocations_sf have the same crs
    county_sf <-
      sf::st_transform(county_sf, crs = sf::st_crs(pointLocations_sf))

    sf::st_join(
      pointLocations_sf,
      county_sf |>
        dplyr::select(country = !!country_column_name,
                      countryCode = !!countryCode_column_name,
                      stateProvince = !!stateProvince_column_name,
                      county = !!county_column_name)
    ) |>
      sf::st_drop_geometry()
  }
