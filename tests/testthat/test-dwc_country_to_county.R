test_that("dwc_country_to_county works", {
  pointLocations_sf <-
    sf::st_as_sf(
      data.frame(
        name = c("A", "B", "C"),
        lon = c(117.093225, 127.502052, 115.674972),
        lat = c(-33.110168, -25.128663, -33.982473)
      ),
      coords = c("lon", "lat"), crs = sf::st_crs("EPSG:4326")
    )

  county_sf <-
    sf::st_buffer(pointLocations_sf, dist = 1000) |>
    dplyr::mutate(
      country_raw = c("Country 1", "Country 2", "Country 3"),
      countryCode_raw = c("C1", "C2", "C3"),
      stateProvince_raw = c("State 1", "State 2", "State 3"),
    )

  dwc_country_to_county_testData1 <- tibble::tribble(
    ~name, ~country, ~countryCode, ~stateProvince, ~county,
    "A", "Country 1", "C1", "State 1", "A",
    "B", "Country 2", "C2", "State 2", "B",
    "C", "Country 3", "C3", "State 3", "C"
  )

  # Prepare test data
  dwc_country_to_county_testData2 <-
    dwc_country_to_county(
      df = pointLocations_sf,
      county_sf = county_sf,
      country_column_name = "country_raw",
      countryCode_column_name = "countryCode_raw",
      stateProvince_column_name = "stateProvince_raw",
      county_column_name = "name"
    ) |>
    sf::st_drop_geometry() |>
    tibble::tibble()

  expect_equal(dwc_country_to_county_testData1, dwc_country_to_county_testData2)
})
