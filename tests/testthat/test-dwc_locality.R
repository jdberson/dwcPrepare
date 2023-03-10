test_that("dwc_locality function works", {
  localityPointLocations_sf <-
    sf::st_as_sf(data.frame(
      name = c("A", "B", "C"),
      lon = c(117.093225, 127.502052, 115.674972),
      lat = c(-33.110168, -25.128663, -33.982473)),
      coords = c("lon", "lat"), crs = sf::st_crs("EPSG:4326"))


  string_locality <-
    dwc_locality(
      decimalLongitude = 115.949465, decimalLatitude = -32.029655,
      localities_sf = localityPointLocations_sf,
      localities_names = "name")

  expect_equal(string_locality, "161 km SE A")
})

test_that("dwc_locality function works with internal data", {

  data("locality_data_aus")

  string_locality <-
    dwc_locality(decimalLongitude = 117.093225,
                 decimalLatitude = -33.110168,
                 localities_sf = locality_data_aus,
                 localities_names = "locality_name")

  expect_equal(string_locality, "13 km N DUMBERNING")
})


test_that("dwc_locality error message works", {

  # Checks on decimal degrees -----------------------------------------------

  expect_error(dwc_locality(decimalLongitude = 117.093225,
                            decimalLatitude = -33.110168,
                            localities_sf = locality_data_aus,
                            localities_names = "wrong_names"),
               "wrong_names not found in the localities_sf object")

})
