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
      localityPointLocations_sf = localityPointLocations_sf,
      locationName = "name")

  expect_equal(string_locality, "161 km SE A")
})

test_that("dwc_locality function works with internal data", {

  string_locality <-
    dwc_locality(decimalLongitude = 117.093225,
                 decimalLatitude = -33.110168)

  expect_equal(string_locality, "13 km N DUMBERNING")
})
