test_that("dwc_Location works", {

  # Without locality of country to county
  expect_equal(

    dwc_Location(longitude = 144.9786072,
                    latitude = -41.1747208,
                    verbatimCoordinateSystem = "decimal degrees",
                    verbatimSRS = "EPSG:4326"),

    tibble::tibble(decimalLatitude = -41.1747208,
                   decimalLongitude = 144.9786072,
                   geodeticDatum = "EPSG:4326",
                   coordinateUncertaintyInMeters = 31,
                   coordinatePrecision = 0.00001,
                   verbatimLatitude = -41.1747208,
                   verbatimLongitude = 144.9786072,
                   verbatimCoordinateSystem = "decimal degrees",
                   verbatimSRS = "EPSG:4326")
  )


  # With locality
  expect_equal(

    dwc_Location(longitude = 144.9786072,
                 latitude = -41.1747208,
                 verbatimCoordinateSystem = "decimal degrees",
                 verbatimSRS = "EPSG:4326",
                 localities_sf = locality_data_aus,
                 localities_names = "locality_name"),

    tibble::tibble(locality = "13 km NNE Roger River",
                   decimalLatitude = -41.1747208,
                   decimalLongitude = 144.9786072,
                   geodeticDatum = "EPSG:4326",
                   coordinateUncertaintyInMeters = 31,
                   coordinatePrecision = 0.00001,
                   verbatimLatitude = -41.1747208,
                   verbatimLongitude = 144.9786072,
                   verbatimCoordinateSystem = "decimal degrees",
                   verbatimSRS = "EPSG:4326")
  )

  # With country to county
  expect_equal(

    dwc_Location(longitude = 144.9786072,
                 latitude = -41.1747208,
                 verbatimCoordinateSystem = "decimal degrees",
                 verbatimSRS = "EPSG:4326",
                 county_sf = county_tas),

    tibble::tibble(country = "Australia",
                   countryCode = "AUS",
                   stateProvince = "Tasmania",
                   county = "Circular Head",
                   decimalLatitude = -41.1747208,
                   decimalLongitude = 144.9786072,
                   geodeticDatum = "EPSG:4326",
                   coordinateUncertaintyInMeters = 31,
                   coordinatePrecision = 0.00001,
                   verbatimLatitude = -41.1747208,
                   verbatimLongitude = 144.9786072,
                   verbatimCoordinateSystem = "decimal degrees",
                   verbatimSRS = "EPSG:4326")
  )


  # With both locality and country to county
  expect_equal(

    dwc_Location(longitude = 144.9786072,
                 latitude = -41.1747208,
                 verbatimCoordinateSystem = "decimal degrees",
                 verbatimSRS = "EPSG:4326",
                 localities_sf = locality_data_aus,
                 localities_names = "locality_name",
                 county_sf = county_tas),

    tibble::tibble(country = "Australia",
                   countryCode = "AUS",
                   stateProvince = "Tasmania",
                   county = "Circular Head",
                   locality = "13 km NNE Roger River",
                   decimalLatitude = -41.1747208,
                   decimalLongitude = 144.9786072,
                   geodeticDatum = "EPSG:4326",
                   coordinateUncertaintyInMeters = 31,
                   coordinatePrecision = 0.00001,
                   verbatimLatitude = -41.1747208,
                   verbatimLongitude = 144.9786072,
                   verbatimCoordinateSystem = "decimal degrees",
                   verbatimSRS = "EPSG:4326")
  )


})


test_that("dwc_Location error message works", {

  # Checks on decimal degrees -----------------------------------------------

  expect_error(dwc_Location(longitude = 144.9786072,
                             latitude = -41.1747208,
                             verbatimCoordinateSystem = "decimal degrees",
                             verbatimSRS = "EPSG:4326",
                             localities_sf = locality_data_aus,
                             county_sf = county_tas),
               "localities_names must be supplied if providing localities_sf")

})
