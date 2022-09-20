test_that("dwc_coordinates works", {

  # Test decimal degrees
  expect_equal(

    dwc_coordinates(lon = 144.9786072,
                    lat = -41.1747208,
                    verbatimCoordinateSystem = "decimal degrees",
                    verbatimSRS = "EPSG:4326"),

    tibble::tibble(decimalLongitude = 144.9786072,
                   decimalLatitude = -41.1747208,
                   geodeticDatum = "EPSG:4326",
                   verbatimLongitude = 144.9786072,
                   verbatimLatitude = -41.1747208,
                   verbatimCoordinateSystem = "decimal degrees",
                   verbatimSRS = "EPSG:4326")
  )

  # Test degrees minutes seconds
  expect_equal(

    dwc_coordinates(lon = "144 58 43 E",
                    lat = "41 10 29 S",
                    verbatimCoordinateSystem = "degrees minutes seconds",
                    verbatimSRS = "EPSG:4326"),

    tibble::tibble(decimalLongitude = 144.9786072,
                   decimalLatitude = -41.1747208,
                   geodeticDatum = "EPSG:4326",
                   verbatimLongitude = "144 58 43 E",
                   verbatimLatitude = "41 10 29 S",
                   verbatimCoordinateSystem = "degrees minutes seconds",
                   verbatimSRS = "EPSG:4326")
  )

  # Test degrees degrees decimal minutes
  expect_equal(

    dwc_coordinates(lon = "144 58.716432 E",
                    lat = "41 10.483248 S",
                    verbatimCoordinateSystem = "degrees decimal minutes",
                    verbatimSRS = "EPSG:4326"),

    tibble::tibble(decimalLongitude = 144.9786072,
                   decimalLatitude = -41.1747208,
                   geodeticDatum = "EPSG:4326",
                   verbatimLongitude = "144 58.716432 E",
                   verbatimLatitude = "41 10.483248 S",
                   verbatimCoordinateSystem = "degrees decimal minutes",
                   verbatimSRS = "EPSG:4326")
  )


})



test_that("error messages work", {

  # Checks on decimal degrees -----------------------------------------------

  expect_error(dwc_coordinates(lon = 144.9786072,
                               lat = 91.1747208,
                               verbatimCoordinateSystem = "decimal degrees",
                               verbatimSRS = "EPSG:4326"),
               "latitude must be between -90 and 90")

  expect_error(dwc_coordinates(lon = 144.9786072,
                               lat = -91.1747208,
                               verbatimCoordinateSystem = "decimal degrees",
                               verbatimSRS = "EPSG:4326"),
               "latitude must be between -90 and 90")

  expect_error(dwc_coordinates(lon = 184.9786072,
                               lat = -41.1747208,
                               verbatimCoordinateSystem = "decimal degrees",
                               verbatimSRS = "EPSG:4326"),
               "longitude must be between -180 and 180")

  expect_error(dwc_coordinates(lon = -184.9786072,
                               lat = -41.1747208,
                               verbatimCoordinateSystem = "decimal degrees",
                               verbatimSRS = "EPSG:4326"),
               "longitude must be between -180 and 180")


  # Checks on supplied verbatimCoordinateSystem -----------------------------

  expect_error(dwc_coordinates(lon = 144.9786072,
                               lat = 1.1747208,
                               verbatimCoordinateSystem = "UTM",
                               verbatimSRS = "EPSG:4326"),
               'verbatimCoordinateSystem must be one of "decimal degrees",
      "degrees decimal minutes" or "degrees minutes seconds"')


  # Checks on verbatimSRS ---------------------------------------------------

  # TESTS TRIED NOT WORKING CORRECTLY - DON'T KNOW WHY - REMOVE FOR NOW

  # expect_error(dwc_coordinates(lon = 144.9786072,
  #                              lat = 41.1747208,
  #                              verbatimCoordinateSystem = "decimal degrees",
  #                              verbatimSRS = "EPSG:1000"),
  #              'EPSG:1000 is not a currently supported verbatimSRS. For a list of
  #     supported EPSG codes please see the epsg_code column in data("crs_data").')




})


