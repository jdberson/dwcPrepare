test_that("dwc_coordinates works", {
  # Test decimal degrees
  expect_equal(
    dwc_coordinates(
      longitude = 144.9786072,
      latitude = -41.1747208,
      verbatimCoordinateSystem = "decimal degrees",
      verbatimSRS = "EPSG:4326"
    ),
    tibble::tibble(
      decimalLatitude = -41.1747208,
      decimalLongitude = 144.9786072,
      geodeticDatum = "EPSG:4326",
      verbatimLatitude = -41.1747208,
      verbatimLongitude = 144.9786072,
      verbatimCoordinateSystem = "decimal degrees",
      verbatimSRS = "EPSG:4326"
    )
  )

  # Test degrees minutes seconds
  expect_equal(
    dwc_coordinates(
      longitude = "144 58 43.0 E",
      latitude = "41 10 29.0 S",
      verbatimCoordinateSystem = "degrees minutes seconds",
      verbatimSRS = "EPSG:4326"
    ),
    tibble::tibble(
      decimalLatitude = -41.1747208,
      decimalLongitude = 144.9786072,
      geodeticDatum = "EPSG:4326",
      verbatimLatitude = "41 10 29.0 S",
      verbatimLongitude = "144 58 43.0 E",
      verbatimCoordinateSystem = "degrees minutes seconds",
      verbatimSRS = "EPSG:4326"
    )
  )

  # Test degrees degrees decimal minutes
  expect_equal(
    dwc_coordinates(
      longitude = "144 58.716432 E",
      latitude = "41 10.483248 S",
      verbatimCoordinateSystem = "degrees decimal minutes",
      verbatimSRS = "EPSG:4326"
    ),
    tibble::tibble(
      decimalLatitude = -41.1747208,
      decimalLongitude = 144.9786072,
      geodeticDatum = "EPSG:4326",
      verbatimLatitude = "41 10.483248 S",
      verbatimLongitude = "144 58.716432 E",
      verbatimCoordinateSystem = "degrees decimal minutes",
      verbatimSRS = "EPSG:4326"
    )
  )
})
