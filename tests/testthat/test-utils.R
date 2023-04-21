test_that("dwc_precision_extractor works", {
  expect_equal(dwc_precision_extractor(0.2356), 0.0001)

  expect_equal(dwc_precision_extractor(2500.23556), 0.00001)

  expect_equal(dwc_precision_extractor(1010), 1)
})



test_that("dwc_check_longitude_latitude works", {
  # Checks on decimal degrees -----------------------------------------------

  expect_error(
    dwc_check_longitude_latitude(
      longitude = 144.9786072,
      latitude = 91.1747208,
      verbatimCoordinateSystem = "decimal degrees"
    ),
    "latitude must be between -90 and 90"
  )

  expect_error(
    dwc_check_longitude_latitude(
      longitude = 144.9786072,
      latitude = -91.1747208,
      verbatimCoordinateSystem = "decimal degrees"
    ),
    "latitude must be between -90 and 90"
  )

  expect_error(
    dwc_check_longitude_latitude(
      longitude = 184.9786072,
      latitude = -41.1747208,
      verbatimCoordinateSystem = "decimal degrees"
    ),
    "longitude must be between -180 and 180"
  )

  expect_error(
    dwc_check_longitude_latitude(
      longitude = -184.9786072,
      latitude = -41.1747208,
      verbatimCoordinateSystem = "decimal degrees"
    ),
    "longitude must be between -180 and 180"
  )


  # Checks on supplied verbatimCoordinateSystem -----------------------------

  expect_error(
    dwc_check_longitude_latitude(
      longitude = 144.9786072,
      latitude = 1.1747208,
      verbatimCoordinateSystem = "UTM"
    ),
    'verbatimCoordinateSystem must be one of "decimal degrees",
      "degrees decimal minutes" or "degrees minutes seconds"'
  )
})
