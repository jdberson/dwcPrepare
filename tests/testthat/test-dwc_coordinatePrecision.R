test_that("dwc_coordinatePrecision works for single values", {
  # decimal degress
  expect_equal(
    dwc_coordinatePrecision(144.97, -41.174,
      verbatimCoordinateSystem = "decimal degrees"
    ),
    0.001
  )

  # degrees decimal minutes
  expect_equal(
    dwc_coordinatePrecision("144 58.71 E", "41 10.43 S",
      verbatimCoordinateSystem = "degrees decimal minutes"
    ),
    0.000166666667
  )

  # degrees minutes seconds
  expect_equal(
    dwc_coordinatePrecision("144 58 43.0 E", "41 10 29.0 S",
      verbatimCoordinateSystem = "degrees minutes seconds"
    ),
    0.00027777778
  )
})






test_that("dwc_coordinatePrecision works for multiple values", {
  data("thylacine_data")

  thylacine_data_extended_dd <-
    thylacine_data |>
    dplyr::mutate(dwc_coordinates(
      lon = longitude_dd,
      lat = latitude_dd,
      verbatimCoordinateSystem = "decimal degrees",
      verbatimSRS = "EPSG:4326"
    ))

  thylacine_data_extended_ddm <-
    thylacine_data |>
    dplyr::mutate(dwc_coordinates(
      lon = longitude_ddm,
      lat = latitude_ddm,
      verbatimCoordinateSystem = "degrees decimal minutes",
      verbatimSRS = "EPSG:4326"
    ))

  thylacine_data_extended_dms <-
    thylacine_data |>
    dplyr::mutate(dwc_coordinates(
      lon = longitude_dms,
      lat = latitude_dms,
      verbatimCoordinateSystem = "degrees minutes seconds",
      verbatimSRS = "EPSG:4326"
    ))

  # decimal degress
  expect_equal(
    thylacine_data_extended_dd |>
      dplyr::mutate(
        coordinatePrecision =
          dwc_coordinatePrecision(
            verbatimLongitude = verbatimLongitude,
            verbatimLatitude = verbatimLatitude,
            verbatimCoordinateSystem = verbatimCoordinateSystem
          )
      ) |>
      dplyr::pull(coordinatePrecision),
    base::rep(0.00001, 8)
  )

  # degrees decimal minutes
  expect_equal(
    thylacine_data_extended_ddm |>
      dplyr::mutate(
        coordinatePrecision =
          dwc_coordinatePrecision(
            verbatimLongitude = verbatimLongitude,
            verbatimLatitude = verbatimLatitude,
            verbatimCoordinateSystem = verbatimCoordinateSystem
          )
      ) |>
      dplyr::pull(coordinatePrecision),
    base::rep(0.00001, 8)
  )


  # degrees minutes seconds
  expect_equal(
    thylacine_data_extended_dms |>
      dplyr::mutate(
        coordinatePrecision =
          dwc_coordinatePrecision(
            verbatimLongitude = verbatimLongitude,
            verbatimLatitude = verbatimLatitude,
            verbatimCoordinateSystem = verbatimCoordinateSystem
          )
      ) |>
      dplyr::pull(coordinatePrecision),
    base::rep(0.00027777778, 8)
  )
})
