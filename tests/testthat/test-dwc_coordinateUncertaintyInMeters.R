test_that("dwc_coordinateUncertaintyInMeters works for single values", {
  expect_equal(dwc_coordinateUncertaintyInMeters(decimalLatitude = -41.17472,
                                                 coordinatePrecision = 0.0001,
                                                 geodeticDatum = "EPSG:4326",
                                                 gps_uncertainty = 30), 44)
})

test_that("dwc_coordinateUncertaintyInMeters works for multiple values", {

  data("thylacine_data")
  thylacine_data_extended <-
    thylacine_data |>
    dplyr::mutate(dwc_coordinates(lon = longitude_dd,
                                  lat = latitude_dd,
                                  verbatimCoordinateSystem = "decimal degrees",
                                  verbatimSRS = "EPSG:4326")) |>
    dplyr::mutate(coordinatePrecision =
                    dwc_coordinatePrecision(verbatimLongitude = verbatimLongitude,
                                            verbatimLatitude = verbatimLatitude,
                                            verbatimCoordinateSystem = verbatimCoordinateSystem)) |>
    dplyr::mutate(coordinateUncertaintyInMeters = dwc_coordinateUncertaintyInMeters(decimalLatitude = decimalLatitude,
                                                                                    coordinatePrecision = coordinatePrecision,
                                                                                    geodeticDatum = geodeticDatum,
                                                                                    gps_uncertainty = gps_uncertainty))

  expect_equal(thylacine_data_extended |> dplyr::pull(coordinateUncertaintyInMeters),
               base::rep(31, 8))
})
