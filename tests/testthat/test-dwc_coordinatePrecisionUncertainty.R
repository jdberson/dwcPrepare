test_that("coordinatePrecisionUncertainty works", {
  expect_equal(
    dwc_coordinatePrecisionUncertainty(coordinatePrecision = 0.01,
                                       decimalLatitude = 10.27), 1556.76476)
})

test_that("error is given", {
  expect_error(dwc_coordinatePrecisionUncertainty(coordinatePrecision = 0.01,
                                                  decimalLatitude = 95),
               regexp = "decimalLatitude must be between -90 and 90")
})
