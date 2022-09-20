test_that("dwc_formatGeoreferenceCalculatorOutput works", {

  dwc_formatGeoreferenceCalculatorOutputTestData1 <-
  tibble::tribble(
    ~locationIdentifier, ~decimalLatitude,	~decimalLongitude,	~geodeticDatum,
    ~coordinateUncertaintyInMeters,	~coordinatePrecision,
    ~georeferencedBy, ~gereferenceDate,	~georeferenceProtocol,

    "A", "10.27", "-123.6", "(WGS84) World Geodetic System 1984", "1557",
    "1e-07", "Joe Bloggs", "2022-08-31T12:19:26.466Z",
    "Georeferencing Quick Reference Guide. 2020") |>
    dplyr::mutate(
      dplyr::across(.cols = tidyselect::contains(c("decimal", "coordinate")),
                    ~base::as.numeric(.x)))


  dwc_formatGeoreferenceCalculatorOutputTestData2 <-
    dwc_formatGeoreferenceCalculatorOutput("A",
                                           "10.27	-123.6	(WGS84) World Geodetic System 1984	1557	0.0000001	Joe Bloggs	2022-08-31T12:19:26.466Z	Georeferencing Quick Reference Guide. 2020")


  expect_equal(dwc_formatGeoreferenceCalculatorOutputTestData1,
               dwc_formatGeoreferenceCalculatorOutputTestData2)
})
