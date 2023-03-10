test_that("dwc_Event works for scalars", {


  # ymd format without times
  a <- tibble::tibble(
    eventDate = "2012-12-24/2012-12-27",
    startDayOfYear = 359,
    endDayOfYear = 362,
    year = 2012,
    month = 12,
    day = 24,
    verbatimEventDate = "2012-12-24 2012-12-27",
    sampleSizeValue = 3,
    sampleSizeUnit = "day")

  expect_equal(a, dwc_Event(start = "2012-12-24", end = "2012-12-27"))

  # dmy format without times
  b <- tibble::tibble(
    eventDate = "2012-12-24/2012-12-27",
    startDayOfYear = 359,
    endDayOfYear = 362,
    year = 2012,
    month = 12,
    day = 24,
    verbatimEventDate = "24/12/2012 27/12/2012",
    sampleSizeValue = 3,
    sampleSizeUnit = "day")

  expect_equal(b, dwc_Event(start = "24/12/2012", end = "27/12/2012"))

  # ymd format with times
  c <- tibble::tibble(
    eventDate = "2012-12-24T14:55+0000/2012-12-27T12:22+0000",
    startDayOfYear = 359,
    endDayOfYear = 362,
    year = 2012,
    month = 12,
    day = 24,
    verbatimEventDate = "2012-12-24 14:55:00 2012-12-27 12:22:00",
    sampleSizeValue = 2.89375,
    sampleSizeUnit = "day")

  expect_equal(c, dwc_Event(start = "2012-12-24 14:55:00",
                            end = "2012-12-27 12:22:00"))

  # dmy format with times
  d <- tibble::tibble(
    eventDate = "2012-12-24T14:55+0000/2012-12-27T12:22+0000",
    startDayOfYear = 359,
    endDayOfYear = 362,
    year = 2012,
    month = 12,
    day = 24,
    verbatimEventDate = "24/12/2012 14:55:00 27/12/2012 12:22:00",
    sampleSizeValue = 2.89375,
    sampleSizeUnit = "day")

  expect_equal(d, dwc_Event(start = "24/12/2012 14:55:00",
                            end = "27/12/2012 12:22:00"))



})

test_that("dwc_Event works with mutate and data frames", {

  # Create data frame
  a <- tibble::tribble(
    ~eventDate, ~startDayOfYear, ~endDayOfYear, ~year, ~month, ~day, ~verbatimEventDate, ~sampleSizeValue, ~sampleSizeUnit,
    "2022-09-05T10:32+0000/2022-09-06T12:24+0000", 248, 249, 2022, 9,  5,  "05/09/2022 10:32:00 06/09/2022 12:24:00", 1.07777777778, "day",
    "2022-09-05T12:15+0000/2022-09-06T14:57+0000", 248, 249, 2022, 9,  5,  "05/09/2022 12:15:00 06/09/2022 14:57:00", 1.112500, "day",
    "2022-10-05/2022-10-06",                       278, 279, 2022, 10, 5,  "05/10/2022 08:23:00 06/10/2022 0:00:00",  1.000000,    "day",
    "2022-10-05T10:14+0000/2022-10-06T10:29+0000", 278, 279, 2022, 10, 5,  "05/10/2022 10:14:00 06/10/2022 10:29:00", 1.01041666667, "day",
    "2022-09-10T10:32+0000/2022-09-11T12:24+0000", 253, 254, 2022, 9,  10, "10/09/2022 10:32:00 11/09/2022 12:24:00", 1.07777777778, "day",
    "2022-09-10T12:15+0000/2022-09-11T14:57+0000", 253, 254, 2022, 9,  10, "10/09/2022 12:15:00 11/09/2022 14:57:00", 1.112500, "day",
    "2022-10-10/2022-10-11",                       283, 284, 2022, 10, 10, "10/10/2022 0:00:00 11/10/2022 08:46:00",  1.000000,    "day",
    "2022-10-10T10:14+0000/2022-10-11T10:29+0000", 283, 284, 2022, 10, 10, "10/10/2022 10:14:00 11/10/2022 10:29:00", 1.01041666667, "day")

  # Use thylacine data to reproduce above
  data("thylacine_data")
  b <-
    thylacine_data |>
    dplyr::mutate(dwc_Event(start = date_trap_setup,
                            end = date_trap_collected)) |>
    dplyr::select(eventDate:sampleSizeUnit)

  expect_equal(a, b)

})
