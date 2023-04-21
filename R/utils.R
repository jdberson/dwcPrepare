# Function used by dwc_coordinatePrecision
dwc_precision_extractor <- function(x) {
  if (stringr::str_detect(base::as.character(x), "\\.")) {
    10^-(base::nchar(stringr::str_split_i(base::as.character(x), "\\.", 2)))
  } else {
    1
  }
}


# Function used by multiple exported functions to perform checks on supplied
# longitude and latitude
dwc_check_longitude_latitude <- function(longitude,
                                         latitude,
                                         verbatimCoordinateSystem) {
  # Check that all arguments are given
  if (base::missing(longitude)) {
    base::stop(
      '"longitude" is missing, please provide the longitude',
      call. = FALSE
    )
  }

  if (base::missing(latitude)) {
    base::stop(
      '"latitude" is missing, please provide the latitude',
      call. = FALSE
    )
  }

  if (base::missing(verbatimCoordinateSystem)) {
    base::stop(
      '"verbatimCoordinateSystem" is missing, please provide the
      verbatimCoordinateSystem',
      call. = FALSE
    )
  }

  # Check supplied value of verbatimCoordinateSystem
  if (!(verbatimCoordinateSystem %in%
    c(
      "decimal degrees", "degrees decimal minutes",
      "degrees minutes seconds"
    ))) {
    base::stop(
      'verbatimCoordinateSystem must be one of "decimal degrees",
      "degrees decimal minutes" or "degrees minutes seconds"',
      call. = FALSE
    )
  }


  # Check that longitude and latitude are in the correct form
  if (verbatimCoordinateSystem %in%
    c("degrees decimal minutes", "degrees minutes seconds") &&
    (!base::is.character(longitude) ||
      !base::is.character(latitude))) {
    base::stop(
      "longitude and latitude must both be strings or a character vectors",
      call. = FALSE
    )
  }



  if (verbatimCoordinateSystem == "decimal degrees" &&
    (!base::is.numeric(longitude) ||
      !base::is.numeric(latitude))) {
    base::stop(
      "longitude and latitude must be numeric",
      call. = FALSE
    )
  }



  # Check formatting of longitude and latitude
  if (verbatimCoordinateSystem == "degrees decimal minutes" &&
    (base::any(!stringr::str_detect(
      longitude,
      "\\d+\\s\\d+\\.\\d+\\s[EW]"
    )) ||
      base::any(!stringr::str_detect(
        latitude,
        "\\d+\\s\\d+\\.\\d+\\s[NS]"
      )))) {
    base::stop(
      'The supported format for coordinates in degrees decimal minutes is as
    per this example: "121 10.566667 W". Please update the coordinates. Note:
      if the precision of the minutes does not include digits after the
      decimal place, indicate this by including a 0 after the decimal place.',
      call. = FALSE
    )
  }



  if (verbatimCoordinateSystem == "degrees minutes seconds" &&
    (base::any(!stringr::str_detect(
      longitude,
      "\\d+\\s\\d+\\s\\d+\\.\\d+\\s[EW]"
    )) ||
      base::any(!stringr::str_detect(
        latitude,
        "\\d+\\s\\d+\\s\\d+\\.\\d+\\s[NS]"
      )))) {
    base::stop(
      'The supported format for coordinates in degrees minutes seconds is as
    per this example: "121 10 34.0 W". Please update the coordinates.Note:
      if the precision of the seconds does not include digits after the
      decimal place, indicate this by including a 0 after the decimal place.',
      call. = FALSE
    )
  }


  # Check longitude/latitude when in decimal degrees
  # (parzer does checks for other formats)
  if (verbatimCoordinateSystem == "decimal degrees") {
    if ((base::any(!longitude >= -180) || base::any(!longitude <= 180))) {
      base::stop(
        "longitude must be between -180 and 180",
        call. = FALSE
      )
    }

    if ((base::any(!latitude >= -90) || base::any(!latitude <= 90))) {
      base::stop(
        "latitude must be between -90 and 90",
        call. = FALSE
      )
    }
  }
}
