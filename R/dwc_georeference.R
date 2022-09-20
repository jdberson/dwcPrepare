# Wrapper for

dwc_georeference <- function(locationID,
                             lon,
                             lat,
                             verbatimCoordinateSystem = "decimal degrees",
                             verbatimSRS = "EPSG:4326",
                             precision = NULL,
                             unit = NULL,
                             gps_uncertainty = 30,
                             georeferencedBy = NA_character_,
                             georeferenceProtocol = NA_character_){

  # Use dwc_coordinates to prepare data for georeferencing
  dwc_coordinates(locationID = locationID,
                  lon = lon,
                  lat = lat,
                  verbatimCoordinateSystem = verbatimCoordinateSystem,
                  verbatimSRS = verbatimSRS) |>

    # Use dwc_coordinatePrecision to calculate / set the coordinate precision
    dplyr::mutate(
      coordinatePrecision  =
        dwc_coordinatePrecision(
          verbatimLongitude = verbatimLongitude,
          verbatimLatitude = verbatimLatitude,
          precision = precision,
          unit = unit)
    ) |>

    # Use dwc_coordinateUncertaintyInMeters to calculate the coordinate
    # uncertainty
    dplyr::mutate(
      coordinateUncertaintyInMeters =
        dwc_coordinateUncertaintyInMeters(
          decimalLatitude = decimalLatitude,
                                          coordinatePrecision = coordinatePrecision,
                                          geodeticDatum = geodeticDatum,
                                          gps_uncertainty = gps_uncertainty)
    ) |>

    # Optionally include georeferencedBy and georeferenceProtocol
    dplyr::mutate(
      georeferencedBy = georeferencedBy,
      georeferenceProtocol = georeferenceProtocol
    )

}
