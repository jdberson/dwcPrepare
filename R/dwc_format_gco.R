#' Format the output of \url{http://georeferencing.org/georefcalculator/gc.html}
#'
#' The \url{http://georeferencing.org/georefcalculator/gc.html} georeferencing
#' calculator allows for the results to be output by selecting 'Copy' and then
#' pasted into a spreadsheet. This function takes the output as an argument and
#' formats it as a \code{\link[tibble]{tibble}}.
#'
#' @param location_identifier The name of the location. Used so that the
#' returned \code{\link[tibble]{tibble}} can be joined with other data.
#' @param georeference_calculator_output The output of the georeferencing
#' calculator at \url{http://georeferencing.org/georefcalculator/gc.html}.
#'
#' @return
#' A \code{\link[tibble]{tibble}}. Column names are Darwin Core terms.
#' @export
#'
#' @examples
#' # Note that the georeference_calculator_output has been truncated for this
#' # example
#' dwc_format_gco(
#' location_identifier = "A",
#' georeference_calculator_output =
#' "10.27	-123.6	WGS84	1557	0.0000001	JB	2022-08-31	Georeferencing...")
dwc_format_gco <-
  function(location_identifier, georeference_calculator_output) {

    a <-
      base::as.character(
        c(location_identifier,
          stringr::str_split(georeference_calculator_output,
                                                 "\t", simplify = TRUE))
      )

    base::names(a) <-
      c("location_identifier", "decimalLatitude",	"decimalLongitude",
        "geodeticDatum", "coordinateUncertaintyInMeters",	"coordinatePrecision",
        "georeferencedBy", "gereferenceDate",	"georeferenceProtocol")

    a |>
      tibble::enframe() |>
      tidyr::pivot_wider() |>
      dplyr::mutate(
        dplyr::across(.cols = tidyselect::contains(c("decimal", "coordinate")),
                      ~base::as.numeric(.x)))
  }
