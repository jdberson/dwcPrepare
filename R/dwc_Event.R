#' Prepare Darwin Core Event fields
#'
#' This function takes a start date/date-time as well as an optional end
#' date/date-time as its main arguments and produces the Darwin Core Event
#' terms. If both a start and end are given the length of time the event took
#' place is assigned to the sampleSizeValue field. If only one of the start
#'  or end values have a time component, both values are treated as dates.
#' \code{dwc_Event} is essentially a wrapper for a series of
#' [lubridate](https://lubridate.tidyverse.org/) functions, and experienced R
#' users may prefer to use the lubridate functions directly.
#'
#' @param start The start date or date-time of the sampling event. Accepts
#' character strings with dates in one of the following formats: "dmy HMS"
#' (eg, "24/12/2012 15:45:00"), "ymd HMS" (eg, "2012-12-24 15:45:00"), "dmy"
#' (eg, "24/12/2012") or "ymd" (eg, "2012-12-24").
#' @param end The end date or date-time of the sampling event. Default is NA.
#' @param time_NA_value Where a mixture of dates/date-times occur, the dates
#' without times are often assigned a time of 00:00:00. This is the default
#' value here, and is used so that the eventDate field is shown correctly. We
#' suggest being cautious if changing the default.
#' @param tzone The timezone of the date-time passed to
#' \code{\link[lubridate]{parse_date_time}}. The default is "UTC".
#' @param sampleSizeUnit The unit of time for calculating the length of the
#' event. Passed to \code{\link[lubridate]{time_length}} as the \code{unit}
#' argument. The default is "day".
#' @param fieldNumber Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/fieldNumber}{fieldNumber} term. Default is
#' NA.
#' @param habitat Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/habitat}{habitat} term. Default is NA.
#' @param samplingProtocol Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/samplingProtocol}{samplingProtocol} term.
#' Default is NA.
#' @param samplingEffort Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/samplingEffort}{samplingEffort} term.
#' Default is NA.
#' @param fieldNotes Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/fieldNotes}{fieldNotes} term. Default is
#' NA.
#' @param eventRemarks Optional argument for what should be included in the
#' \href{http://rs.tdwg.org/dwc/terms/eventRemarks}{eventRemarks} term. Default
#' is NA.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with Darwin Core terms
#' \href{http://rs.tdwg.org/dwc/terms/fieldNumber}{fieldNumber} (if supplied),
#' \href{http://rs.tdwg.org/dwc/terms/eventDate}{eventDate},
#' \href{http://rs.tdwg.org/dwc/terms/startDayOfYear}{startDayOfYear},
#' \href{http://rs.tdwg.org/dwc/terms/endDayOfYear}{endDayOfYear},
#' \href{http://rs.tdwg.org/dwc/terms/year}{year},
#' \href{http://rs.tdwg.org/dwc/terms/month}{month},
#' \href{http://rs.tdwg.org/dwc/terms/day}{day},
#' \href{http://rs.tdwg.org/dwc/terms/verbatimEventDate}{verbatimEventDate},
#' \href{http://rs.tdwg.org/dwc/terms/habitat}{habitat} (if supplied),
#' \href{http://rs.tdwg.org/dwc/terms/samplingProtocol}{samplingProtocol}
#' (if supplied),
#' \href{http://rs.tdwg.org/dwc/terms/sampleSizeValue}{sampleSizeValue} (here
#' given as the length of time of the event, given if both the \code{start} and
#' \code{end} are given),
#' \href{http://rs.tdwg.org/dwc/terms/sampleSizeUnit}{sampleSizeUnit} (given if
#' both the \code{start} and \code{end} are given),
#' \href{http://rs.tdwg.org/dwc/terms/samplingEffort}{samplingEffort} (if
#' supplied),
#' \href{http://rs.tdwg.org/dwc/terms/fieldNotes}{fieldNotes} (if supplied) and
#' \href{http://rs.tdwg.org/dwc/terms/eventRemarks}{eventRemarks} (if supplied).
#'
#' @export
#'
#' @examples
#' # For a single start and a single end date-time:
#' dwc_Event(start = "2012-12-24 15:00:00", end = "2012-12-27 16:00:00")
#'
#' # The returned tibble will be added to an existing dataframe if combined with
#' #  dplyr's mutate, for example:
#'
#' # Load data
#' data("thylacine_data")
#'
#' # Use dwc_Event withe dplyr::mutate()
#' thylacine_data |>
#'   dplyr::mutate(dwc_Event(start = date_trap_setup,
#'                    end = date_trap_collected))
#'
dwc_Event <- function(
    start,
    end = NA,
    time_NA_value = "00:00:00",
    tzone = "UTC",
    sampleSizeUnit = "day",
    fieldNumber = NA,
    habitat = NA,
    samplingProtocol = NA,
    samplingEffort = NA,
    fieldNotes = NA,
    eventRemarks = NA){

  purrr::pmap_df(.l = list(
    start,
    end,
    time_NA_value,
    tzone,
    sampleSizeUnit,
    fieldNumber,
    habitat,
    samplingProtocol,
    samplingEffort,
    fieldNotes,
    eventRemarks),
    .f = dwc_Event_scalar) |>

    dplyr::select(tidyselect::where(
      ~!base::all(base::is.na(.x))
    ))

}


dwc_Event_scalar <- function(start,
                             end,
                             time_NA_value,
                             tzone,
                             sampleSizeUnit,
                             fieldNumber,
                             habitat,
                             samplingProtocol,
                             samplingEffort,
                             fieldNotes,
                             eventRemarks) {

  # If both a start and end date/datetime are given the interval information is
  # calculated
  if(!base::is.na(end)) {

    start_d <- lubridate::parse_date_time(start,
                                        c("dmy HMS", "ymd HMS", "dmy", "ymd"),
                                        tz = tzone)

    end_d <- lubridate::parse_date_time(end,
                                        c("dmy HMS", "ymd HMS", "dmy", "ymd"),
                                        tz = tzone)

    # If either the start of finish have NA time, assign both values to date
    # precision for eventDate and the event_interval
    if(
      hms::as_hms(start_d) == hms::as_hms(time_NA_value) ||
      hms::as_hms(end_d) == hms::as_hms(time_NA_value)
    ){

      event_interval <-
        lubridate::interval(lubridate::as_date(start_d),
                            lubridate::as_date(end_d))

      eventDate <- lubridate::format_ISO8601(event_interval, precision = "ymd")

      # Time level precision for the eventDate and the event_interval where both
      # are available
    } else{

      event_interval <-
        lubridate::interval(start_d, end_d)

      eventDate <- lubridate::format_ISO8601(event_interval,
                                             usetz = TRUE,
                                             precision = "ymdhm")

    }

    # Both start and finish shown in verbatimEventDate
    verbatimEventDate <- stringr::str_c(start, end, sep = " ")

    # Assign endDayOfYear as end
    endDayOfYear <- lubridate::yday(end_d)

    # Calculate the length of time that sampling took place
    sampleSizeValue <- lubridate::time_length(event_interval,
                                              unit = sampleSizeUnit)


    # If only the start date/datetime is given, we do not calculate the interval
    # information

  } else{

    start_d <- lubridate::parse_date_time(start,
                                          c("dmy HMS", "ymd HMS", "dmy", "ymd"),
                                          tz = tzone)

    if(
      hms::as_hms(start_d) == hms::as_hms(time_NA_value)
    ){

      eventDate <- lubridate::format_ISO8601(start_d,
                                             precision = "ymd")

    } else{

      eventDate <- lubridate::format_ISO8601(start_d,
        usetz = TRUE,
        precision = "ymdhm")

    }

    # Both start and finish shown in verbatimEventDate
    verbatimEventDate <- start

    # Assign endDayOfYear as start
    endDayOfYear <- lubridate::yday(start_d)

    # Assign NULL interval information
    sampleSizeValue <- NULL
    sampleSizeUnit <- NULL


  }

  # Return information as a tibble
  tibble::tibble(

    fieldNumber = fieldNumber,

    eventDate = eventDate,

    startDayOfYear = lubridate::yday(start_d),

    endDayOfYear = endDayOfYear,

    year = lubridate::year(start_d),

    month = lubridate::month(start_d),

    day = lubridate::day(start_d),

    verbatimEventDate = verbatimEventDate,

    habitat = habitat,

    samplingProtocol = samplingProtocol,

    sampleSizeValue = sampleSizeValue,

    sampleSizeUnit = sampleSizeUnit,

    samplingEffort = samplingEffort,

    fieldNotes = fieldNotes,

    eventRemarks = eventRemarks
  )

}
