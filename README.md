
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dwcPrepare

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of dwcPrepare is to make it easier to prepare biodiversity data
that use [Darwin Core](https://dwc.tdwg.org/) terms.

Our current focus has been the Darwin Core
[Location](https://dwc.tdwg.org/terms/#location) terms. For users with
established data processing pipelines, the most useful functions are
likely to be `dwc_coordinateUncertaintyInMeters()`, which as the name
suggests, calculates the
[coordinateUncertaintyInMeters](http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters),
and the `dwc_locality()` function that provides text for the Darwin Core
[locality](http://rs.tdwg.org/dwc/terms/locality) term.

## Installation

You can install the development version of dwcPrepare from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jdberson/dwcPrepare")
```

## Getting started

All functions in the package start with **dwc\_** as the prefix,
generally followed by the name of the Darwin Core field. For example,
`dwc_coordinateUncertaintyInMeters()` will calculate the value for the
Darwin Core
[coordinateUncertaintyInMeters](http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters)
field.

Exceptions to this rule are utility functions (`dwc_format_gco()`,
`dwc_point_cluster()` and `dwc_polygon_cluster()`) or if a function
returns multiple Darwin Core fields (`dwc_coordinates()`,
`dwc_country_to_county()`, `dwc_Event()` and `dwc_Location()`).

The `dwc_Event()` and `dwc_Location()` functions provide wrappers that
can be used with `dplyr::mutate()` to generate all of the Darwin Core
[Event](https://dwc.tdwg.org/terms/#event) and
[Location](https://dwc.tdwg.org/terms/#location) terms supported by
`dwcPrepare`.

For example, using the toy dataset that comes shipped with the package:

``` r
# Load packages
library("dwcPrepare")
library("dplyr")
library("tibble")

# Load data
data("thylacine_data")

# Use dplyr::mutate() with dwc_Event() to generate Darwin Core Event fields
thylacine_data |>
  mutate(
    dwc_Event(
      start = date_trap_setup,
      end = date_trap_collected,
      tzone = "Australia/Hobart",
      samplingEffort = "1 trap"
    )
  )

# Use dplyr::mutate() with dwc_Location() to generate Darwin Core Location fields
thylacine_data |>
  mutate(
    dwc_Location(
      longitude = longitude_dd,
      latitude = latitude_dd,
      verbatimCoordinateSystem = "decimal degrees",
      verbatimSRS = "EPSG:4326",
      gps_uncertainty = gps_uncertainty,
      localities_sf = locality_data_aus,
      localities_names = "locality_name",
      county_sf = county_tas
    )
  )
```

See `vignette("dwcPrepare")` for more help with getting started.

## Citation

The package can be cited as:

Berson J, Manger J (2023). dwcPrepare: Helps Prepare Data for use in a
Darwin Core Archive. R package version 0.0.0.9000,
<https://github.com/jdberson/dwcPrepare>.

## Issues

If you find an error or a bug we would love to hear from you! Please let
us know what you have found by creating an issue at
<https://github.com/jdberson/dwcPrepare/issues>.
