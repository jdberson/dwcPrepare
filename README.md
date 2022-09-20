
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dwcPrepare

<!-- badges: start -->
<!-- badges: end -->

The goal of dwcPrepare is to make it easier to prepare biodiversity data
that use Darwin Core terms.

More specifically, dwcPrepare can help with georeferencing and
formatting date / date-time data.

dwcPrepare is under active development - please be aware that there may
be errors!

## Installation

You can install the development version of dwcPrepare from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jdberson/dwcPrepare")
```

## Getting started

First load the package:

``` r
library("dwcPrepare")
library("dplyr")
```

dwcPrepare comes with a small mock data set - Let’s load the data and
take a quick look.

``` r
data("thylacine_data")

thylacine_data
#> # A tibble: 8 × 13
#>   site   trap    date_…¹ date_…² longi…³ latit…⁴ longi…⁵ latit…⁶ longi…⁷ latit…⁸
#>   <chr>  <chr>   <chr>   <chr>     <dbl>   <dbl> <chr>   <chr>   <chr>   <chr>  
#> 1 Sumac  Sumac 1 05/09/… 06/09/…    145.   -41.2 144 58… 41 10 … 144 58… 41 10.…
#> 2 Sumac  Sumac 2 05/09/… 06/09/…    145.   -41.2 144 59… 41 12 … 144 59… 41 12.…
#> 3 Sumac  Sumac 1 05/10/… 06/10/…    145.   -41.2 144 58… 41 10 … 144 58… 41 10.…
#> 4 Sumac  Sumac 2 05/10/… 06/10/…    145.   -41.2 144 59… 41 12 … 144 59… 41 12.…
#> 5 Picton Picton… 10/09/… 11/09/…    147.   -43.3 146 42… 43 15 … 146 42… 43 15.…
#> 6 Picton Picton… 10/09/… 11/09/…    147.   -43.2 146 41… 43 13 … 146 41… 43 13.…
#> 7 Picton Picton… 10/10/… 11/10/…    147.   -43.3 146 42… 43 15 … 146 42… 43 15.…
#> 8 Picton Picton… 10/10/… 11/10/…    147.   -43.2 146 41… 43 13 … 146 41… 43 13.…
#> # … with 3 more variables: gps_uncertainty <dbl>, species <chr>, count <dbl>,
#> #   and abbreviated variable names ¹​date_trap_setup, ²​date_trap_collected,
#> #   ³​longitude_dd, ⁴​latitude_dd, ⁵​longitude_dms, ⁶​latitude_dms, ⁷​longitude_ddm,
#> #   ⁸​latitude_ddm
```

We want to provide some georeferencing information for this data, format
the data-time information and change column names to Darwin Core terms.

The fist step is to ensure we have all the information we need for
georeferencing. For this we use the function dwc_coordinates.

We need to tell the function which columns in our data contain the
longitude and latitude, as well as whether the coordinates are in
decimal degrees, degrees minutes seconds or degrees decimal minutes, and
the EPSG code for the coordinate reference system.

If the coordinate reference system is not known then “unknown” can be
used. However, we recommend trying to determine the coordinate reference
system as a value of “unknown” will likely increase the uncertainty
associated with the coordinates. See for more information.

``` r

# Note that we can use the mutate() function from the dyplr package to include
#  the new colimns with our data.
thylacine_data_1 <-
  thylacine_data |>
  mutate(dwc_coordinates(lon = longitude_dms, 
                         lat = latitude_dms, 
                         verbatimCoordinateSystem = "degrees minutes seconds", 
                         verbatimSRS = "EPSG:4326"))
```

The next step is to include the Darwin Core term coordinatePrecision. We
use the dwc_coordinatePrecision function to do this.

``` r
thylacine_data_2 <- 
thylacine_data_1 |>
  mutate(coordinatePrecision  = dwc_coordinatePrecision(
    verbatimLongitude = verbatimLongitude,
    verbatimLatitude = verbatimLatitude,
    precision = 1, 
    unit = "seconds"))
```

We now have all the information we need to calculate the uncertainty
associated with the geographic coordinates. This is given by the Darwin
Core term coordinateUncertaintyInMeters. We use the
dwc_coordinateUncertaintyInMeters function to calculate this.

``` r

thylacine_data_3 <-
  thylacine_data_2 |>
  mutate(coordinateUncertaintyInMeters = 
           dwc_coordinateUncertaintyInMeters(decimalLatitude = decimalLatitude,
                                             coordinatePrecision = coordinatePrecision,
                                             geodeticDatum = geodeticDatum,
                                             gps_uncertainty = gps_uncertainty))
```
