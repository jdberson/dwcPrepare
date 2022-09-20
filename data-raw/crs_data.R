## code to prepare `crs_data` dataset goes here

library("tidyverse")
library("rgdal")
library("sf")
library("dbplyr")


# Function that uses sf::st_crs to extract data from crs info
epsg_info <- function(code){
  tibble::tibble(
    epsg_code = stringr::str_c("EPSG:", code),
    name = sf::st_crs(code)$Name,
    semi_major_axis = base::as.numeric(sf::st_crs(code)$SemiMajor),
    inverse_flattening = sf::st_crs(code)$InvFlattening
  )
}

# Test function
epsg_info(4326)

# Obtain epsg codes for lon / lat coordinates
epsg <-
  rgdal::make_EPSG() %>%
  filter(str_detect(prj4, "longlat"))


# Obtain epsg info for all lon / lat epsg codes
crs_data <-
purrr::map_df(epsg$code, epsg_info)

# Save the data
usethis::use_data(crs_data, overwrite = TRUE)
