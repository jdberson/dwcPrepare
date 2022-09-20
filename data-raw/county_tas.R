## code to prepare `county_tas` dataset goes here

library("sf")
library("tidyverse")


county_tas_metadata <-
  tribble(
    ~dataset, ~source, ~attribution, ~license, ~url, ~download_link,

    "Non ABS Structures - ASGS Edition 3 - Local Government Areas - 2022 Shapefile",
    "Australian Bureau of Statistics",
    "Source: Australian Bureau of Statistics",
    "Creative Commons Attribution 2.5 Australia",
    "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files",
    "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2022_AUST_GDA2020_SHP.zip"

  )


# Download shape file -----------------------------------------------------

# Download
download.file(
  url = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2022_AUST_GDA2020_SHP.zip",
  destfile = "data-raw/LGA_2022_AUST_GDA2020_SHP.zip",
  mode = "wb")

# Unzip
unzip("data-raw/LGA_2022_AUST_GDA2020_SHP.zip",
      exdir = "data-raw/LGA_2022_AUST_GDA2020_SHP")

# Remove .zip folder
file.remove("data-raw/LGA_2022_AUST_GDA2020_SHP.zip")


# Load and process data ---------------------------------------------------

# Load data
LGA_2022_AUST_GDA2020 <-
  st_read("data-raw/LGA_2022_AUST_GDA2020_SHP")

# Check for invalid geometries
LGA_2022_AUST_GDA2020[!st_is_valid(LGA_2022_AUST_GDA2020), ] # none

# Check for empty geometries
LGA_2022_AUST_GDA2020[st_is_empty(LGA_2022_AUST_GDA2020), ]

# Remove empty geometries
LGA_2022_AUST_GDA2020 <-
  LGA_2022_AUST_GDA2020 %>%
  filter(!st_is_empty(geometry))

# Rename columns to Darwin Core terms
county_stateProvince_aus <-
  LGA_2022_AUST_GDA2020 %>%
  select(country = AUS_NAME21,
         countryCode = AUS_CODE21,
         stateProvince = STE_NAME21,
         county = LGA_NAME22)

# Change crs to what will most commonly be used for points
county_stateProvince_aus <-
  st_transform(county_stateProvince_aus, crs = st_crs("EPSG:4326"))

# File is too big for cran - filter to Tasmania only
county_tas <-
  county_stateProvince_aus %>%
  filter(stateProvince == "Tasmania") %>%
  filter(!county %in% c("King Island", "Flinders (Tas.)")) %>%
  st_simplify(dTolerance = 100)

# Plot
ggplot(data = county_tas) +
  geom_sf(aes(fill = county)) +
  theme(legend.position = "none")


# Remove downloaded data --------------------------------------------------

unlink("data-raw/LGA_2022_AUST_GDA2020_SHP/", recursive = TRUE)
unlink("data-raw/LGA_2022_AUST_GDA2020_SHP/", recursive = TRUE)


# Save data ---------------------------------------------------------------

usethis::use_data(county_tas, overwrite = TRUE)
