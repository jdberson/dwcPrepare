## code to prepare `locality_data_aus` dataset goes here

library("tibble")
library("dplyr")
library("stringr")
library("sf")

# locality_aus -------------------------------------------------------

locality_aus_metadata <-
  tribble(
    ~dataset, ~source, ~attribution, ~license, ~url, ~download_link,
    "Elvis - Place Names - Foundation Spatial Data - Locality Data",
    "ANZLIC Committee on Surveying & Mapping",
    "Source: Commonwealth of Australia (The Intergovernmental Committee on Surveying and Mapping, ICSM) 2018",
    "Creative Commons Attribution 4.0 International",
    "https://placenames.fsdf.org.au/",
    NA_character_
  )

# load and format data

locality_data_aus <-
  st_read("data-raw/locality_data_4273158/locality_data_4273158.json") %>%
  select(locality_name = NAME) |>
  # Correct variation in capitalisation of locality names
  mutate(locality_name = str_to_title(locality_name))


# Write data --------------------------------------------------------------

usethis::use_data(locality_data_aus, overwrite = TRUE)
