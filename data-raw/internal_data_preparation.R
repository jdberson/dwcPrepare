## code to prepare data used internally by dwcPrepare functions goes here


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
  select(localityName = NAME)


# Write data --------------------------------------------------------------

usethis::use_data(locality_data_aus,
                  overwrite = TRUE, internal = TRUE)
