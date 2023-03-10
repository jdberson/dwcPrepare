## code to prepare `thylacine_data` dataset goes here

library(tibble)
library(dplyr)

thylacine_data <-
  tibble::tribble(
    ~site, ~trap, ~date_trap_setup, ~date_trap_collected, ~longitude_dd, ~latitude_dd, ~longitude_dms, ~latitude_dms, ~longitude_ddm, ~latitude_ddm, ~species, ~count,
    "Sumac", "Sumac 1", "05/09/2022 10:32:00", "06/09/2022 12:24:00", 144.9786072, -41.1747208, "144 58 43.0 E", "41 10 29.0 S", "144 58.716432 E", "41 10.483248 S", "Thylacinus cynocephalus", 0,
    "Sumac", "Sumac 2", "05/09/2022 12:15:00", "06/09/2022 14:57:00", 144.9861145, -41.2080574, "144 59 10.0 E", "41 12 29.0 S", "144 59.166870 E", "41 12.483444 S", "Thylacinus cynocephalus", 0,
    "Sumac", "Sumac 1", "05/10/2022 08:23:00", "06/10/2022 0:00:00", 144.9786072, -41.1747208, "144 58 43.0 E", "41 10 29.0 S", "144 58.716432 E", "41 10.483248 S", "Thylacinus cynocephalus", 0,
    "Sumac", "Sumac 2", "05/10/2022 10:14:00", "06/10/2022 10:29:00", 144.9861145, -41.2080574, "144 59 10.0 E", "41 12 29.0 S", "144 59.166870 E", "41 12.483444 S", "Thylacinus cynocephalus", 0,
    "Picton", "Picton 1", "10/09/2022 10:32:00", "11/09/2022 12:24:00", 146.7033386, -43.2611122, "146 42 12.0 E", "43 15 40.0 S", "146 42.200316 E", "43 15.666732 S", "Thylacinus cynocephalus", 0,
    "Picton", "Picton 2", "10/09/2022 12:15:00", "11/09/2022 14:57:00", 146.6952820, -43.2330551, "146 41 43.0 E", "43 13 59.0 S", "146 41.716920 E", "43 13.983306 S", "Thylacinus cynocephalus", 0,
    "Picton", "Picton 1", "10/10/2022 0:00:00", "11/10/2022 08:46:00", 146.7033386, -43.2611122, "146 42 12.0 E", "43 15 40.0 S", "146 42.200316 E", "43 15.666732 S", "Thylacinus cynocephalus", 0,
    "Picton", "Picton 2", "10/10/2022 10:14:00", "11/10/2022 10:29:00", 146.6952820, -43.2330551, "146 41 43.0 E", "43 13 59.0 S", "146 41.716920 E", "43 13.983306 S", "Thylacinus cynocephalus", 0,
  ) |>

  mutate(gps_uncertainty = 30, .before = species)

usethis::use_data(thylacine_data, overwrite = TRUE)
