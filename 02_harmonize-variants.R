## ---------------------------------------------------------- ##
# Harmonize Pre/Post-2020 Survey Variants
## ---------------------------------------------------------- ##

# Purpose:
## Use a data key to standardize column names between pre/post 2020 survey variants

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, ltertools)

# Make needed sub-folder(s)
dir.create(file.path("data", "raw"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Download Data Key ----
## ------------------------------------- ##

# Identify all files in relevant Drive folder
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IDI3xruhkmhq__uXa-p9fCwCDPSM_G9x")) %>% 
  # Subset to only this file
  dplyr::filter(name == "synthesis-group-survey_data-key")

# Check that
key_drive

# Download it
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

# Read it in and prepare it
survey_key <- read.csv(file = file.path("data", "synthesis-group-survey_data-key.csv")) %>% 
  ltertools::check_key(key = .)

# Check structure
dplyr::glimpse(survey_key)

## ------------------------------------- ##
# Harmonize Survey Variants ----
## ------------------------------------- ##





# End ----
