## ---------------------------------------------------------- ##
# Quality Control Survey Data - Full WG Survey 1
## ---------------------------------------------------------- ##

# Purpose:
## Do various QC on survey data
## Incl. standardizing response categories across survey variants, etc.

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Make needed sub-folder(s)
dir.create(file.path("data", "tidy"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Read in the data
early_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_full_first-meeting.csv"))

# Check structure
dplyr::glimpse(early_v1)

## ------------------------------------- ##

## ------------------------------------- ##







## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
early_v99 <- early_v1

# Export locally
write.csv(early_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_first-meeting.csv"))

# End ----
