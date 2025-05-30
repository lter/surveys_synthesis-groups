## ---------------------------------------------------------- ##
# Quality Control Survey Data - Demographics
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
demo_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_demographic.csv"))

# Check structure
dplyr::glimpse(demo_v1)

## ------------------------------------- ##

## ------------------------------------- ##







## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
demo_v99 <- demo_v1

# Export locally
write.csv(demo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_demographic.csv"))

# End ----

