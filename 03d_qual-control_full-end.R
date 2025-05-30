## ---------------------------------------------------------- ##
# Quality Control Survey Data - Full WG Survey 3
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
end_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_full_end.csv"))

# Check structure
dplyr::glimpse(end_v1)

## ------------------------------------- ##

## ------------------------------------- ##







## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
end_v99 <- end_v1

# Export locally
write.csv(end_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_end.csv"))

# End ----
