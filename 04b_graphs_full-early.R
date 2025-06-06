# ---------------------------------------------------------- ##
# Graph Survey Data - Full WG (Early)
## ---------------------------------------------------------- ##

# Purpose:
## Make graphs for inclusion in the report for this survey
## Also likely useful for other LNO reports (e.g., annual report)

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Make needed sub-folder(s)
dir.create(file.path("graphs"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

# Read in the data
early_v1 <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_full_first-meeting.csv"))

# Check structure
dplyr::glimpse(early_v1)













# End ----
