# ---------------------------------------------------------- ##
# Graph Survey Data - All Full WG Surveys
## ---------------------------------------------------------- ##

# Purpose:
## Make graphs that show patterns across time *within cohort*

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
combo <- read.csv(file = file.path("data", "tidy", 
                                   "wg-survey-tidy_full_all-three-surveys.csv")) %>% 
  # Make empty cells into true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(is.na(.) | nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(combo)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-04_full-wg-all-surveys_"

## ------------------------------------- ##
# Satisfaction ----
## ------------------------------------- ##


## ------------------------------------- ##
# Expectation Evolution ----
## ------------------------------------- ##




## ------------------------------------- ##
# Attendance ----
## ------------------------------------- ##


## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##



## ------------------------------------- ##
# Challenges ----
## ------------------------------------- ##



# End ----
