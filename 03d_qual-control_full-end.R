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
# Remove Test Rows ----
## ------------------------------------- ##

# Remove known test rows or non-LTER group data
end_v2 <- end_v1 %>% 
  dplyr::filter(status != "Survey Preview") %>% 
  dplyr::filter(!synthesis_group %in% c("Admin Group", "Audacious",
                                        "Communicating with style",
                                        "Qualtrics")) %>% 
  # This survey is administered _after_ groups finish
  ## So no surveys before 2019 can be LTER-focused.
  ## Even the earliest cohort wouldn't be surveyed until 2019
  dplyr::filter(stringr::str_sub(string = start_date, 
                                 start = 1, end = 4) %in% c("2017", "2018") != T) %>% 
  # Drop any columns that are completely NA
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .))) %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# How many of each group?
supportR::count(end_v2$synthesis_group)
  
# Check lost columns
supportR::diff_check(old = names(end_v1), new = names(end_v2))

# Re-check structure
dplyr::glimpse(end_v2)

## ------------------------------------- ##
# Fill Missing Synthesis Groups ----
## ------------------------------------- ##

# Use start/end dates to identify WG when not provided
end_v3 <- end_v2 %>% 
  # Identify start year / start year + month
  dplyr::mutate(start_year = stringr::str_sub(start_date, 1, 4),
                start_y_m = stringr::str_sub(start_date, 1, 7)) %>% 
  # Fill empty characters with true NAs
  dplyr::mutate(synthesis_group = ifelse(nchar(synthesis_group) == 0, 
                                         yes = NA_character_, no = synthesis_group)) %>% 
  # Identify missing group names
  dplyr::mutate(synthesis_group = dplyr::case_when(
    start_y_m %in% c("2019-02", "2019-04") ~ "Metacommunities", # Last mtg = 11/5/2018
    start_y_m %in% c("2019-09") ~ "C2E", # Last mtg = 7/15/2019
    start_y_m %in% c("2019-10") ~ "Stream Cycling", # Last mtg = 9/11/2019
    T ~ synthesis_group)) %>% 
  # Identify cohort information too
  dplyr::mutate(cohort = case_when(
    # 2016
    synthesis_group %in% c("C2E", "Stream Cycling", "Metacommunities") ~ "2016",
    start_y_m == "2017-10" ~ "2016",
    # 2017
    synthesis_group %in% c("Synchrony", "Biodiversity Productivity", "SOM") ~ "2017",
    # 2020
    synthesis_group %in% c("River Si Exports", "EMERGENT", "Drought Effects") ~ "2020",
    # 2023
    synthesis_group %in% c("Ecosystem Transitions", "Plant Reproduction",
                           "Fire and Aridlands", "Marine Consumer Nutrient Dynamics",
                           "Producers-Consumers-Disturbance", "Selection Across Scales",
                           "Soil P Controls on C and N", "Pelagic Community Structure",
                           "Flux-Gradient") ~ "2023",
    # 2025
    synthesis_group %in% c("ResilienceandManagement", "CAGED") ~ "2025",
    T ~ synthesis_group), .before = synthesis_group) %>% 
  # Drop temp columns
  dplyr::select(-start_year, -start_y_m)

# Check responses / group
supportR::count(vec = end_v3$synthesis_group)

# Check timing of unknown groups
end_v3 %>% 
  dplyr::filter(is.na(synthesis_group)) %>% 
  dplyr::select(cohort, synthesis_group, start_date)

# And check everything has cohort info
supportR::num_check(data = end_v3, col = "cohort")















## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
end_v99 <- end_v1

# Export locally
write.csv(end_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_end.csv"))

# End ----
