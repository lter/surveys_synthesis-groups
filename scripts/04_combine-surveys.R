## ---------------------------------------------------------- ##
# Combine Surveys
## ---------------------------------------------------------- ##

# Purpose:
## Some questions are asked in multiple surveys
## By combining survey data, we can graph these to more explicitly identify change over time
### Not just across cohorts (as is done by within-survey graphs)
### But within them as well

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Ingest Needed Data ----
## ------------------------------------- ##

# Read in all tidy data
combo_v1 <- purrr::map(.x = dir(path = file.path("data", "tidy")),
                       .f = ~ read.csv(file.path("data", "tidy", .x))) %>% 
  # Collapse into a df
  purrr::list_rbind(x = .) %>% 
  # Remove SPARC/demographic surveys and any completely empty columns
  dplyr::filter(!survey_type %in% c("demographic", "sparc")) %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))

# Check structure
dplyr::glimpse(combo_v1)

## ------------------------------------- ##
# Streamline Columns ----
## ------------------------------------- ##

# Pare down to only needed columns
combo_v2 <- combo_v1 %>% 
  dplyr::select(survey_iteration:synthesis_group,
                satisfaction_rating, expectations_evolve,
                dplyr::starts_with("attendance_mtg_"),
                dplyr::starts_with("benefits_"),
                dplyr::starts_with("challenge_")) %>% 
  # And drop free text answers
  dplyr::select(-dplyr::ends_with(c("_other", "other_text")))
  
# Check what columns are gained/lost
supportR::diff_check(old = names(combo_v1), new = names(combo_v2))

# Check structure
dplyr::glimpse(combo_v2)

## ------------------------------------- ##
# Tidy 'Survey Type' ----
## ------------------------------------- ##

# Check current survey types
supportR::count(vec = combo_v2$survey_type)

# Tweak these to be better for graphing
combo_v3 <- combo_v2 %>% 
  dplyr::mutate(survey_type = dplyr::case_when(
    survey_type == "full_first-meeting" ~ "Beginning",
    survey_type == "full_mid-point" ~ "Middle",
    survey_type == "full_end" ~ "End"))

# Check resulting survey types
supportR::count(vec = combo_v3$survey_type)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
combo_v99 <- combo_v3

# Export locally
write.csv(combo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_all-three-surveys.csv"))

# End ----
