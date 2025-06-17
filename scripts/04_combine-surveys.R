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
combo_v1 <- purrr::map(.x = setdiff(dir(path = file.path("data", "tidy")),
                                    "wg-survey-tidy_full_all-three-surveys.csv"),
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
                dplyr::all_of(paste0("attendance_mtg_", 1:4)),
                dplyr::starts_with("benefits_")) %>% 
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
# Tidy 'Attendance' ----
## ------------------------------------- ##

# Check current attendance entries
combo_v3 %>% 
  dplyr::select(dplyr::starts_with("attendance_")) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("attendance_")) %>% 
  pull(value) %>% 
  supportR::count(vec = .)
  
# Do needed tidying/standardization
combo_v4 <- combo_v3 %>% 
  # Make empty cells into true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("attendance_"),
                              .fns = ~ ifelse(is.na(.) | nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # Fill missing 'meeting 3' with 'meeting 4' data
  dplyr::mutate(attendance_mtg_3 = ifelse(is.na(attendance_mtg_3), 
                                          yes = attendance_mtg_4,
                                          no = attendance_mtg_3)) %>% 
  # Standardize attendance mode entries
  dplyr::mutate(
    dplyr::across(.cols = dplyr::starts_with("attendance_"),
                  .fns = ~ dplyr::case_when(
                    stringr::str_detect(string = tolower(.), 
                                        pattern = "in-person") ~ "In-Person",
                    . %in% c("Virtual", "via videoconference") ~ "Virtual",
                    stringr::str_detect(string = tolower(.), 
                                        pattern = "did not participate") ~ "Absent",
                    T ~ .)) )

# Re-check attendance entries
combo_v4 %>% 
  dplyr::select(dplyr::starts_with("attendance_")) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("attendance_")) %>% 
  pull(value) %>% 
  supportR::count(vec = .)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
combo_v99 <- combo_v4

# Export locally
write.csv(combo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_all-three-surveys.csv"))

# End ----
