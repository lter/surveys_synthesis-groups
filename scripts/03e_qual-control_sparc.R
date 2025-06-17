## ---------------------------------------------------------- ##
# Quality Control Survey Data - SPARC Survey
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
sparc_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_sparc.csv"))

# Check structure
dplyr::glimpse(sparc_v1)

## ------------------------------------- ##
# Fill Missing Synthesis Groups ----
## ------------------------------------- ##

# Use start/end dates to identify WG when not provided
sparc_v2 <- sparc_v1 %>% 
  # Identify start year / start year + month
  dplyr::mutate(start_year = stringr::str_sub(start_date, 1, 4),
                start_y_m = stringr::str_sub(start_date, 1, 7)) %>% 
  # Fill empty characters with true NAs
  dplyr::mutate(synthesis_group = ifelse(nchar(synthesis_group) == 0, 
                                         yes = NA_character_, no = synthesis_group)) %>% 
  # Identify missing group names
  dplyr::mutate(synthesis_group = dplyr::case_when(
    
    T ~ synthesis_group)) %>% 
  # Identify cohort information too
  dplyr::mutate(cohort = case_when(
    # 2022
    synthesis_group %in% c("Fire and Aridlands", "Selection Across Scales",
                           "Producers-Consumers-Disturbance") ~ "2023",
    T ~ synthesis_group), .before = synthesis_group) %>% 
  # Drop temp columns
  dplyr::select(-start_year, -start_y_m)

# Check responses / group
supportR::count(vec = sparc_v2$synthesis_group)

# Check timing of unknown groups
sparc_v2 %>% 
  dplyr::filter(is.na(synthesis_group)) %>% 
  dplyr::select(cohort, synthesis_group, start_date)

# And check everything has cohort info
supportR::num_check(data = sparc_v2, col = "cohort")

# Full structure check
dplyr::glimpse(sparc_v2)

## ------------------------------------- ##
# Streamline Frequency Checkboxes ----
## ------------------------------------- ##

# Make sure that phrasing of importance/challenge answers is consistent
sparc_v2a <- sparc_v2 %>%
  # Fix the benefits columns in the same way (start by `pivot_longer`)
  tidyr::pivot_longer(cols = dplyr::starts_with("benefits_"), 
                      names_to = "benefits_qs",
                      values_to = "benefits_as") %>% 
  # Standardize these entries too
  dplyr::mutate(benefits_as = dplyr::case_when(
    benefits_as %in% c("no benefit at all") ~ "No benefit",
    benefits_as %in% c("slight benefit") ~ "Slight benefit",
    benefits_as %in% c("moderate benefit") ~ "Moderate benefit",
    benefits_as %in% c("great benefit") ~ "Great benefit",
    # benefits_as %in% c() ~ "",
    T ~ benefits_as))

# Check unique values
supportR::count(sparc_v2a$benefits_as)

# Do the same for time spent
sparc_v2b <- sparc_v2a %>% 
  # Pivot benefits back to wide format
  tidyr::pivot_wider(names_from = benefits_qs, values_from = benefits_as) %>% 
  # Pivot time columns into long format
  tidyr::pivot_longer(cols = dplyr::starts_with("time_"), 
                      names_to = "time_qs",
                      values_to = "time_as") %>% 
  # Standardize these entries too
  dplyr::mutate(time_as = dplyr::case_when(
    time_as %in% c("less than 10 hours (less than 30 min per month)") ~ "Less than 30 min/month",
    time_as %in% c("10-20 hours (30 min to 1 hour per month)",
                   "20-40 hours (1-2 hours per month)",
                   "10-40 hours (30 min to 2 hours per month)") ~ "30 min to 2 hr/month",
    time_as %in% c("40-80 hours (2-4 hours per month)",
                   "40-160 hours (2-6 hours per month)",
                   "80-160 hours (4-6 hours per month)") ~ "2-6 hr/month",
    time_as %in% c("more than 160 hours (more than 6 hours per month)") ~ "More than 6 hr/month",
    # time_as %in% c() ~ "",
    T ~ time_as))

# Check unique values
supportR::count(sparc_v2b$time_as)

# Do the same for support value
sparc_v2c <- sparc_v2b %>% 
  # Pivot benefits back to wide format
  tidyr::pivot_wider(names_from = time_qs, values_from = time_as) %>% 
  # Pivot time columns into long format
  tidyr::pivot_longer(cols = dplyr::starts_with("support_value_"), 
                      names_to = "support_qs",
                      values_to = "support_as") %>% 
  # Standardize these entries too
  dplyr::mutate(support_as = dplyr::case_when(
    support_as %in% c("did not help") ~ "Did not help",
    support_as %in% c("helped a little") ~ "Helped a little",
    support_as %in% c("helped somewhat") ~ "Helped somewhat",
    support_as %in% c("helped significantly") ~ "Helped significantly",
    # support_as %in% c() ~ "",
    T ~ support_as))

# Check unique values
supportR::count(sparc_v2c$support_as)

# Now flip back to wide format & standardize some other single-column questions
sparc_v3 <- sparc_v2c %>% 
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = support_qs, values_from = support_as) %>% 
  # Standardize 'satisfaction' entries
  dplyr::mutate(satisfaction_rating = dplyr::case_when(
    # satisfaction_rating %in% c() ~ "",
    T ~ satisfaction_rating)) %>% 
  # Standardize 'future_sparc_interest_self' entries
  dplyr::mutate(future_sparc_interest_self = dplyr::case_when(
    # future_sparc_interest_self %in% c("definitely yes") ~ "Extremely interested",
  #   future_sparc_interest_self %in% c("probably yes") ~ "Somewhat interested",
  #   future_sparc_interest_self %in% c("might or might not") ~ "Neither interested nor disinterested",
  #   # future_sparc_interest_self %in% c() ~ "",
    T ~ future_sparc_interest_self)) %>%
  # Standardize 'future_sparc_encourage_peer' entries
  dplyr::mutate(future_sparc_encourage_peer = dplyr::case_when(
  #   future_sparc_encourage_peer %in% c("extremely likely") ~ "Extremely likely",
  #   future_sparc_encourage_peer %in% c("quite likely") ~ "Quite likely",
  #   future_sparc_encourage_peer %in% c("somewhat likely") ~ "Somewhat likely",
  #   # future_sparc_encourage_peer %in% c() ~ "",
    T ~ future_sparc_encourage_peer)) %>%
  # Standardize 'anticipate_continuing_wg_research' entries
  dplyr::mutate(anticipate_continuing_wg_research = dplyr::case_when(
    anticipate_continuing_wg_research %in% c("definitely yes") ~ "Definitely yes",
    anticipate_continuing_wg_research %in% c("probably yes") ~ "Probably yes",
    # anticipate_continuing_wg_research %in% c() ~ "",
    T ~ anticipate_continuing_wg_research))

# Check unique values
supportR::count(sparc_v3$satisfaction_rating)
supportR::count(sparc_v3$future_sparc_interest_self)
supportR::count(sparc_v3$future_sparc_encourage_peer)
supportR::count(sparc_v3$anticipate_continuing_wg_research)

# Check structure
dplyr::glimpse(sparc_v3)

## ------------------------------------- ##
# Reorder / Streamline Columns ----
## ------------------------------------- ##

# Reorder the columns sensibly
sparc_v4 <- sparc_v3 %>%
  dplyr::select(survey_iteration, survey_type, start_date, end_date,
                program, cohort, synthesis_group, attendance_mode, 
                dplyr::starts_with("future_sparc_"),
                anticipate_continuing_wg_research,
                continue_plan_text,
                outcomes_achieved,
                dplyr::starts_with("satisfaction_"),
                dplyr::starts_with("challenges_"),
                dplyr::starts_with("time_"),
                dplyr::starts_with("benefits_"),
                dplyr::starts_with("support_"),
                feedback_text)

# Check which columns were lost
supportR::diff_check(old = names(sparc_v3), new = names(sparc_v4))

# Check structure
dplyr::glimpse(sparc_v4)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
sparc_v99 <- sparc_v4

# Export locally
write.csv(sparc_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_sparc.csv"))

# End ----
