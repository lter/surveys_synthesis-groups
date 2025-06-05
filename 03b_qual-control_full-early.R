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
librarian::shelf(tidyverse, supportR)

# Make needed sub-folder(s)
dir.create(file.path("data", "tidy"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Read in the data
early_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_full_first-meeting.csv"))

# Check structure
dplyr::glimpse(early_v1)

## ------------------------------------- ##
# Remove Test Rows ----
## ------------------------------------- ##

# Remove known test rows or non-LTER group data
early_v2 <- early_v1 %>% 
  dplyr::filter(status != "Survey Preview") %>% 
  dplyr::filter(!synthesis_group %in% c("Admin Group", "Audacious" ,
                                        "Communicating with style",
                                        "Grassland Birds", "MPA network assessment",
                                        "Plastic Pollution", "Qualtrics")) %>% 
  # Streamline CAGED respondent group IDs (some of them identified past group affiliations)
  dplyr::mutate(synthesis_group = ifelse(stringr::str_sub(start_date, 1, 4) == 2025 & 
                                           stringr::str_detect(string = synthesis_group, 
                                                               pattern = "CAGED, "),
                                         yes = "CAGED", no = synthesis_group))

# Re-check structure
dplyr::glimpse(early_v2)

# How many of each?
supportR::count(early_v2$synthesis_group)

## ------------------------------------- ##
# Fill Missing Synthesis Groups ----
## ------------------------------------- ##

# Use start/end dates to identify WG when not provided
early_v3 <- early_v2 %>% 
  # Identify start year / start year + month
  dplyr::mutate(start_year = stringr::str_sub(start_date, 1, 4),
                start_y_m = stringr::str_sub(start_date, 1, 7)) %>% 
  # Fill empty characters with true NAs
  dplyr::mutate(synthesis_group = ifelse(nchar(synthesis_group) == 0, 
                                         yes = NA_character_, no = synthesis_group)) %>% 
  # Fill missing 'program' info with "LTER"
  dplyr::mutate(program = ifelse(is.na(program), yes = "LTER", no = program)) %>% 
  # Start filling missing group IDs
  dplyr::mutate(synthesis_group = dplyr::case_when(
    start_y_m == "2016-12" ~ "", # Several 2016 groups met for the first time in December...
    start_y_m %in% c("2017-03", "2018-03") ~ "SOM",
    start_y_m == "2017-05" ~ "Synchrony",
    start_y_m == "2017-06" ~ "Biodiversity Productivity",
    T ~ synthesis_group)) %>% 
  # Identify cohort information too
  dplyr::mutate(cohort = case_when(
    # 2016
    synthesis_group %in% c("C2E", "Stream Cycling", "Metacommunities") ~ "2016",
    start_year == "2016" ~ "2016",
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
supportR::count(vec = early_v3$synthesis_group)

# And check everything has cohort info
supportR::num_check(data = early_v3, col = "cohort")

# General structure check
dplyr::glimpse(early_v3)

## ------------------------------------- ##
# Streamline Frequency Checkboxes ----
## ------------------------------------- ##

# Make sure that phrasing of importance/challenge answers is consistent
early_v3a <- early_v3 %>%
  # Pivot to long format so we only have one column to check
  tidyr::pivot_longer(cols = dplyr::starts_with("challenge_"), 
                      names_to = "challenge_qs",
                      values_to = "challenge_as") %>% 
  # Standardize entries (if needed & ignoring the free text responses)
  mutate(challenge_as = case_when(
    challenge_as %in% c("Not a challenge at all") ~ "Not a challenge",
    # challenge_as %in% c() ~ "",
    T ~ challenge_as))

# Check unique values
supportR::count(early_v3a$challenge_as)

# Do the same for benefits
early_v3b <- early_v3a %>% 
  # Pivot challenges back to wide format
  tidyr::pivot_wider(names_from = challenge_qs, values_from = challenge_as) %>%
  # Fix the benefits columns in the same way (start by `pivot_longer`)
  tidyr::pivot_longer(cols = dplyr::starts_with("benefits_"), 
                      names_to = "benefits_qs",
                      values_to = "benefits_as") %>% 
  # Standardize these entries too
  dplyr::mutate(benefits_as = dplyr::case_when(
    benefits_as %in% c("Not at all important",
                       "Not important at all") ~ "Not important",
    # benefits_as %in% c() ~ "",
    T ~ benefits_as))

# Check unique values
supportR::count(early_v3b$benefits_as)

# Do the same for 'time' columns
early_v3c <- early_v3b %>% 
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = benefits_qs, values_from = benefits_as) %>% 
  # Pivot time columns long
  tidyr::pivot_longer(cols = dplyr::starts_with("time_"), 
                      names_to = "time_qs",
                      values_to = "time_as") %>%
  # Standardize entries
  dplyr::mutate(time_as = dplyr::case_when(
    # time_as %in% c() ~ "",
    T ~ time_as))

# Check unique values
supportR::count(early_v3c$time_as)

# Finally, do the same for satisfaction questions
early_v4 <- early_v3c %>% 
  # Pivot challenges back to wide format
  tidyr::pivot_wider(names_from = time_qs, values_from = time_as) %>% 
  # Standardize 'satisfaction so far' entries
  dplyr::mutate(satisfaction_so_far = dplyr::case_when(
    # satisfaction_so_far %in% c() ~ ~ "",
    T ~ satisfaction_so_far))

# Check unique values
supportR::count(early_v4$satisfaction_so_far)

# Check structure
dplyr::glimpse(early_v4)

## ------------------------------------- ##
# Reorder / Streamline Columns ----
## ------------------------------------- ##

# Reorder the columns sensibly
early_v5 <- early_v4 %>%
  dplyr::select(survey_iteration, start_date, end_date,
                program, cohort, synthesis_group, attendance_mode, 
                expected_products_text, unique_skills_text,
                dplyr::starts_with("time_"),
                dplyr::starts_with("benefits_"),
                dplyr::starts_with("challenge_"),
                desired_training_text,
                dplyr::starts_with("satisfaction_"),
                feedback_text)

# Check which columns were lost
supportR::diff_check(old = names(early_v4), new = names(early_v5))

# Check structure
dplyr::glimpse(early_v5)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
early_v99 <- early_v5

# Export locally
write.csv(early_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_first-meeting.csv"))

# End ----
