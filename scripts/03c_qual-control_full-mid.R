## ---------------------------------------------------------- ##
# Quality Control Survey Data - Full WG Survey 2
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
mid_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_full_mid-point.csv"))

# Check structure
dplyr::glimpse(mid_v1)

## ------------------------------------- ##
# Remove Test Rows ----
## ------------------------------------- ##

# Remove known test rows or non-LTER group data
mid_v2 <- mid_v1 %>% 
  dplyr::filter(status != "Survey Preview") %>% 
  dplyr::filter(!synthesis_group %in% c("Admin Group", "Audacious" ,
                                        "Communicating with style")) %>% 
  # Streamline CAGED respondent group IDs (some of them identified past group affiliations)
  dplyr::mutate(synthesis_group = ifelse(stringr::str_sub(start_date, 1, 4) == 2025 & 
                                           stringr::str_detect(string = synthesis_group, 
                                                               pattern = "CAGED, "),
                                         yes = "CAGED", no = synthesis_group))

# Re-check structure
dplyr::glimpse(mid_v2)

# How many of each?
supportR::count(mid_v2$synthesis_group)

## ------------------------------------- ##
# Fill Missing Synthesis Groups ----
## ------------------------------------- ##

# Use start/end dates to identify WG when not provided
mid_v3 <- mid_v2 %>% 
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
    start_y_m %in% c("2017-05", "2018-05") ~ "Synchrony",
    start_y_m == "2017-03" ~ "Metacommunities",
    start_y_m == "2018-02" ~ "Biodiversity Productivity",
    start_y_m %in% c("2018-11", "2018-12") ~ "SOM",
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
    # 2021
    synthesis_group %in% c("Ecosystem Transitions", "Plant Reproduction") ~ "2021",
    # 2023
    synthesis_group %in% c("Fire and Aridlands", "Marine Consumer Nutrient Dynamics",
                           "Producers-Consumers-Disturbance", "Selection Across Scales",
                           "Soil P Controls on C and N", "Pelagic Community Structure",
                           "Flux-Gradient") ~ "2023",
    # 2025
    synthesis_group %in% c("ResilienceandManagement", "CAGED") ~ "2025",
    T ~ synthesis_group), .before = synthesis_group) %>% 
  # Drop temp columns
  dplyr::select(-start_year, -start_y_m)

# Check responses / group
supportR::count(vec = mid_v3$synthesis_group)

# Check timing of unknown groups
## Oct. 2017 are both Synchrony & Stream Cycling
mid_v3 %>% 
  dplyr::filter(is.na(synthesis_group)) %>% 
  dplyr::select(cohort, synthesis_group, start_date)

# And check everything has cohort info
supportR::num_check(data = mid_v3, col = "cohort")

# General structure check
dplyr::glimpse(mid_v3)

## ------------------------------------- ##
# Streamline Frequency Checkboxes ----
## ------------------------------------- ##

# Make sure that phrasing of importance/challenge answers is consistent
mid_v3a <- mid_v3 %>%
  # Fix the benefits columns in the same way (start by `pivot_longer`)
  tidyr::pivot_longer(cols = dplyr::starts_with("benefits_"), 
                      names_to = "benefits_qs",
                      values_to = "benefits_as") %>% 
  # Standardize these entries too
  dplyr::mutate(benefits_as = dplyr::case_when(
    benefits_as %in% c("no benefit at all\nso far\n",
                       "Not at all important") ~ "No benefit",
    benefits_as %in% c("slight benefit\nso far",
                       "Slightly important") ~ "Slight benefit",
    benefits_as %in% c("moderate benefit\nso far",
                       "Moderately important") ~ "Moderate benefit",
    benefits_as %in% c("great benefit\nso far",
                       "Very important",
                       "Extremely important") ~ "Great benefit",
    # benefits_as %in% c() ~ "",
    T ~ benefits_as))

# Check unique values
supportR::count(mid_v3a$benefits_as)

mid_v3b <- mid_v3a %>% 
  # Pivot benefits back to wide format
  tidyr::pivot_wider(names_from = benefits_qs, values_from = benefits_as) %>% 
  # Pivot time columns into long format
  tidyr::pivot_longer(cols = dplyr::starts_with("time_spent_"), 
                      names_to = "time_qs",
                      values_to = "time_as") %>% 
  # Standardize these entries too
  dplyr::mutate(time_as = dplyr::case_when(
    time_as %in% c("\nMUCH MORE time than expected\nup to this point in the project") ~ "Much more time than expected",
    time_as %in% c("\nMORE time than expected\nup to this point in the project") ~ "More time than expected",
    time_as %in% c("\nABOUT THE EXPECTED AMOUNT of time\nup to this point in the project") ~ "About the expected amount of time",
    time_as %in% c("\nLESS time than expected\nup to this point in the project") ~ "Less time than expected",
    time_as %in% c("MUCH LESS time than expected\nup to this point in the project") ~ "Much less time than expected",
    time_as %in% c("NA as I did not attend last meeting") ~ "",
    # time_as %in% c() ~ "",
    T ~ time_as))

# Check unique values
supportR::count(mid_v3b$time_as)

# Finally, do the same for satisfaction questions
mid_v4 <- mid_v3b %>% 
  # Reshape time columns back to wide format
  tidyr::pivot_wider(names_from = time_qs, values_from = time_as) %>% 
  # Standardize 'satisfaction so far' entries
  dplyr::mutate(satisfaction_rating = dplyr::case_when(
    # satisfaction_rating %in% c() ~ ~ "",
    T ~ satisfaction_rating))

# Check unique values
supportR::count(mid_v4$satisfaction_rating)

# Check structure
dplyr::glimpse(mid_v4)

## ------------------------------------- ##
# Reorder / Streamline Columns ----
## ------------------------------------- ##

# Reorder the columns sensibly
mid_v5 <- mid_v4 %>%
  dplyr::select(survey_iteration, survey_type, start_date, end_date,
                program, cohort, synthesis_group, dplyr::starts_with("attendance_mtg_"), 
                expectations_evolve:challenges_remaining,
                dplyr::starts_with("time_"),
                dplyr::starts_with("benefits_"),
                dplyr::starts_with("satisfaction_"),
                expected_outcomes, feedback_text)

# Check which columns were lost
supportR::diff_check(old = names(mid_v4), new = names(mid_v5))

# Check structure
dplyr::glimpse(mid_v5)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
mid_v99 <- mid_v5

# Export locally
write.csv(mid_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_mid-point.csv"))

# End ----
