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
librarian::shelf(tidyverse, supportR)

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

# Full structure check
dplyr::glimpse(end_v3)

## ------------------------------------- ##
# Streamline Frequency Checkboxes ----
## ------------------------------------- ##

# Make sure that phrasing of importance/challenge answers is consistent
end_v3a <- end_v3 %>%
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
supportR::count(end_v3a$benefits_as)

# Do the same for time spent
end_v3b <- end_v3a %>% 
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
supportR::count(end_v3b$time_as)

# Do the same for support value
end_v3c <- end_v3b %>% 
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
supportR::count(end_v3c$support_as) %>% 
  # Removing a nice commet about Nick for legibility
  dplyr::filter(stringr::str_detect(string = value, pattern = "Lyon") != T)

# Do the same for support value
end_v3c <- end_v3b %>% 
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
supportR::count(end_v3c$support_as) %>% 
  # Removing a nice commet about Nick for legibility
  dplyr::filter(stringr::str_detect(string = value, pattern = "Lyon") != T)

# Do the same for unmet expectations
end_v3d <- end_v3c %>% 
  # Pivot benefits back to wide format
  tidyr::pivot_wider(names_from = support_qs, values_from = support_as) %>% 
  # Pivot time columns into long format
  tidyr::pivot_longer(cols = dplyr::starts_with("expec_unmet_"), 
                      names_to = "expec_qs",
                      values_to = "expec_as") %>% 
  # Tidy up 'question' column
  dplyr::mutate(expec_qs = gsub(pattern = paste0("_", 1:4, "_", collapse = "|"),
                                replacement = "_", x = expec_qs)) %>% 
  # Standardize the answer entries too
  dplyr::mutate(expec_as = dplyr::case_when(
    expec_as %in% c("NO, this was not a question of needing additional support") ~ "All support irrelevant",
    expec_as %in% c("NO, the additional support that was needed is not the kind of support that the NCO can provide") ~ "LNO support irrelevant",
    expec_as %in% c("YES, additional support from the NCO could have helped us to achieve this outcome") ~ "LNO support would have helped",
    expec_as %in% c("Additional support in this area would not have helped at all to achieve this expected outcome.") ~ "Wouldn't have helped",
    expec_as %in% c("Additional support in this area may have helped somewhat to achieve this expected outcome.") ~ "Would have been somewhat helpful",
    expec_as %in% c("Additional support in this area most likely would have helped to achieve this expected outcome.") ~ "Would likely have been helpful",
    expec_as %in% c("Additional support in this area would certainly have helped to achieve this expected outcome.") ~ "Certainly would have been helpful",
    # expec_as %in% c() ~ "",
    T ~ expec_as)) %>% 
  # Collapse 1:4 'unmet expectations' into single values
  dplyr::distinct() %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("expec_as"))))) %>% 
  dplyr::summarize(expec_as = paste0(expec_as, collapse = "; "),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
# Tidy answers resulting from that
dplyr::mutate(expec_as = gsub(pattern = "; NA", replacement = "", x = expec_as)) %>% 
  dplyr::mutate(expec_as = dplyr::case_when(
    expec_as == "NA" ~ NA,
    expec_as %in% c("Certainly would have been helpful; Would likely have been helpful") ~ "Certainly would have been helpful",
    expec_as %in% c("All support irrelevant; LNO support would have helped") ~ "LNO support would have helped",
    expec_as %in% c("All support irrelevant; LNO support irrelevant") ~ "LNO support irrelevant",
    expec_as %in% c("Would have been somewhat helpful; Wouldn't have helped") ~ "Would have been somewhat helpful",
    # expec_as %in% c() ~ "",
    T ~ expec_as))
  
# Check unique values
unique(end_v3d$expec_as)

# Finally, tidy up attendance modality info
end_v3e <- end_v3d %>% 
  # Pivot benefits back to wide format
  tidyr::pivot_wider(names_from = expec_qs, values_from = expec_as) %>% 
  # Pivot time columns into long format
  tidyr::pivot_longer(cols = dplyr::starts_with("attendance_mtg_"), 
                      names_to = "attend_qs",
                      values_to = "attend_as") %>% 
  # Standardize these entries too
  dplyr::mutate(attend_as = dplyr::case_when(
    attend_as %in% c("I attended this meeting in person.") ~ "In-person",
    attend_as %in% c("I did not attend this meeting in person, but I participated remotely.") ~ "Virtual",
    attend_as %in% c("I did not participate in this meeting.") ~ "Did not participate",
    # attend_as %in% c() ~ "",
    T ~ attend_as))

# Check unique values
supportR::count(end_v3e$attend_as)

# Now flip back to wide format & standardize some other single-column questions
end_v4 <- end_v3e %>% 
  # Reshape time columns back to wide format
  tidyr::pivot_wider(names_from = attend_qs, values_from = attend_as) %>% 
  # Standardize 'satisfaction' entries
  dplyr::mutate(satisfaction_rating = dplyr::case_when(
    # satisfaction_rating %in% c() ~ "",
    T ~ satisfaction_rating)) %>% 
  # Standardize 'future_wg_interest_self' entries
  dplyr::mutate(future_wg_interest_self = dplyr::case_when(
    future_wg_interest_self %in% c("definitely yes") ~ "Extremely interested",
    future_wg_interest_self %in% c("probably yes") ~ "Somewhat interested",
    future_wg_interest_self %in% c("might or might not") ~ "Neither interested nor disinterested",
    # future_wg_interest_self %in% c() ~ "",
    T ~ future_wg_interest_self)) %>% 
  # Standardize 'future_wg_encourage_peer' entries
  dplyr::mutate(future_wg_encourage_peer = dplyr::case_when(
    future_wg_encourage_peer %in% c("extremely likely") ~ "Extremely likely",
    future_wg_encourage_peer %in% c("quite likely") ~ "Quite likely",
    future_wg_encourage_peer %in% c("somewhat likely") ~ "Somewhat likely",
    # future_wg_encourage_peer %in% c() ~ "",
    T ~ future_wg_encourage_peer)) %>% 
  # Standardize 'anticipate_continuing_wg_research' entries
  dplyr::mutate(anticipate_continuing_wg_research = dplyr::case_when(
    anticipate_continuing_wg_research %in% c("definitely yes") ~ "Definitely yes",
    anticipate_continuing_wg_research %in% c("probably yes") ~ "Probably yes",
    # anticipate_continuing_wg_research %in% c() ~ "",
    T ~ anticipate_continuing_wg_research)) %>% 
  # Standardize 'asset_used' entries
  dplyr::mutate(asset_used = dplyr::case_when(
    asset_used == as.character(1) ~ "Extremely poorly-utilized",
    asset_used %in% as.character(2:3) ~ "Poorly utilized",
    asset_used == as.character(4) ~ "Neither well- nor poorly-utilized",
    asset_used %in% as.character(5:6) ~ "Moderately well-utilized",
    asset_used == as.character(7) ~ "Extremely well-utilized",
    # asset_used %in% c() ~ "",
    T ~ asset_used)) %>% 
  # Standardize 'expectations_evolve' entries
  dplyr::mutate(expectations_evolve = dplyr::case_when(
    expectations_evolve %in% c("Dramatic change") ~ "Substantial change",
    # expectations_evolve %in% c() ~ "",
    T ~ expectations_evolve))

# Check unique values
supportR::count(end_v4$satisfaction_rating)
supportR::count(end_v4$future_wg_interest_self)
supportR::count(end_v4$future_wg_encourage_peer)
supportR::count(end_v4$anticipate_continuing_wg_research)
supportR::count(end_v4$asset_used)
supportR::count(end_v4$expectations_evolve)

# Check structure
dplyr::glimpse(end_v4)

## ------------------------------------- ##
# Reorder / Streamline Columns ----
## ------------------------------------- ##

# Reorder the columns sensibly
end_v5 <- end_v4 %>%
  dplyr::select(survey_iteration, start_date, end_date,
                program, cohort, synthesis_group, attendance_mode, 
                dplyr::starts_with("attendance_mtg_"),
                expectations_evolve:outcomes_unexpected_text,
                dplyr::starts_with("satisfaction_"),
                dplyr::starts_with("future_wg_"),
                anticipate_continuing_wg_research,
                continue_plan_text,
                dplyr::contains("outcome"),
                dplyr::contains("overcome"),
                dplyr::starts_with("asset_"),
                dplyr::starts_with("benefits_"),
                dplyr::starts_with("support_"),
                dplyr::starts_with("time_spent_"),
                dplyr::starts_with("expec_unmet_"),
                feedback_text) %>% 
  # Rename NCO with current name for LTER Network Office
  dplyr::rename(expec_unmet_could_lno_help = expec_unmet_could_nco_help)

# Check which columns were lost
supportR::diff_check(old = names(end_v4), new = names(end_v5))

# Check structure
dplyr::glimpse(end_v5)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
end_v99 <- end_v5

# Export locally
write.csv(end_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_full_end.csv"))

# End ----
