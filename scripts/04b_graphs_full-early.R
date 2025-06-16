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
early <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_full_first-meeting.csv")) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(early)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-01_full-wg-first-meeting_"

## ------------------------------------- ##
# Satisfaction Rating ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(early$satisfaction_so_far))
sub_cols <- c("Extremely satisfied" = "#001219",
              "Moderately satisfied" = "#005f73",
              "Somewhat satisfied" = "#0a9396",
              "Slightly satisfied" = "#94d2bd",
              "Neither satisfied nor dissatisfied" = "#e9d8a6",
              "Slightly dissatisfied" = "#ca6702",
              "Somewhat dissatisfied" = "#bb3e03",
              "Moderately dissatisfied" = "#ae2012",
              "Extremely dissatisfied" = "#9b2226")

# Prepare the data for plotting
early_sub <- survey_prep(df = early, resp = "satisfaction_so_far", grp = "cohort") %>% 
  dplyr::mutate(satisfaction_so_far = factor(satisfaction_so_far, levels = names(sub_cols))) %>% 
  dplyr::mutate(cohort = factor(cohort, levels = sort(unique(cohort))))

# Make desired graph
plot_stack_perc(df = early_sub, resp = "satisfaction_so_far", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "satisfaction", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("early_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##

# Prepare the data for plotting
early_sub <- early %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("benefits")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>%
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Extremely important" #, "Very important"
  )) %>%
  # Drop unwanted 'other benefit' column
  dplyr::filter(!question %in% c("benefits_other")) %>% 
  # Prepare for survey creation
  survey_prep(df = ., resp = "question", grp = "cohort") %>% 
  # Make better-formatted text (for graph axis marks)
  dplyr::mutate(question = dplyr::case_when(
    question == "benefits_access_data" ~ "Access Data",
    question == "benefits_authorship_data" ~ "Author Data",
    question == "benefits_authorship_papers" ~ "Author Papers",
    question == "benefits_co_create" ~ "Co-Create Knowledge",
    question == "benefits_communication" ~ "Public Communication",
    question == "benefits_data_security" ~ "Data Security",
    question == "benefits_diff_culture" ~ "Understand Different Cultures",
    question == "benefits_expertise" ~ "Acquire Expertise",
    question == "benefits_leadership" ~ "Leadership Opportunities",
    question == "benefits_mentorship_get" ~ "Be a Mentee",
    question == "benefits_mentorship_give" ~ "Be a Mentor",
    question == "benefits_network" ~ "Networking",
    question == "benefits_other_disc" ~ "Other Disciplines",
    question == "benefits_others_ideas" ~ "Others' Ideas",
    question == "benefits_skills" ~ "Learn New Skills",
    question == "benefits_solve_group" ~ "Problem Solve as a Group",
    question == "benefits_solve_quick" ~ "Quickly Problem Solve",
    T ~ question)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = early_sub, resp = "question", facet = T,
                    colors = c("#013a63", "#01497c", "#014f86", 
                               "#2a6f97", "#2c7da0", "#468faf",
                               "#61a5c2", "#89c2d9", "#a9d6e5"))

# Generate nice file name
(plotname <- paste0(filestem, "benefits", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("early_sub", "plotname")); gc()

## ------------------------------------- ##
# Challenges ----
## ------------------------------------- ##

# Prepare the data for plotting
early_sub <- early %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("challenge")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>%
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Very serious challenge",
                              "Serious challenge")) %>%
  # Drop unwanted 'other challenge' column
  dplyr::filter(!question %in% c("challenge_other")) %>% 
  # Prepare for survey creation
  survey_prep(df = ., resp = "question", grp = "cohort") %>% 
  # Make better-formatted text (for graph axis marks)
  dplyr::mutate(question = dplyr::case_when(
    question == "challenge_assumptions" ~ "Unexamined Assumptions",
    question == "challenge_comm_to_group" ~ "Within Group Communication",
    question == "challenge_comm_to_public" ~ "Public Communication",
    question == "challenge_data_incompatible" ~ "Data Incompatible",
    question == "challenge_data_unavailable" ~ "Data Unavailable",
    question == "challenge_diff_goals" ~ "Differing Goals",
    question == "challenge_funding_post_wg" ~ "Funding Post-WG",
    question == "challenge_instit_applying_results" ~ "Institutional Interest",
    question == "challenge_lack_diff_comm_exp" ~ "Lack Different Communities",
    question == "challenge_lack_non_science" ~ "Lack Non-Scientists",
    question == "challenge_time" ~ "Researcher Time",
    question == "challenge_unsafe_expressing_ideas" ~ "Unsafe Contributing",
    question == "challenge_vocab" ~ "Shared Vocabulary",
    T ~ question)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = early_sub, resp = "question", facet = T,
                    colors = c("#800f2f", "#a4133c", "#c9184a", 
                               "#ff4d6d", "#ff758f", "#ff8fa3",
                               "#ffb3c1", "#ffccd5", "#fff0f3"))

# Generate nice file name
(plotname <- paste0(filestem, "challenges", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("early_sub", "plotname")); gc()

# End ----
