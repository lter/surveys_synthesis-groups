# ---------------------------------------------------------- ##
# Graph Survey Data - Full WG (End)
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
end <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_full_end.csv")) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(end)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-03_full-wg-end_"

## ------------------------------------- ##
# Satisfaction Rating ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(end$satisfaction_rating))
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
end_sub <- survey_prep(df = end, resp = "satisfaction_rating", grp = "cohort") %>% 
  dplyr::mutate(satisfaction_rating = factor(satisfaction_rating, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = end_sub, resp = "satisfaction_rating", colors = sub_cols, 
                hline_int = 50, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "satisfaction", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 7, height = 7, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Future WG Interest (Self) ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(end$future_wg_interest_self))
sub_cols <- c("Extremely interested" = "#642ca9",
              "Quite interested" = "#ff36ab",
              "Somewhat interested" = "#ff74d4",
              "Neither interested nor disinterested" = "#ffdde1")

# Prepare the data for plotting
end_sub <- survey_prep(df = end, resp = "future_wg_interest_self", grp = "cohort") %>% 
  dplyr::mutate(future_wg_interest_self = factor(future_wg_interest_self, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = end_sub, resp = "future_wg_interest_self", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "future-interest-self", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Encourage Colleague ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(end$future_wg_encourage_peer))
sub_cols <- c("Extremely likely" = "#31cb00",
              "Quite likely" = "#119822",
              "Somewhat likely" = "#1e441e")

# Prepare the data for plotting
end_sub <- survey_prep(df = end, resp = "future_wg_encourage_peer", grp = "cohort") %>% 
  dplyr::mutate(future_wg_encourage_peer = factor(future_wg_encourage_peer, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = end_sub, resp = "future_wg_encourage_peer", colors = sub_cols, 
                hline_int = 25, hline_col = "#000", 
                total_y = 90, total_col = "#000")

# Generate nice file name
(plotname <- paste0(filestem, "future-encourage-peer", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Expectation Evolution ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(end$expectations_evolve))
sub_cols <- c("Dramatic change" = "#f95738",
              "Substantial change" = "#ee964b",
              "Modest change" = "#f4d35e",
              "No change at all" = "#faf0ca")

# Prepare the data for plotting
end_sub <- survey_prep(df = end, resp = "expectations_evolve", grp = "cohort") %>% 
  dplyr::mutate(expectations_evolve = factor(expectations_evolve, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = end_sub, resp = "expectations_evolve", colors = sub_cols, 
                hline_int = c(25, 75), hline_col = "#000", 
                total_y = 95, total_col = "#000")

# Generate nice file name
(plotname <- paste0(filestem, "expectations-evolve", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Personal Skill Utilization ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(end$asset_used))
sub_cols <- c("Extremely well-utilized" = "#392f5a",
              "Moderately well-utilized" = "#9dd9d2",
              "Neither well- nor poorly-utilized" = "#fff8f0",
              "Poorly utilized" = "#f4d06f",
              "Extremely poorly utilized" = "#ff8811")

# Prepare the data for plotting
end_sub <- survey_prep(df = end, resp = "asset_used", grp = "cohort") %>% 
  dplyr::mutate(asset_used = factor(asset_used, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = end_sub, resp = "asset_used", colors = sub_cols, 
                hline_int = 25, hline_col = "#000", 
                total_y = 30, total_col = "#000") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "asset-used", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##

# Prepare the data for plotting
end_sub <- end %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("benefits")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>%
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Great benefit")) %>%
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
    question == "benefits_other_cultures" ~ "Understand Other Cultures",
    question == "benefits_expertise" ~ "Acquire Expertise",
    question == "benefits_leadership" ~ "Leadership Opportunities",
    question == "benefits_mentorship_get" ~ "Be a Mentee",
    question == "benefits_mentorship_give" ~ "Be a Mentor",
    question == "benefits_network" ~ "Networking",
    question == "benefits_other_disc" ~ "Other Disciplines",
    question == "benefits_others_ideas" ~ "Others' Ideas",
    question == "benefits_skills" ~ "Learn New Skills",
    question == "benefits_sensitive_data" ~ "Sensitive Data",
    question == "benefits_solve_group" ~ "Problem Solve as a Group",
    question == "benefits_solve_quick" ~ "Quickly Problem Solve",
    T ~ question)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = end_sub, resp = "question", facet = T,
                    colors = c("#013a63", "#01497c", "#014f86", 
                               "#2a6f97", "#2c7da0", "#468faf",
                               "#61a5c2", "#89c2d9", "#a9d6e5"))

# Generate nice file name
(plotname <- paste0(filestem, "benefits", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname")); gc()

## ------------------------------------- ##
# Support Value ----
## ------------------------------------- ##

# Prepare the data for plotting
end_sub <- end %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("support_value")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>%
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Helped significantly")) %>%
  # Drop unwanted 'other benefit' column
  dplyr::filter(question != "support_value_other_text") %>% 
  # Prepare for survey creation
  survey_prep(df = ., resp = "question", grp = "cohort") %>% 
  # Make better-formatted text (for graph axis marks)
  dplyr::mutate(question = dplyr::case_when(
    question == "support_value_analysis_id" ~ "Identify Analyses",
    question == "support_value_comm_group" ~ "Within Group Communication",
    question == "support_value_comm_public" ~ "Communication to Public",
    question == "support_value_data_get" ~ "Acquire Data",
    question == "support_value_data_prep" ~ "Prepare Data",
    question == "support_value_id_new_participants" ~ "Identify New Participants",
    question == "support_value_plan_mtgs_in_person" ~ "Meeting Logistics",
    question == "support_value_mtg_facilitation" ~ "Meeting Facilitation",
    question == "support_value_plan_mtgs_virtual" ~ "Meeting Virtually",
    question == "support_value_teamwork" ~ "Teamwork Facilitation",
    question == "support_value_workflow" ~ "Workflow Development",
    T ~ question)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = end_sub, resp = "question", facet = T,
                    colors = c("#05668d", "#028090", "#00a896", 
                               "#02c39a", "#f0f3bd"))

# Generate nice file name
(plotname <- paste0(filestem, "support-value", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 3, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("end_sub", "plotname")); gc()

# End ----
