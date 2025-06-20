# ---------------------------------------------------------- ##
# Graph Survey Data - Demographics
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
demo <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_demographic.csv")) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(demo)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-00_demographics_"

## ------------------------------------- ##
# Gender ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$gender))
sub_cols <- c("Male" = "#219ebc", "Female" = "#ffb703", 
              "Prefer To Self-Identify" = "gray70")

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "gender", grp = "cohort") %>% 
  dplyr::mutate(gender = factor(gender, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "gender", colors = sub_cols, 
                hline_int = 50, hline_col = "#000", 
                total_y = 90, total_col = "#fff")

# Generate nice file name
(plotname <- paste0(filestem, "gender", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Sexuality ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$sexuality))
sub_cols <- c("Does not identify as LGBTQI" = "#669bbc",
              "Identifies as LGBTQI" = "#c1121f", 
              "Prefer to self-identify" = "gray70",
              "Prefer not to answer" = "#000")

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "sexuality", grp = "cohort") %>% 
  dplyr::mutate(sexuality = factor(sexuality, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "sexuality", colors = sub_cols, 
                hline_int = 25, hline_col = "#000",
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "sexuality", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Race ----
## ------------------------------------- ##

# Identify max number of semicolons in 'race' column & add 1
semi_ct <- max(stringr::str_count(string = demo$race, pattern = ";"), na.rm = T) + 1

# Prepare the data for plotting
demo_sub <- demo %>% 
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(race) & nchar(race) != 0) %>% 
  # Pare down to desired columns
  dplyr::select(cohort, race) %>% 
  # Separate race entries by semicolon
  tidyr::separate_wider_delim(cols = race, delim = "; ", too_few = "align_start",
                              names = paste0("race_temp", 1:semi_ct)) %>% 
  # Remove some placeholder info
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("race_temp"),
                              .fns = ~ ifelse(stringr::str_detect(string = ., pattern = "other racial"),
                                              yes = gsub("other racial or ethnic group\\(s\\)\\: ",
                                                         "", tolower(.)),
                                              no = .))) %>% 
  # Pivot longer
  tidyr::pivot_longer(cols = dplyr::starts_with("race_temp"), 
                      names_to = "junk", values_to = "race") %>%
  # Remove NAs that introduces
  dplyr::filter(!is.na(race) & nchar(race) != 0) %>% 
  # Prepare for graph creation
  survey_prep(df = ., resp = "race", grp = "cohort") %>% 
  # Wrap text so the axis is readable
  dplyr::mutate(race = stringr::str_wrap(race, width = 20))

# Make desired graph
plot_across_cohorts(df = demo_sub, resp = "race", facet = T,
                   colors = c("#662506", "#993404", "#cc4c02", 
                              "#ec7014", "#fe9929", "#fec44f"))

# Generate nice file name
(plotname <- paste0(filestem, "race", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 8, height = 8, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "semi_ct")); gc()

## ------------------------------------- ##
# Latinx ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$latinx))
sub_cols <- c("Does not identify as Latino/a" = "#6d597a",
              "Identifies as Latino/a" = "#e56b6f")

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "latinx", grp = "cohort") %>% 
  dplyr::mutate(latinx = factor(latinx, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "latinx", colors = sub_cols, 
                hline_int = 25, hline_col = "#000", 
                total_y = 90, total_col = "#fff")

# Generate nice file name
(plotname <- paste0(filestem, "latinx", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Disability ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$disability))
sub_cols <- c("No" = "#006d77", "Yes" = "#e29578",
              "Prefer not to answer" = "gray70")

# Prepare the data for plotting
demo_sub <- demo %>% 
  dplyr::mutate(disability = ifelse(stringr::str_detect(string = disability, pattern = "Yes,"),
                                    yes = "Yes", no = disability)) %>% 
  survey_prep(df = ., resp = "disability", grp = "cohort") %>% 
  dplyr::mutate(disability = factor(disability, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "disability", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff")

# Generate nice file name
(plotname <- paste0(filestem, "disability", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Caregiving ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$caregiving))
sub_cols <- c("Not caregiver" = "#335c67",
              "Contributor" = "#fff3b0",
              "Past caregiver" = "#e09f3e",
              "Shares equally" = "#9e2a2b",
              "Primary" = "#540b0e",
              "Prefer not to answer" = "gray70")

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "caregiving", grp = "cohort") %>% 
  dplyr::mutate(caregiving = factor(caregiving, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "caregiving", colors = sub_cols, 
                hline_int = c(25, 75), hline_col = "#000", 
                total_y = 60, total_col = "#000")

# Generate nice file name
(plotname <- paste0(filestem, "caregiving", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# First Generation (College) ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$first_gen))
sub_cols <- c("No" = "#3f4139",
              "Yes" = "#d2fa52",
              "Prefer not to answer" = "gray70")

# Prepare the data for plotting
demo_sub <- demo %>% 
  dplyr::mutate(first_gen = dplyr::case_when(
    first_gen == "First person in family to attend college" ~ "Yes",
    first_gen == "Not first person to attend college" ~ "No",
    T ~ first_gen)) %>% 
  survey_prep(df = ., resp = "first_gen", grp = "cohort") %>% 
  dplyr::mutate(first_gen = factor(first_gen, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "first_gen", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff")

# Generate nice file name
(plotname <- paste0(filestem, "first-gen", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Career Stage ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$career_stage))
sub_cols <- c("prep (0 years)" = "#90e0ef",
              "early (1-9 years)" = "#00b4d8", 
              "mid (10-25 years)" = "#0077b6", 
              "mature (26+ years)" = "#03045e")

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "career_stage", grp = "cohort") %>% 
  dplyr::mutate(career_stage = factor(career_stage, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "career_stage", colors = sub_cols, 
                hline_int = c(25, 75), hline_col = "#000", 
                total_y = 88, total_col = "#000") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "career-stage", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Professional Role ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$professional_role))
sub_cols <- c("researcher" = "#5f0f40",
              "stakeholder" = "#9a031e", 
              "data professional" = "#fb8b24", 
              "educator" = "#e36414", 
              "administrator" = "#227c9d", 
              "project manager" = "#0f4c5c", 
              "other" = "gray70")

# Prepare the data for plotting
demo_sub <- demo %>% 
  dplyr::mutate(professional_role = dplyr::case_when(
    professional_role == "researcher/scientific expert" ~ "researcher",
    professional_role == "advisor/stakeholder" ~ "stakeholder",
    T ~ professional_role)) %>% 
  survey_prep(df = ., resp = "professional_role", grp = "cohort") %>% 
  dplyr::mutate(professional_role = factor(professional_role, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "professional_role", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  theme(legend.position = "right")

# Generate nice file name
(plotname <- paste0(filestem, "professional-role", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Personal Thinking Style ----
## ------------------------------------- ##

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "personal_thinking_style", grp = "cohort") %>% 
  # Wrap text so the axis is readable
  dplyr::mutate(personal_thinking_style = stringr::str_wrap(personal_thinking_style, 
                                                            width = 40)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = demo_sub, resp = "personal_thinking_style", facet = T,
                    colors = c("#00441b", "#006d2c", "#2ca25f", 
                               "#66c2a4", "#99d8c9", "#ccece6"))
# Generate nice file name
(plotname <- paste0(filestem, "personal-thinking-style", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 8, height = 8, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname")); gc()

## ------------------------------------- ##
# Group Project Approach ----
## ------------------------------------- ##

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "group_project_approach", grp = "cohort") %>% 
  # Wrap text so the axis is readable
  dplyr::mutate(group_project_approach = stringr::str_wrap(group_project_approach, 
                                                            width = 40)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = demo_sub, resp = "group_project_approach", facet = T,
                    colors = c("#49006a", "#7a0177", "#ae017e", 
                               "#dd3497", "#f768a1", "#fa9fb5"))

# Generate nice file name
(plotname <- paste0(filestem, "group-project-approach", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 8, height = 8, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname")); gc()

## ------------------------------------- ##
# Conflict Resolution Strategy ----
## ------------------------------------- ##

# Prepare the data for plotting
demo_sub <- survey_prep(df = demo, resp = "conflict_strategy", grp = "cohort") %>% 
  # Wrap text so the axis is readable
  dplyr::mutate(conflict_strategy = stringr::str_wrap(conflict_strategy, 
                                                           width = 40)) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = demo_sub, resp = "conflict_strategy", facet = T,
                    colors = c("#084081", "#0868ac", "#4eb3d3", 
                               "#7bccc4", "#a8ddb5", "#e0f3db"))

# Generate nice file name
(plotname <- paste0(filestem, "conflict-strategy", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 8, height = 8, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname")); gc()

## ------------------------------------- ##
# Job Sector ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$job_sector))
sub_cols <- c("Academia" = "#004e98",
              "Government" = "#cc0000", 
              "Non-Profit" = "#ffc300", 
              "Other" = "gray70")

# Prepare the data for plotting
demo_sub <- demo %>% 
  dplyr::mutate(job_sector = dplyr::case_when(
    job_sector == "Non-profit sector" ~ "Non-Profit",
    job_sector == "Academic research" ~ "Academia",
    job_sector == "State, local or federal government" ~ "Government",
    T ~ job_sector)) %>% 
  survey_prep(df = ., resp = "job_sector", grp = "cohort") %>% 
  dplyr::mutate(job_sector = factor(job_sector, levels = names(sub_cols))) %>% 
  # Tweak cohort levels
  dplyr::mutate(cohort = factor(cohort, levels = sort(unique(cohort))))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "job_sector", colors = sub_cols, 
                hline_int = 25, hline_col = "gray80", 
                total_y = 90, total_col = "#fff")

# Generate nice file name
(plotname <- paste0(filestem, "job-sector", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Highest Level of Education (Self) ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(demo$self_educ))
sub_cols <- c("doctoral degree" = "#1780a1",
              "master's degree" = "#5c4d7d",
              "4-year degree" = "#a01a58")

# Prepare the data for plotting
demo_sub <- demo %>% 
  dplyr::mutate(self_educ = dplyr::case_when(
    self_educ == "doctoral degree (PhD)" ~ "doctoral degree",
    T ~ self_educ)) %>% 
  survey_prep(df = ., resp = "self_educ", grp = "cohort") %>% 
  dplyr::mutate(self_educ = factor(self_educ, levels = names(sub_cols))) %>% 
  dplyr::mutate(cohort = factor(cohort, levels = sort(unique(cohort))))

# Make desired graph
plot_stack_perc(df = demo_sub, resp = "self_educ", colors = sub_cols, 
                hline_int = 25, hline_col = "#000", 
                total_y = 90, total_col = "#000")

# Generate nice file name
(plotname <- paste0(filestem, "self-educ", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Method Frequency ----
## ------------------------------------- ##

# Prepare the data for plotting
demo_sub <- demo %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("method_freq")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>% 
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Always", "Often")) %>%
  # Prepare for survey creation
  survey_prep(df = ., resp = "question", grp = "cohort") %>% 
  # Make better-formatted text (for graph axis marks)
  dplyr::mutate(question = dplyr::case_when(
    question == "method_freq_case_study" ~ "Case Studies",
    question == "method_freq_mix" ~ "Mixed Methods",
    question == "method_freq_participatory" ~ "Participatory",
    question == "method_freq_policy" ~ "Policy",
    question == "method_freq_qual" ~ "Qualitative",
    question == "method_freq_quant" ~ "Quantitative",
    question == "method_freq_simulation" ~ "Simulation",
    question == "method_freq_action" ~ "Action",
    question == "method_freq_deductive" ~ "Deductive",
    question == "method_freq_inductive" ~ "Inductive")) %>% 
  # Reorder factor level
  dplyr::mutate(cohort = factor(cohort, levels = rev(sort(unique(.$cohort)))))

# Make desired graph
plot_across_cohorts(df = demo_sub, resp = "question", facet = T,
                    colors = c("#7f0000", "#b30000", "#d7301f", 
                               "#ef6548", "#fdd49e", "#fff7ec"))

# Generate nice file name
(plotname <- paste0(filestem, "method-freq", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname")); gc()

# End ----
