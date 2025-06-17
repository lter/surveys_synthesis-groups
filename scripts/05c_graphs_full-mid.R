# ---------------------------------------------------------- ##
# Graph Survey Data - Full WG (Mid)
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
mid <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_full_mid-point.csv")) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(mid)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-02_full-wg-mid-project_"

## ------------------------------------- ##
# Satisfaction Rating ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(mid$satisfaction_rating))
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
mid_sub <- survey_prep(df = mid, resp = "satisfaction_rating", grp = "cohort") %>% 
  dplyr::mutate(satisfaction_rating = factor(satisfaction_rating, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = mid_sub, resp = "satisfaction_rating", colors = sub_cols, 
                hline_int = 50, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "satisfaction", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 7, height = 7, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("mid_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Expectation Evolution ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(mid$expectations_evolve))
sub_cols <- c("Dramatic change" = "#f95738",
              "Substantial change" = "#ee964b",
              "Modest change" = "#f4d35e",
              "No change at all" = "#faf0ca")

# Prepare the data for plotting
mid_sub <- survey_prep(df = mid, resp = "expectations_evolve", grp = "cohort") %>% 
  dplyr::mutate(expectations_evolve = factor(expectations_evolve, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = mid_sub, resp = "expectations_evolve", colors = sub_cols, 
                hline_int = 75, hline_col = "#000", 
                total_y = 10, total_col = "#000") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "expectations-evolve", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("mid_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##

# Prepare the data for plotting
mid_sub <- mid %>% 
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
    # question == "benefits_co_create" ~ "Co-Create Knowledge",
    question == "benefits_co_create_science" ~ "Co-Create Knowledge",
    question == "benefits_communication" ~ "Public Communication",
    question == "benefits_data_security" ~ "Data Security",
    # question == "benefits_diff_culture" ~ "Understand Different Cultures",
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
plot_across_cohorts(df = mid_sub, resp = "question", facet = T,
                    colors = c("#013a63", "#01497c", "#014f86", 
                               "#2a6f97", "#2c7da0", "#468faf",
                               "#61a5c2", "#89c2d9", "#a9d6e5"))

# Generate nice file name
(plotname <- paste0(filestem, "benefits", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("mid_sub", "plotname")); gc()

# End ----
