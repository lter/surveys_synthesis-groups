# ---------------------------------------------------------- ##
# Graph Survey Data - SPARC
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
sparc <- read.csv(file = file.path("data", "tidy", "wg-survey-tidy_sparc.csv")) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort))))

# Check structure
dplyr::glimpse(sparc)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-01B_sparc_"

## ------------------------------------- ##
# Satisfaction Rating ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(sparc$satisfaction_rating))
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
sparc_sub <- survey_prep(df = sparc, resp = "satisfaction_rating", grp = "cohort") %>% 
  dplyr::mutate(satisfaction_rating = factor(satisfaction_rating, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = sparc_sub, resp = "satisfaction_rating", colors = sub_cols, 
                hline_int = 50, hline_col = "gray80", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "satisfaction", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 5, height = 5, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Future WG Interest (Self) ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(sparc$future_sparc_interest_self))
sub_cols <- c("Extremely interested" = "#642ca9",
              "Quite interested" = "#ff36ab",
              "Somewhat interested" = "#ff74d4",
              "Neither interested nor disinterested" = "#ffdde1",
              "Not at all interested" = "#ffbd00")

# Prepare the data for plotting
sparc_sub <- survey_prep(df = sparc, resp = "future_sparc_interest_self", grp = "cohort") %>% 
  dplyr::mutate(future_sparc_interest_self = factor(future_sparc_interest_self, 
                                                    levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = sparc_sub, resp = "future_sparc_interest_self", colors = sub_cols, 
                hline_int = 50, hline_col = "#fff", 
                total_y = 90, total_col = "#fff") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "future-interest-self", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 5, height = 5, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Encourage Colleague ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(sparc$future_sparc_encourage_peer))
sub_cols <- c("Extremely likely" = "#31cb00",
              "Quite likely" = "#119822",
              "Somewhat likely" = "#1e441e",
              "Not likely at all" = "#081c15")

# Prepare the data for plotting
sparc_sub <- survey_prep(df = sparc, resp = "future_sparc_encourage_peer", grp = "cohort") %>% 
  dplyr::mutate(future_sparc_encourage_peer = factor(future_sparc_encourage_peer, levels = names(sub_cols)))

# Make desired graph
plot_stack_perc(df = sparc_sub, resp = "future_sparc_encourage_peer", colors = sub_cols, 
                hline_int = 50, hline_col = "#000", 
                total_y = 90, total_col = "#000") +
  guides(fill = guide_legend(nrow = 2))

# Generate nice file name
(plotname <- paste0(filestem, "future-encourage-peer", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 5, height = 5, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##

# Prepare the data for plotting
sparc_sub <- sparc %>% 
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
plot_across_cohorts(df = sparc_sub, resp = "question", facet = T,
                    colors = c("#013a63", "#01497c", "#014f86", 
                               "#2a6f97", "#2c7da0", "#468faf",
                               "#61a5c2", "#89c2d9", "#a9d6e5"))

# Generate nice file name
(plotname <- paste0(filestem, "benefits", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname")); gc()

## ------------------------------------- ##
# Support Value ----
## ------------------------------------- ##

# Prepare the data for plotting
sparc_sub <- sparc %>% 
  # Pare down to desired columns only
  dplyr::select(cohort, dplyr::contains("support_value")) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -cohort, names_to = 'question', values_to = 'answer') %>%
  # Filter out non-responses (i.e., NAs)
  dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>%
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Helped significantly")) %>%
  # Drop unwanted 'other benefit' column
  dplyr::filter(stringr::str_detect(string = question, pattern = "support_value_other") != T) %>% 
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
plot_across_cohorts(df = sparc_sub, resp = "question", facet = T,
                    colors = c("#05668d", "#028090", "#00a896", 
                               "#02c39a", "#f0f3bd"))

# Generate nice file name
(plotname <- paste0(filestem, "support-value", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 3, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname")); gc()

## ------------------------------------- ##
# Time Spent ----
## ------------------------------------- ##

# Define desired category order and colors
sub_cols <- c("Less than 30 min/month" = "#eae2b7",
              "30 min to 2 hr/month" = "#fcbf49",
              "2-6 hr/month" = "#f77f00",
              "More than 6 hr/month" = "#d62828")

# Prepare the data for graphing
sparc_sub <- multi_cat_prep(df = sparc, q_stem = "time_spent_", grp = "cohort", 
                            excl_qs = c("time_spent_other", "time_spent_other_text")) %>% 
  # Make better-formatted text (for graph axis marks)
  dplyr::mutate(question = dplyr::case_when(
    question == "time_spent_analysis" ~ "Analysis",
    question == "time_spent_archive_prep" ~ "Archive Prep",
    question == "time_spent_comm_group" ~ "Communicating with Group",
    question == "time_spent_comm_public" ~ "Communicating with Public",
    question == "time_spent_data_prep" ~ "Preparing Data",
    question == "time_spent_writing" ~ "Writing",
    T ~ question)) %>% 
  # Determine order of answers
  dplyr::mutate(answer = factor(answer, levels = names(sub_cols)))

# Make desired graph
ggplot(sparc_sub, aes(y = reorder(question, -perc_resp), x = perc_resp, 
                      fill = answer, color = "x")) +
  geom_bar(stat = "identity") +
  labs(x = "Percent of Responses") +
  facet_wrap(cohort ~ ., axes = "all_x") +
  scale_fill_manual(values =  sub_cols) +
  scale_color_manual(values = "#000") +
  guides(color = "none",
         fill = guide_legend(reverse = T)) +
  theme_bw() + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_blank(),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank())

# Generate nice file name
(plotname <- paste0(filestem, "time-spent", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 3, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("sparc_sub", "plotname", "sub_cols")); gc()

# End ----
