# ---------------------------------------------------------- ##
# Graph Survey Data - All Full WG Surveys
## ---------------------------------------------------------- ##

# Purpose:
## Make graphs that show patterns across time *within cohort*

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
combo <- read.csv(file = file.path("data", "tidy", 
                                   "wg-survey-tidy_full_all-three-surveys.csv")) %>% 
  # Make empty cells into true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(is.na(.) | nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # Make cohort a factor with levels
  dplyr::mutate(cohort = factor(x = cohort, levels = sort(unique(.$cohort)))) %>% 
  # And make survey type a factor with a preferred order too
  dplyr::mutate(survey_type = factor(x = survey_type, 
                                     levels = c("Beginning", "Middle", "End")))

# Check structure
dplyr::glimpse(combo)

# Load needed custom function(s)
purrr::walk(.x = dir(path = "tools"), .f = ~ source(file.path("tools", .x)))

# Assemble a nice file stem for graphs on this survey's data
filestem <- "LTER_survey-01-to-03_full-wg-all-surveys_"

## ------------------------------------- ##
# Satisfaction Rating ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(combo$satisfaction_rating))
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
combo_sub <- survey_prep(df = combo, resp = "satisfaction_rating", grp = "cohort-survey") %>% 
  dplyr::mutate(satisfaction_rating = factor(satisfaction_rating, levels = names(sub_cols)))

# Make desired graph
ggplot(data = combo_sub, aes(x = survey_type, y = perc_resp, 
                             fill = satisfaction_rating, color = "x")) +
  geom_bar(stat = "identity") +
  facet_wrap(cohort ~ .) +
  labs(x = "Project Stage", y = "Percent of Responses (%)") +
  scale_fill_manual(values = sub_cols) +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  # Custom theme elements
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        strip.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Generate nice file name
(plotname <- paste0(filestem, "satisfaction", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 10, height = 8, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("combo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Expectation Evolution ----
## ------------------------------------- ##

# Define desired category order and colors
sort(unique(combo$expectations_evolve))
sub_cols <- c("Dramatic change" = "#f95738",
              "Substantial change" = "#ee964b",
              "Modest change" = "#f4d35e",
              "No change at all" = "#faf0ca")

# Prepare the data for plotting
combo_sub <- survey_prep(df = combo, resp = "expectations_evolve", grp = "cohort-survey") %>% 
  dplyr::mutate(expectations_evolve = factor(expectations_evolve, levels = names(sub_cols)))

# Make desired graph
ggplot(data = combo_sub, aes(x = survey_type, y = perc_resp, 
                             fill = expectations_evolve, color = "x")) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ cohort) +
  labs(x = "Project Stage", y = "Percent of Responses (%)") +
  scale_fill_manual(values = sub_cols) +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  # Custom theme elements
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        strip.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Generate nice file name
(plotname <- paste0(filestem, "expectations-evolve", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 10, height = 4, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("combo_sub", "plotname", "sub_cols")); gc()

## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##

# Assemble a bunch of nice three-part color vectors for project stage within question
sub_cols <- list(
  "Networking" = c("Beginning" = "#ade8f4", "Middle" = "#0096c7", "End" = "#023e8a"),
  "Others' Ideas" = c("Beginning" = "#e9c46a", "Middle" = "#f4a261", "End" = "#e76f51"),
  "Group Problem Solving" = c("Beginning" = "#a7c957", "Middle" = "#6a994e", "End" = "#386641"),
  "Other Disciplines" = c("Beginning" = "#e0aaff", "Middle" = "#9d4edd", "End" = "#5a189a"),
  "Authoring Papers" = c("Beginning" = "#778da9", "Middle" = "#415a77", "End" = "#1b263b"),
  "Quick Problem Solving" = c("Beginning" = "#a68a64", "Middle" = "#7f4f24", "End" = "#582f0e"),
  "Acquiring Expertise" = c("Beginning" = "#b56576", "Middle" = "#6d597a", "End" = "#355070"),
  "Accessing Data" = c("Beginning" = "#e5383b", "Middle" = "#a4161a", "End" = "#660708")
  # "" = c("Beginning" = "#", "Middle" = "#", "End" = "#")
)

# Prepare data for plotting
combo_sub <- multi_cat_prep(df = combo, q_stem = "benefits_", grp = "cohort-survey") %>% 
  # Filter to only desired answer levels
  dplyr::filter(answer %in% c("Extremely important", "Great benefit")) %>% 
  # Tidy up question text
  dplyr::mutate(question = dplyr::case_when(
    question == "benefits_access_data" ~ "Accessing Data",
    question == "benefits_authorship_data" ~ "Authoring Data",
    question == "benefits_authorship_papers" ~ "Authoring Papers",
    question == "benefits_co_create" ~ "Co-Creating Knowledge",
    question == "benefits_communication" ~ "Public Communication",
    question == "benefits_data_security" ~ "Data Security",
    question == "benefits_other_cultures" ~ "Understanding Other Cultures",
    question == "benefits_expertise" ~ "Acquiring Expertise",
    question == "benefits_leadership" ~ "Leadership Opportunities",
    question == "benefits_mentorship_get" ~ "Being a Mentee",
    question == "benefits_mentorship_give" ~ "Being a Mentor",
    question == "benefits_network" ~ "Networking",
    question == "benefits_other_disc" ~ "Other Disciplines",
    question == "benefits_others_ideas" ~ "Others' Ideas",
    question == "benefits_skills" ~ "Learning New Skills",
    question == "benefits_sensitive_data" ~ "Sensitive Data",
    question == "benefits_solve_group" ~ "Group Problem Solving",
    question == "benefits_solve_quick" ~ "Quick Problem Solving",
    T ~ question))

# Which questions are we excluding (for now)?
supportR::diff_check(old = unique(combo_sub$question), new = names(sub_cols))

# Iterate across questions
for(focal_benefit in names(sub_cols)){
  
  # Progress message
  message("Generating graph for perceived benefits of: ", focal_benefit)
  
  # Subset to one specific sub-question
  combo_sub2 <- dplyr::filter(combo_sub, question == focal_benefit)
    
  # Create desired plot
  ggplot(data = combo_sub2, aes(x = survey_type, y = perc_resp, 
                                fill = survey_type, color = "x")) +
    geom_bar(stat = "identity") +
    facet_wrap(cohort ~ .) +
    labs(x = "Project Stage", y = "Percent of Responses (%)", 
         title = paste0("Import of: ", focal_benefit)) +
    scale_fill_manual(values = sub_cols[[focal_benefit]]) +
    scale_color_manual(values = "#000") +
    guides(color = "none") +
    # Custom theme elements
    theme_bw() + 
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          strip.text = element_text(size = 14),
          text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  # Generate nice file name
  benefit_tidy <- tolower(gsub(" |'", "-", focal_benefit))
  plotname <- paste0(filestem, "benefits_", benefit_tidy, ".png")

  # Export the graph
  ggsave(filename = file.path("graphs", plotname), width = 7, height = 5, units = "in")
  
} # Close loop

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("combo_sub", "combo_sub2", "plotname", "sub_cols",
            "benefit_tidy", "focal_benefit")); gc()

## ------------------------------------- ##
# Attendance ----
## ------------------------------------- ##

# Define desired category order and colors
sub_cols <- c("Absent" = "#5d576b",
              "In-Person" = "#f7567c",
              "Virtual" = "#99e1d9")

# Process data to prepare for graphing
combo_sub <- multi_cat_prep(df = combo, q_stem = "attendance_mtg", grp = "cohort") %>% 
  # Tidy meeting number
  dplyr::rename(meeting = question) %>% 
  dplyr::mutate(meeting = dplyr::case_when(
    meeting == "attendance_mtg_1" ~ "First",
    meeting == "attendance_mtg_2" ~ "Second",
    meeting == "attendance_mtg_3" ~ "Last")) %>% 
  # Drop missing meetings
  dplyr::filter(!is.na(meeting)) %>% 
  # Make relevant columns have an explicit order
  dplyr::mutate(meeting = factor(meeting, levels = c("First", "Second", "Last")),
                answer = factor(answer, levels = names(sub_cols)))

# Make the desired graph
ggplot(combo_sub, aes(x = meeting, y = perc_resp, fill = answer, color = "x")) +
  geom_bar(stat = "identity") +
  facet_wrap(cohort ~ .) +
  labs(x = "Meeting Instance", y = "Percent of Responses (%)") +
  scale_fill_manual(values = sub_cols) +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  # Custom theme elements
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        strip.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Generate nice file name
(plotname <- paste0(filestem, "attendance", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 8, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("combo_sub", "plotname", "sub_cols")); gc()

# End ----
