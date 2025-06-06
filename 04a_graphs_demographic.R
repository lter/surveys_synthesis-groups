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
ggplot(demo_sub, mapping = aes(x = cohort, y = perc_resp, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Cohort", y = "Percent of Participants") +
  geom_hline(yintercept = 50, linetype = 2) +
  scale_fill_manual(values = sub_cols) +
  lno_theme +
  theme(legend.position = "top")

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
ggplot(demo_sub, mapping = aes(x = cohort, y = perc_resp, fill = sexuality)) +
  geom_bar(stat = "identity") +
  labs(x = "Cohort", y = "Percent of Participants") +
  scale_fill_manual(values = sub_cols) +
  guides(fill = guide_legend(nrow = 2)) +
  lno_theme +
  theme(legend.position = "top")

# Generate nice file name
(plotname <- paste0(filestem, "sexuality", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "sub_cols")); gc()








# End ----
