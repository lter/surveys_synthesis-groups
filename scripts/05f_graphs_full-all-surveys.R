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
filestem <- "LTER_survey-04_full-wg-all-surveys_"

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
# Attendance ----
## ------------------------------------- ##


## ------------------------------------- ##
# Benefits ----
## ------------------------------------- ##



## ------------------------------------- ##
# Challenges ----
## ------------------------------------- ##



# End ----
