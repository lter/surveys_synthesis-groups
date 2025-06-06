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
  survey_prep(df = ., resp = "race", grp = "cohort")
  

str(demo_sub)


# Make desired graph
ggplot(demo_sub, mapping = aes(x = count, y = reorder(race, count), fill = cohort)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#662506", "#993404", "#cc4c02", 
                               "#ec7014", "#fe9929", "#fec44f")) +
  lno_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.7, 0.2),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank())

# Generate nice file name
(plotname <- paste0(filestem, "race", ".png"))

# Export the graph
ggsave(filename = file.path("graphs", plotname), width = 6, height = 6, units = "in")

# Remove some things from the environment to avoid 'wrong data' errors
rm(list = c("demo_sub", "plotname", "semi_ct")); gc()






# End ----
