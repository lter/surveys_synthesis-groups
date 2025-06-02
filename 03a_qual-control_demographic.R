## ---------------------------------------------------------- ##
# Quality Control Survey Data - Demographics
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
demo_v1 <- read.csv(file = file.path("data", "standardized", "wg-survey-results_demographic.csv"))

# Check structure
dplyr::glimpse(demo_v1)

## ------------------------------------- ##
# Remove Test Rows ----
## ------------------------------------- ##

# Remove known test rows or non-LTER group data
demo_v2 <- demo_v1 %>% 
  dplyr::filter(status != "Survey Preview") %>% 
  dplyr::filter(!synthesis_group %in% c("MPA network assessment", "Food Equity", 
                                        "Grassland Birds", "Plastic Pollution"))

# Re-check structure
dplyr::glimpse(demo_v2)

## ------------------------------------- ##
# Parse Free Text Answers ----
## ------------------------------------- ##

# Wrangle free text response columns to be more usable
demo_v3 <- demo_v2 %>%
  # Doing this in an `ifelse` framework avoids potentially overwriting real entries
  dplyr::mutate(
    career_stage = ifelse(is.na(career_stage_other), yes = career_stage,
                          no = paste(career_stage, career_stage_other)),
    primary_disc = ifelse(is.na(primary_disc_other), yes = primary_disc,
                          no = paste(primary_disc, primary_disc_other)),
    life_sci_disc = ifelse(is.na(life_sci_disc_other), yes = life_sci_disc,
                           no = paste(life_sci_disc, life_sci_disc_other)),
    phys_sci_disc = ifelse(is.na(phys_sci_disc_other), yes = phys_sci_disc,
                           no = paste(phys_sci_disc, phys_sci_disc_other)),
    secondary_disc = ifelse(is.na(secondary_disc_other), yes = secondary_disc,
                            no = paste(secondary_disc, secondary_disc_other)),
    gender = ifelse(is.na(gender_other), yes = gender, no = paste(gender, gender_other)),
    family_educ = ifelse(is.na(family_educ_other), yes = family_educ,
                         no = paste(family_educ, family_educ_other)),
    latinx = ifelse(is.na(latinx_other), yes = as.character(latinx),
                    no = paste(latinx, latinx_other))
  ) %>%
  # Rename the 'race_other' column for later processing
  dplyr::rename(race_text = race_other) %>%
  # That done, remove all of those "_other" columns we now no longer need
  dplyr::select(-contains('_other'))

# Make sure only unwanted columns are lost
supportR::diff_check(old = names(demo_v2), new = names(demo_v3))

# Re-check structure
dplyr::glimpse(demo_v3)

## ------------------------------------- ##
# Simplify 'Career Stage' Entries ----
## ------------------------------------- ##

# Streamline entries to make for better axis labels
demo_v4 <- demo_v3 %>%
  dplyr::mutate(
    # Combine the two career length column types
    career_stage = dplyr::coalesce(career_stage, as.character(years_in_field)),
    # Now standardize the entries
    career_stage = dplyr::case_when(
      ## Simplify long text options
      career_stage == "early stage (first 9 years in the workforce)" ~ "early (1-9 years)",
      career_stage == "mid stage (10 to 25 years in the workforce)" ~ "mid (10-25 years)",
      career_stage == "mature stage (more than 25 years in the workforce)" ~ "mature (26+ years)",
      career_stage == "career preparation (e.g., in education or training program)" ~ "prep (0 years)",
      ## Move number entries into relevant categories
      years_in_field == 0 ~ "prep (0 years)",
      years_in_field > 0 & years_in_field <= 9 ~ "early (1-9 years)",
      years_in_field >= 10 & years_in_field <= 25 ~ "mid (10-25 years)",
      years_in_field >= 26 ~ "mature (26+ years)",
      T ~ as.character(career_stage) )
  ) %>%
  # And rename the 'years in field' column
  dplyr::rename(career_duration_yr = years_in_field)

# Check what that gained us
supportR::count(vec = demo_v3$career_stage)
supportR::count(vec = demo_v4$career_stage)

# Re-check structure
dplyr::glimpse(demo_v4)

## ------------------------------------- ##
# Fix Typos & Casing Issues ----
## ------------------------------------- ##

# Casing of categorical options differed between surveys so we need to standardize to get the real number of groups
demo_v5 <- demo_v4 %>%
  dplyr::mutate(
    # Standardize professional role
    professional_role = tolower(professional_role),
    professional_role = dplyr::case_when(
      professional_role == "analyst/data scientist/programmer" ~ "data professional", 
      professional_role == "other: data manager" ~ "data professional", 
      professional_role == "researcher" ~ "researcher/scientific expert", 
      professional_role == "other: educator and researcher" ~ "other: researcher and educator", 
      T ~ as.character(professional_role)),
    # Primary discipline
    primary_disc = dplyr::case_when(
      primary_disc == "other: Environmental" ~ "other: Environmental Sciences", 
      T ~ as.character(primary_disc)),
    # Life sciences discipline
    life_sci_disc = dplyr::case_when(
      life_sci_disc == "other: biogeochemistry" ~ "other: Biogeochemistry",
      life_sci_disc == "other: urban ecology" ~ "other: Urban Ecology",
      T ~ as.character(life_sci_disc)),
    # Method frequency columns
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("method_freq_"),
                                .fns = ~ stringr::str_to_title(.))),
    # Demographic entries (race fixed separately)
    gender = stringr::str_to_title(gender),
    sexuality = dplyr::case_when(sexuality == "Yes" ~ "Identifies as LGBTQI",
                                 sexuality == "No" ~ "Does not identify as LGBTQI",
                                 T ~ as.character(sexuality)),
    family_educ = gsub("^PhD$", "doctoral degree (PhD)", family_educ),
    family_educ = gsub("Master's degree", "master's degree", family_educ),
    family_educ = gsub("bachelor's degree", "4-year degree", family_educ),
    self_educ = gsub("^PhD$", "doctoral degree (PhD)", self_educ),
    self_educ = gsub("Master's degree", "master's degree", self_educ),
    self_educ = gsub("bachelor's degree", "4-year degree", self_educ),
    latinx = case_when(latinx == "Yes" ~ "Identifies as Latino/a",
                       latinx == "No" ~ "Does not identify as Latino/a",
                       stringr::str_detect(race, pattern = "Latin") == T ~ "Identifies as Latino/a",
                       stringr::str_detect(race, pattern = "Latin") == F & !is.na(race) ~ "Does not identify as Latino/a",
                       T ~ as.character(latinx)),
    first_gen = dplyr::case_when(
      # Yes
      first_gen == "Yes" ~ "First person in family to attend college",
      family_educ %in% c("some or no high school", "high school diploma", 
                         "trade/technical/vocational training") ~ "First person in family to attend college",
      family_educ %in% c("No", "4-year degree", "master's degree" ,
                         "associate's degree", "doctoral degree (PhD)",
                         "professional degree (MD, JD, etc.)") ~ "Not first person to attend college",
      T ~ as.character(first_gen)) )

# Re-check structure
dplyr::glimpse(demo_v5)

# Check out one of those repairs
supportR::count(vec = demo_v4$first_gen)
supportR::count(vec = demo_v5$first_gen)

## ------------------------------------- ##
# Synonymize Race Categories ----
## ------------------------------------- ##

# Some of the syntax of these differed between survey variants but not the meaning
## Feels safe to synonymize those for visualization purposes
# In the first form all race answers are concatenated in a single column.
# In the second variant, each checkbox option gained its own column
demo_v6 <- demo_v5 %>%
  # Combine all race questions into a single column
  dplyr::mutate(dplyr::across(.cols = dplyr::contains("race"),
                              .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .))) %>% 
  tidyr::unite(col = "race_new", dplyr::contains("race"), sep = "; ", na.rm = T) %>%
  # Tidy that column
  dplyr::mutate(race_simp = dplyr::case_when(
    race_new == "Asian,other racial or ethnic group(s):; south east asian" ~ "Asian; Other racial or ethnic group(s): south east asian",
    race_new == "Asian,White/Caucasian" ~ "Asian; White/Caucasian",
    race_new == "American Indian/Native American,White/Caucasian" ~ "American Indian/Native American; White/Caucasian",
    race_new == "Black/African American" ~ "Black Or African American",
    tolower(race_new) == tolower("South East Asian; Asian,Other Racial Or Ethnic Group(s):") ~ "South East Asian; Asian",
    # race_new == "" ~ "",
    T ~ as.character(race_new) ),
    # Make each word capitalized
    race_title = stringr::str_to_title(race_simp),
    # And fix any errors that introduced
    race_title = gsub('Latino/A', 'Latino/a', race_title),
    race_title = gsub('Group\\(S\\)', 'Group(s)', race_title),
    race_title = gsub('American Indian/Native American', 'American Indian Or Alaska Native', race_title)) %>%
  # Do some more revisions
  dplyr::mutate(race = dplyr::case_when(
    race_title %in% c("Mexican", "Hispanic", "Latin American",
                      "Latinx", "Latino") ~ "Hispanic Or Latino/a",
    # race_title == "" ~ "",
    T ~ race_title)) %>%
  # Remove the united/messy and the tidy but not cased appropriately columns
  dplyr::select(-race_new, -race_simp, -race_title)

# Check that out briefly
supportR::count(vec = demo_v6$race)

# Re-check structure
dplyr::glimpse(demo_v6)

## ------------------------------------- ##
# Reorder & Streamline Columns ----
## ------------------------------------- ##

# Use 'select' to implicitly drop unwanted columns
demo_v7 <- demo_v6 %>%
  dplyr::select(
    # Survey metadata
    survey_iteration, start_date:end_date, 
    # Group information
    program, synthesis_group, attendance_mode,
    # Demographics
    gender, sexuality, race, latinx, disability,
    caregiving, socio_economics, family_educ, self_educ, first_gen,
    # Profession / career
    job_sector, professional_role, dplyr::starts_with('career_'), highest_degree_yr,
    # Method / discipline
    does_science, conduct_research,
    dplyr::ends_with('_disc'), interdisc,
    dplyr::starts_with('method_freq_'),
    # Personality / values
    dplyr::starts_with('personality_'),
    dplyr::starts_with('rankPersonal_'),
    personal_thinking_style,
    dplyr::starts_with('rankGroup_'),
    group_project_approach,
    dplyr::starts_with('rankConflict_'),
    conflict_strategy,
    # Additional feedback
    feedback_text )

# Check to be sure we only dropped what we wanted to
supportR::diff_check(old = names(demo_v6), new = names(demo_v7))

# Re-check structure
dplyr::glimpse(demo_v7)

## ------------------------------------- ##
# Fill Missing Synthesis Groups ----
## ------------------------------------- ##

# Check 'groups' currently in data
supportR::count(vec = demo_v7$synthesis_group)

# Use start/end dates to identify WG when not provided
demo_v8 <- demo_v7 %>% 
  # Identify start year
  dplyr::mutate(start_year = stringr::str_sub(start_date, 1, 4), .before = start_date) %>% 
  # Fill empty characters with true NAs
  dplyr::mutate(synthesis_group = ifelse(nchar(synthesis_group) == 0, 
                                         yes = NA_character_, no = synthesis_group)) %>% 
  # Start filling missing group IDs
  dplyr::mutate(synthesis_group = dplyr::case_when(
    start_year == 2018 ~ "SOM",
    is.na(synthesis_group) & start_date %in% c("2019-10-17 14:35:41", "2019-10-17 14:33:59") ~ "SOM",
    is.na(synthesis_group) & start_date %in% c("2019-03-26 12:45:36", "2019-03-29 14:50:18") ~ "saola",
    T ~ synthesis_group)) %>% 
  # Now replace faux group names and/or misspellings with true names
  dplyr::mutate(synthesis_group = dplyr::case_when(
    # 2016-19 used animal placeholders instead of group names
    synthesis_group == "javanRhino" ~ "C2E",
    synthesis_group == "saola" ~ "Stream Cycling",
    synthesis_group == "vaquita" ~ "SOM",
    synthesis_group == "amurLeopard" ~ "Synchrony",
    synthesis_group == "hawksbillTurtle" ~ "Biodiversity Productivity",
    # Some CAGED participants listed all prior working groups; pare that down
    start_year == 2025 & stringr::str_detect(string = synthesis_group, pattern = "CAGED, ") ~ "CAGED",
    # If there's anything, use that
    !is.na(synthesis_group) ~ synthesis_group,
    # If still nothing, put a placeholder
    T ~ "unknown group")) %>% 
  # Extract cohort years from synthesis group names
  dplyr::mutate(cohort = case_when(
    # 2016
    synthesis_group %in% c("C2E", "Stream Cycling", "Metacommunities") ~ "2016",
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
  # Drop temp column
  dplyr::select(-start_year)

# Check any still-missing synthesis groups
demo_v8 %>% 
  dplyr::filter(synthesis_group == "unknown group") %>% 
  dplyr::select(synthesis_group, start_date) %>% 
  dplyr::distinct()

# And check everything has cohort info
supportR::num_check(data = demo_v8, col = "cohort")

# Finally, check actual group designations
supportR::count(vec = demo_v8$synthesis_group)

# General structure check
dplyr::glimpse(demo_v8)

## ------------------------------------- ##
# Wrangle "Ranking" Questions ----
## ------------------------------------- ##

# Will need to split the dataframe and later re-combine to do this
demo_v8b <- demo_v8 %>% 
  dplyr::mutate(row = 1:nrow(.),
                person_id = paste0(row, "-", synthesis_group))

# Is that a unique ID?
length(unique(demo_v8b$person_id)) == nrow(demo_v8b)

# To deduce the top ranking of unranked answers, we need a new dataframe
unrank_df <- demo_v8b %>% 
  # Grab only a unique row identifier and unranked version of the rank columns
  dplyr::select(person_id, personal_thinking_style, group_project_approach, conflict_strategy) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -person_id, names_to = "names", values_to = "top_ranked") %>% 
  # Remove rows where no value was supplied to the 'top ranked' column
  dplyr::filter(!is.na(top_ranked) & nchar(top_ranked) != 0) %>% 
  # Replace the full text with the comparable column name in the ranked answers
  dplyr::mutate(top_ranked_fix = dplyr::case_when(
    # Personal answers
    top_ranked == "New ideas attract me more than existing solutions." ~ "rankPersonal_new_vs_existing_ideas",
    top_ranked == "I study each problem until I understand the underlying logic." ~ "rankPersonal_underlying_logic",
    top_ranked == "Developing a clear and detailed plan is very important to me." ~ "rankPersonal_detailed_plan_important",
    top_ranked == "I like to extend boundaries." ~ "rankPersonal_push_boundaries",
    top_ranked == "I appreciate collaboration as a way to bring new methods and knowledge to a challenge." ~ "rankPersonal_collaboration",
    top_ranked == "I enjoy exploring and understanding the perspective and world view of others." ~ "rankPersonal_understand_others",
    top_ranked == "I like to talk through my ideas as they come to me." ~ "rankPersonal_talk_through_ideas",
    top_ranked == "I prefer to articulate my ideas in writing and on my own before sharing them with a group." ~ "rankPersonal_prefer_writing_alone",
    top_ranked == "I always want to know what should be done when." ~ "rankPersonal_want_to_know_what_to_do",
    top_ranked == "I make detailed analyses." ~ "rankPersonal_make_detailed_analyses",
    # Group answers
    top_ranked == "Getting the project organized and underway." ~ "rankGroup_project_organized",
    top_ranked == "Understanding how the project can be of benefit to the group." ~ "rankGroup_group_benefit",
    top_ranked == "Determining how we are to go about doing the project." ~ "rankGroup_making_plan",
    top_ranked == "Discovering the goals and values of individuals in the group." ~ "rankGroup_id_individual_goals",
    top_ranked == "Understanding the purpose and value of the project." ~ "rankGroup_id_project_value",
    # Conflict answers
    top_ranked == "Identifying and trying to clarify the conflict." ~ "rankConflict_clarify_conflict",
    top_ranked == "Discussing the relationships between the ideas and your own experiences." ~ "rankConflict_connect_experiences",
    top_ranked == "Discussing the ideals and values involved." ~ "rankConflict_discussing_ideals",
    top_ranked == "Making sure all voices are heard, even those that are conflict-averse." ~ "rankConflict_allow_full_participation",
    top_ranked == "Expressing arguments emphatically and concisely." ~ "rankConflict_emphatic_argument",
  top_ranked == "Making the most logical and consistent arguments." ~ "rankConflict_logical_argument",
  T ~ NA)) %>% 
  # Drop unstandardized 'top ranking' column
  dplyr::select(-top_ranked) %>% 
  # Add "1" to all values (if it was identified here, it was their top choice for the category)
  dplyr::mutate(ranking = 1) %>% 
  # Make 'names' column entries specific to this dataframe
  dplyr::mutate(names = paste0(names, "_2")) %>% 
  # Reshape back to wide format
  tidyr::pivot_wider(names_from = names, values_from = top_ranked_fix, values_fill = NA)

# Does that yield no more than 3 responses per person? (1 / category of unranked response)
unrank_df %>% 
  dplyr::group_by(person_id) %>% 
  dplyr::summarize(ct = dplyr::n()) %>% 
  dplyr::filter(ct > 3)

# Any un-"fixed" responses?
unrank_df %>% 
  dplyr::filter(is.na(top_ranked_fix)) %>% 
  dplyr::pull(top_ranked) %>% 
  unique()

# General structure check
dplyr::glimpse(unrank_df)

# To put the ranking system in the unranked columns, we need another new dataframe
rank_df <- demo_v8b %>%
  # Grab only a unique row identifier and the rank columns
  dplyr::select(person_id, dplyr::starts_with("rank")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = -person_id, names_to = "names", values_to = "ranking") %>%
  # Create column for each ranking question and options
  dplyr::mutate(
    rank_group = stringr::str_sub(names, start = 1, end = 9),
    option = dplyr::case_when(
      # Conflict
      names == "rankConflict_clarify_conflict" ~ "Identifying and trying to clarify the conflict.",
      names == "rankConflict_connect_experiences" ~ "Discussing the relationships between the ideas and your own experiences.",
      names == "rankConflict_discussing_ideals" ~ "Discussing the ideals and values involved.",
      names == "rankConflict_emphatic_argument" ~ "Expressing arguments emphatically and concisely.",
      names == "rankConflict_logical_argument" ~ "Making the most logical and consistent arguments.",
      # Group
      names == "rankGroup_group_benefit" ~ "Understanding how the project can be of benefit to the group.",
      names == "rankGroup_id_individual_goals" ~ "Discovering the goals and values of individuals in the group.",
      names == "rankGroup_id_project_value" ~ "Understanding the purpose and value of the project.",
      names == "rankGroup_making_plan" ~ "Determining how we are to go about doing the project.",
      names == "rankGroup_personal_benefit" ~ "Understanding how the project can be of benefit to me.",
      names == "rankGroup_project_organized" ~ "Getting the project organized and underway.",
      names == "rankPersonal_detailed_plan_important" ~ "Developing a clear and detailed plan is very important to me.",
      # Personal
      names == "rankPersonal_make_detailed_analyses" ~ "I make detailed analyses.",
      names == "rankPersonal_new_vs_existing_ideas" ~ "New ideas attract me more than existing solutions.",
      names == "rankPersonal_push_boundaries" ~ "I like to extend boundaries.",
      names == "rankPersonal_underlying_logic" ~ "I study each problem until I understand the underlying logic.",
      names == "rankPersonal_want_to_know_what_to_do" ~ "I always want to know what should be done when.",
      # Other
      T ~ names), .before = names) %>%
  # Identify the top priority for each person within a question / survey respondent
  dplyr::group_by(rank_group, person_id) %>%
  dplyr::filter(ranking == 1) %>%
  dplyr::ungroup() %>% 
  # Remove the ranking column and old column name vector
  dplyr::select(-ranking, -names) %>%
  # Pivot to wide format
  tidyr::pivot_wider(id_cols = person_id, names_from = rank_group, values_from = option)

# Check structure
dplyr::glimpse(rank_df)


















## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
demo_v99 <- demo_v1

# Export locally
write.csv(demo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_demographic.csv"))

# End ----

