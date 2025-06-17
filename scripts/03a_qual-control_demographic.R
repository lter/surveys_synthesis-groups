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
                         no = paste(family_educ, family_educ_other))
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
# Synonymize Race Categories ----
## ------------------------------------- ##

# Some of the syntax of these differed between survey variants but not the meaning
## Feels safe to synonymize those for visualization purposes
# In the first form all race answers are concatenated in a single column.
# In the second variant, each checkbox option gained its own column
demo_v5 <- demo_v4 %>%
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
supportR::count(vec = demo_v5$race)

# Re-check structure
dplyr::glimpse(demo_v5)

## ------------------------------------- ##
# Fix Typos & Casing Issues ----
## ------------------------------------- ##

# Casing of categorical options differed between surveys so we need to standardize to get the real number of groups
demo_v6 <- demo_v5 %>%
  dplyr::mutate(
    # Standardize professional role
    professional_role = tolower(professional_role),
    professional_role = dplyr::case_when(
      professional_role == "analyst/data scientist/programmer" ~ "data professional", 
      professional_role == "other: data manager" ~ "data professional", 
      professional_role == "researcher" ~ "researcher/scientific expert", 
      professional_role == "other: educator and researcher" ~ "other: researcher and educator",
      professional_role == "other:" ~ "other",
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
    latinx = dplyr::case_when(latinx == "Yes" ~ "Identifies as Latino/a",
                              latinx == "No" ~ "Does not identify as Latino/a",
                              stringr::str_detect(race, pattern = "Latin") == T ~ "Identifies as Latino/a",
                              stringr::str_detect(race, pattern = "Latin") == F & !is.na(race) ~ "Does not identify as Latino/a",
                              T ~ latinx),
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
dplyr::glimpse(demo_v6)

# Check out one of those repairs
supportR::count_diff(vec1 = demo_v5$first_gen, vec2 = demo_v6$first_gen)
supportR::count_diff(vec1 = demo_v5$latinx, vec2 = demo_v6$latinx)

## ------------------------------------- ##
# Reorder & Streamline Columns ----
## ------------------------------------- ##

# Use 'select' to implicitly drop unwanted columns
demo_v7 <- demo_v6 %>%
  dplyr::select(
    # Survey metadata
    survey_iteration, survey_type, start_date:end_date, 
    # Group information
    program, synthesis_group, 
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
    # 2021
    synthesis_group %in% c("Ecosystem Transitions", "Plant Reproduction") ~ "2021",
    # 2023
    synthesis_group %in% c("Fire and Aridlands", "Marine Consumer Nutrient Dynamics",
                           "Producers-Consumers-Disturbance", "Selection Across Scales",
                           "Soil P Controls on C and N", "Pelagic Community Structure",
                           "Flux-Gradient") ~ "2023",
    # 2025
    synthesis_group %in% c("ResilienceandManagement", "CAGED") ~ "2025",
    T ~ synthesis_group), .before = synthesis_group) %>% 
  # Fill missing progra info too
  dplyr::mutate(program = "LTER") %>% 
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

# Add '1' to rank columns when the un-ranked option that corresponds to it is picked
demo_v8c <- demo_v8b %>% 
  # Create columns if they didn't previously exist in the 'rank___' columns
  dplyr::mutate(rankPersonal_collaboration = NA,
                rankPersonal_understand_others = NA,
                rankPersonal_talk_through_ideas = NA,
                rankPersonal_prefer_writing_alone = NA,
                rankConflict_allow_full_participation = NA) %>% 
  # Identify top-ranked answer by what they put in the corresponding 'what was your top ___?' column 
  dplyr::mutate(
    # Personal rankings
    rankPersonal_new_vs_existing_ideas = ifelse(personal_thinking_style == "New ideas attract me more than existing solutions.", yes = 1, no = rankPersonal_new_vs_existing_ideas),
    rankPersonal_underlying_logic = ifelse(personal_thinking_style == "I study each problem until I understand the underlying logic.", yes = 1, no = rankPersonal_underlying_logic),
    rankPersonal_detailed_plan_important = ifelse(personal_thinking_style == "Developing a clear and detailed plan is very important to me.", yes = 1, no = rankPersonal_detailed_plan_important),
    rankPersonal_push_boundaries = ifelse(personal_thinking_style == "I like to extend boundaries.", yes = 1, no = rankPersonal_push_boundaries),
    rankPersonal_collaboration = ifelse(personal_thinking_style == "I appreciate collaboration as a way to bring new methods and knowledge to a challenge.", yes = 1, no = rankPersonal_collaboration),
    rankPersonal_understand_others = ifelse(personal_thinking_style == "I enjoy exploring and understanding the perspective and world view of others.", yes = 1, no = rankPersonal_understand_others),
    rankPersonal_talk_through_ideas = ifelse(personal_thinking_style == "I like to talk through my ideas as they come to me.", yes = 1, no = rankPersonal_talk_through_ideas),
    rankPersonal_prefer_writing_alone = ifelse(personal_thinking_style == "I prefer to articulate my ideas in writing and on my own before sharing them with a group.", yes = 1, no = rankPersonal_prefer_writing_alone),
    rankPersonal_want_to_know_what_to_do = ifelse(personal_thinking_style == "I always want to know what should be done when.", yes = 1, no = rankPersonal_want_to_know_what_to_do),
    rankPersonal_make_detailed_analyses = ifelse(personal_thinking_style == "I make detailed analyses.", yes = 1, no = rankPersonal_make_detailed_analyses),
    # Group rankings
    rankGroup_project_organized = ifelse(group_project_approach == "Getting the project organized and underway.", yes = 1, no = rankGroup_project_organized),
    rankGroup_group_benefit = ifelse(group_project_approach == "Understanding how the project can be of benefit to the group.", yes = 1, no = rankGroup_group_benefit),
    rankGroup_making_plan = ifelse(group_project_approach == "Determining how we are to go about doing the project.", yes = 1, no = rankGroup_making_plan),
    rankGroup_id_individual_goals = ifelse(group_project_approach == "Discovering the goals and values of individuals in the group.", yes = 1, no = rankGroup_id_individual_goals),
    rankGroup_id_project_value = ifelse(group_project_approach == "Understanding the purpose and value of the project.", yes = 1, no = rankGroup_id_project_value),
    # Conflict rankings
    rankConflict_clarify_conflict = ifelse(conflict_strategy == "Identifying and trying to clarify the conflict.", yes = 1, no = rankConflict_clarify_conflict),
    rankConflict_connect_experiences = ifelse(conflict_strategy == "Discussing the relationships between the ideas and your own experiences.", yes = 1, no = rankConflict_connect_experiences),
    rankConflict_discussing_ideals = ifelse(conflict_strategy == "Discussing the ideals and values involved.", yes = 1, no = rankConflict_discussing_ideals),
    rankConflict_allow_full_participation = ifelse(conflict_strategy == "Making sure all voices are heard, even those that are conflict-averse.", yes = 1, no = rankConflict_allow_full_participation),
    rankConflict_emphatic_argument = ifelse(conflict_strategy == "Expressing arguments emphatically and concisely.", yes = 1, no = rankConflict_emphatic_argument),
    rankConflict_logical_argument = ifelse(conflict_strategy == "Making the most logical and consistent arguments.", yes = 1, no = rankConflict_logical_argument) )

# Check whether that cut down on NAs
supportR::count(demo_v8b$rankPersonal_new_vs_existing_ideas)
supportR::count(demo_v8c$rankPersonal_new_vs_existing_ideas)

# Check structure
dplyr::glimpse(demo_v8c)

# To put the ranking system in the unranked columns, we need a new dataframe
rank_df <- demo_v8c %>%
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

# With this in hand we can bring in the new columns
demo_v9 <- demo_v8c %>%
  dplyr::left_join(rank_df, by = "person_id") %>%
  # Fill empty values in unranked option column with real NAs
  dplyr::mutate(dplyr::across(.cols = c(personal_thinking_style, group_project_approach,
                                        conflict_strategy),
                              .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .))) %>% 
  # And can coalesce the two versions of each ranked option question
  dplyr::mutate(personal_thinking_style = coalesce(personal_thinking_style, rankPerso),
                group_project_approach = coalesce(group_project_approach, rankGroup),
                conflict_strategy = coalesce(conflict_strategy, rankConfl) ) %>%
  # Reorder columns so 'like' columns are next to each other
  dplyr::relocate(dplyr::starts_with('rankPersonal_'),
                  personal_thinking_style,
                  dplyr::starts_with('rankGroup_'),
                  group_project_approach,
                  dplyr::starts_with('rankConflict_'),
                  conflict_strategy,
                  .after = dplyr::starts_with("personality_")) %>% 
  # Drop unwanted columns
  dplyr::select(-row, -person_id, -rankGroup, -rankConfl, -rankPerso)

# Check gained / lost columns
supportR::diff_check(old = names(demo_v8c), new = names(demo_v9))

# Did _that_ cut down on NAs?
supportR::count(demo_v8c$personal_thinking_style)
supportR::count(demo_v9$personal_thinking_style)

# Check structure
dplyr::glimpse(demo_v9)

## ------------------------------------- ##
# Wrangle Caregiving ----
## ------------------------------------- ##

# Streamline caregiving options
demo_v10 <- demo_v9 %>%
  dplyr::mutate(caregiving = dplyr::case_when(
    # Contributor
    caregiving %in% c("I am not a primary caregiver, but I contribute to the care of another person",
                      "I contribute to the care of another person, but am not the primary caregiver"
                      ) ~ "Contributor",
    # Past
    caregiving == "I am not currently responsible for caring for another person, and I have not been a primary caregiver in the past" ~ "Past caregiver",
    # Primary
    caregiving == "I am the primary caregiver for at least one person" ~ "Primary",
    # Shares equally
    caregiving %in% c("I share responsibility for care equally with another person",
                      "I am not a primary caregiver, but I share responsibility for care equally with another person"
                      ) ~ "Shares equally",
    # Not
    caregiving == "I am not currently responsible for caring for another person, but I have been a primary caregiver in the past" ~ "Not caregiver",
    T ~ caregiving) )

# Check categories
supportR::count(demo_v9$caregiving)
supportR::count(demo_v10$caregiving)

# Check structure
dplyr::glimpse(demo_v10)

## ------------------------------------- ##
# Discipline Wrangling ----
## ------------------------------------- ##

# Check discipline columns
demo_v10 %>% 
  dplyr::select(dplyr::contains("disc")) %>% 
  dplyr::glimpse()

# Do needed wrangling
demo_v10b <- demo_v10 %>%
  # Strip out key life science facets
  dplyr::mutate(
    tmp_life_ecoevo = ifelse(stringr::str_detect(life_sci_disc,
                                                 paste0(c("Animal Sciences", "Biodiversity", 
                                                          "Earth System Sciences",
                                                          "Ecology", "Evolution",
                                                          "Entomology", "Forest Sciences",
                                                          "Marine Biology", "Physiology",
                                                          "Plant Sciences", "Soil",
                                                          "Systems Biology"),
                                                        collapse = "|")),
                             yes = "Ecology and Evolution", no = NA),
    tmp_life_genetics = ifelse(stringr::str_detect(life_sci_disc,
                                                   paste0(c("Biophysics", "Genetics", 
                                                            "Genetics and Genomics",
                                                            "Microbiology", "Nutrition", 
                                                            "Pharmacology", "Structural Biology",
                                                            "Toxicology and Environmental Health"), 
                                                          collapse = "|")),
                               yes = "Microbiology and Genetics", no = NA),
    tmp_life_biochem = ifelse(stringr::str_detect(life_sci_disc,
                                                  paste0(c("Biochemistry",
                                                           "Biogeochemistry"), 
                                                         collapse = "|")),
                              yes = "Biochemistry", no = NA),
    tmp_life_bioinf = ifelse(stringr::str_detect(life_sci_disc,
                                                 paste0(c("Bioinformatics", "Modeling",
                                                          "Mathematical", "Statistics"), 
                                                        collapse = "|")),
                             yes = "Bioinformatics", no = NA)) %>% 
  # Recombine these new temporary columns
  tidyr::unite(col = life_sci_disc2, dplyr::starts_with("tmp_life_"), 
               sep = "; ", na.rm = T)

# Check difference between old/new discipline columns
demo_v10b %>% 
  dplyr::select(life_sci_disc2, life_sci_disc) %>% 
  dplyr::distinct()

# Do the same wrangling for the next discipline
demo_v10c <- demo_v10b %>% 
  # Make needed temporary columns
  dplyr::mutate(tmp_phys_chem = ifelse(stringr::str_detect(phys_sci_disc,
                                                       paste0(c("Chemistry", "chemistry"), 
                                                              collapse = "|")),
                                   yes = "Chemistry", no = NA),
                tmp_phys_env = ifelse(stringr::str_detect(phys_sci_disc,
                                                       paste0(c("Atmospheric Sciences and Meteorology",
                                                                "Environmental Sciences",
                                                                "Oceanography"), 
                                                              collapse = "|")),
                                   yes = "Environmental Sci.", no = NA),
                tmp_phys_earth = ifelse(stringr::str_detect(phys_sci_disc,
                                                       paste0(c("Earth Sciences", "Geology"), 
                                                              collapse = "|")),
                                   yes = "Earth Sci.", no = NA)) %>% 
  # Recombine these new temporary columns
  tidyr::unite(col = phys_sci_disc2, dplyr::starts_with("tmp_phys_"), 
               sep = "; ", na.rm = T)

# Check difference between old/new discipline columns
demo_v10c %>% 
  dplyr::select(phys_sci_disc2, phys_sci_disc) %>% 
  dplyr::distinct()

# Do the same wrangling for the next discipline
demo_v10d <- demo_v10c %>% 
  # Make needed temporary columns
  dplyr::mutate(tmp_inter_life = ifelse(stringr::str_detect(interdisc,
                                                        paste0(c("Bioinformatics",
                                                                 "Life Sciences"), 
                                                               collapse = "|")),
                                    yes = "Life Sci.", no = NA),
                tmp_inter_phys = ifelse(stringr::str_detect(interdisc,
                                                        paste0(c("Engineering",
                                                                 "Physical Sciences"), 
                                                               collapse = "|")),
                                    yes = "Physical Sci.", no = NA),
                tmp_inter_social = ifelse(stringr::str_detect(interdisc,
                                                        paste0(c("Education", "Sociology"), 
                                                               collapse = "|")),
                                    yes = "Social Sci.", no = NA),
                tmp_inter_math = ifelse(stringr::str_detect(interdisc,
                                                        paste0(c("Computer Science",
                                                                 "Math", "Statistics"), 
                                                               collapse = "|")),
                                    yes = "Math", no = NA)) %>% 
  # Recombine these new temporary columns
  tidyr::unite(col = interdisc2, dplyr::starts_with("tmp_inter_"), 
               sep = "; ", na.rm = T)

# Check difference between old/new discipline columns
demo_v10d %>% 
  dplyr::select(interdisc2, interdisc) %>% 
  dplyr::distinct()

# Do the same wrangling for the next discipline
demo_v10e <- demo_v10d %>% 
  # Make needed temporary columns
  dplyr::mutate(tmp_prim_inter = ifelse(stringr::str_detect(primary_disc,
                                                            paste0(c("Interdisciplinary",
                                                                     "no primary"), 
                                                                   collapse = "|")),
                                        yes = "Interdisciplinary", no = NA),
                tmp_prim_life = ifelse(stringr::str_detect(primary_disc,
                                                           paste0(c("Biology", "Bioinformatics",
                                                                    "Ecology", "Environmental",
                                                                    "Evolution", "fisheries", 
                                                                    "Life Sciences"), 
                                                                  collapse = "|")),
                                       yes = "Life Sci.", no = NA),
                tmp_prim_phys = ifelse(stringr::str_detect(primary_disc,
                                                           paste0(c("Earth Sciences", "Engineering",
                                                                    "Geochemistry", "Geology",
                                                                    "Physical Sciences", 
                                                                    "Soil Science"), 
                                                                  collapse = "|")),
                                       yes = "Physical Sci.", no = NA),
                tmp_prim_soc = ifelse(stringr::str_detect(primary_disc,
                                                          paste0(c("Behavioral", "Education", 
                                                                   "governance", "policy",
                                                                   "Social", "Sociology"), 
                                                                 collapse = "|")),
                                      yes = "Social Sci.", no = NA),
                tmp_prim_math = ifelse(stringr::str_detect(primary_disc,
                                                           paste0(c("Computer Science",
                                                                    " informatics",
                                                                    "Mathematics",
                                                                    "Statistics"), 
                                                                  collapse = "|")),
                                       yes = "Math", no = NA)) %>% 
  # Recombine these new temporary columns
  tidyr::unite(col = primary_disc2, dplyr::starts_with("tmp_prim_"), 
               sep = "; ", na.rm = T)

# Check difference between old/new discipline columns
demo_v10e %>% 
  dplyr::select(primary_disc2, primary_disc) %>% 
  dplyr::distinct()

# Do the same wrangling for the next discipline
demo_v10f <- demo_v10e %>% 
  # Make needed temporary columns
  dplyr::mutate(tmp_sec_inter = ifelse(stringr::str_detect(secondary_disc,
                                                            paste0(c("Interdisciplinary",
                                                                     "no secondary"), 
                                                                   collapse = "|")),
                                        yes = "Interdisciplinary", no = NA),
                tmp_sec_life = ifelse(stringr::str_detect(secondary_disc,
                                                           paste0(c("Biology", "Bioinformatics",
                                                                    "Ecology", "Environmental",
                                                                    "Evolution", "fisheries", 
                                                                    "Life Sciences"), 
                                                                  collapse = "|")),
                                       yes = "Life Sci.", no = NA),
                tmp_sec_phys = ifelse(stringr::str_detect(secondary_disc,
                                                           paste0(c("Earth Sciences", "Engineering",
                                                                    "Geochemistry", "Geology",
                                                                    "Physical Sciences", 
                                                                    "Soil Science"), 
                                                                  collapse = "|")),
                                       yes = "Physical Sci.", no = NA),
                tmp_sec_soc = ifelse(stringr::str_detect(secondary_disc,
                                                          paste0(c("Behavioral", "Education", 
                                                                   "governance", "policy",
                                                                   "Social", "Sociology"), 
                                                                 collapse = "|")),
                                      yes = "Social Sci.", no = NA),
                tmp_sec_math = ifelse(stringr::str_detect(secondary_disc,
                                                           paste0(c("Computer Science",
                                                                    " informatics",
                                                                    "Mathematics",
                                                                    "Statistics"), 
                                                                  collapse = "|")),
                                       yes = "Math", no = NA)) %>% 
  # Recombine these new temporary columns
  tidyr::unite(col = secondary_disc2, dplyr::starts_with("tmp_sec_"), 
               sep = "; ", na.rm = T)

# Check difference between old/new discipline columns
demo_v10f %>% 
  dplyr::select(secondary_disc2, secondary_disc) %>% 
  dplyr::distinct()

# Do the same wrangling for the next discipline
demo_v10g <- demo_v10f %>% 
  # Make needed temporary columns
  dplyr::mutate(academic_disc2 = dplyr::case_when(
    academic_disc %in% c("Biological Sciences") ~ "Life Sci.",
    academic_disc %in% c("Engineering", "Physical Sciences") ~ "Physical Sci.",
    academic_disc %in% c("Other fields or professions") ~ "Other Fields",
    T ~ academic_disc))

# Check difference between old/new discipline columns
demo_v10g %>% 
  dplyr::select(academic_disc2, academic_disc) %>% 
  dplyr::distinct()

# Make final (discipline) object
demo_v11 <- demo_v10g %>% 
  # Drop temporary columns
  dplyr::select(-dplyr::starts_with("tmp_")) %>% 
  # Replace old columns with new ones
  dplyr::select(-dplyr::ends_with("disc")) %>% 
  dplyr::rename_with(.cols = dplyr::ends_with("disc2"),
                     .fn = ~ gsub("disc2", "disc", x = .))

# Check gained / lost columns
supportR::diff_check(old = names(demo_v10), new = names(demo_v11))

# Check out tidy columns
demo_v11 %>% 
  dplyr::select(dplyr::ends_with("disc")) %>% 
  dplyr::distinct()

# General structure check
dplyr::glimpse(demo_v11)

## ------------------------------------- ##
# Export ----
## ------------------------------------- ##

# Save a final object
demo_v99 <- demo_v11

# Export locally
write.csv(demo_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "wg-survey-tidy_demographic.csv"))

# End ----
