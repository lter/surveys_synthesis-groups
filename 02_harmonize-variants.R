## ---------------------------------------------------------- ##
# Harmonize Pre/Post-2020 Survey Variants
## ---------------------------------------------------------- ##

# Purpose:
## Use a data key to standardize column names between pre/post 2020 survey variants

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, ltertools)

# Make needed sub-folder(s)
dir.create(file.path("data", "standardized"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Download Data Key ----
## ------------------------------------- ##

# Identify all files in relevant Drive folder
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IDI3xruhkmhq__uXa-p9fCwCDPSM_G9x")) %>% 
  # Subset to only this file
  dplyr::filter(name == "synthesis-group-survey_data-key")

# Check that
key_drive

# Download it
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

# Read it in and prepare it
key_v1 <- read.csv(file = file.path("data", "synthesis-group-survey_data-key.csv"))

# Prepare the key for use in standardizing the survey variants
key_v2 <- ltertools::check_key(key = key_v1)

# Check structure
dplyr::glimpse(key_v2)

## ------------------------------------- ##
# Harmonize Survey Variants ----
## ------------------------------------- ##

# Read in all surveys
list_orig <- ltertools::read(raw_folder = file.path("data", "raw"))

# Check structure
dplyr::glimpse(list_orig[[1]])

# Make an empty list for storing outputs
list_std <- list()

# Iterate across survey files
for(focal_survey in unique(key_v2$source)){
  
  # Progress message
  message("Standarizing file: '", focal_survey, "'")
  
  # Standardize this file
  focal_v1 <- ltertools::standardize(focal_file = focal_survey, key = key_v2, 
                                     df_list = list_orig)
  
  # Add some useful columns
  focal_v2 <- focal_v1 %>% 
    dplyr::mutate(survey_iteration = ifelse(stringr::str_sub(focal_survey, 1, 4) == "LTER",
                                            yes = "2016-19 variant",
                                            no = "2020-present variant"),
                  .before = dplyr::everything())
  
  # Add it to the list
  list_std[[focal_survey]] <- focal_v2 }

# Check structure
dplyr::glimpse(list_std[[1]])

## ------------------------------------- ##
# Combine & Export Like Surveys ----
## ------------------------------------- ##

# Identify survey type by file name in key
survey_types <- key_v1 %>% 
  dplyr::select(survey_type, source) %>% 
  dplyr::distinct()

# What types are available?
survey_types

# Iterate across these
for(focal_type in unique(key_v1$survey_type)){
  # focal_type <- "demographic"
  
  # Progress message
  message("Processing survey type ", focal_type)
  
  # Subset the key to only the data files in this type
  surveys_in_type <- key_v1 %>% 
    dplyr::filter(survey_type == focal_type) %>%
    dplyr::pull(source) %>% 
    unique()
  
  # Make a list for just this type of survey
  list_type <- list()
  
  # Iterate across these
  for(focal_single in surveys_in_type){
    # focal_single <- "1_NCEAS_Synthesis_Group_Survey.csv"
    
    # Grab that element of the list
    survey_df <- purrr::pluck(.x = list_std, focal_single)
    
    # Add to the list for the type
    list_type[[focal_single]] <- survey_df
    
  }
  
  # Unlist
  type_df <- purrr::list_rbind(x = list_type) %>% 
    # Remove any empty columns (shouldn't be any but good to check)
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
    # Remove obvious test/bad rows
    dplyr::filter(start_date != "Start Date") %>% 
    dplyr::filter(stringr::str_detect(string = start_date, pattern = "ImportId") != T) %>% 
    dplyr::filter(!synthesis_group %in% c("test-test", "test-group-1", "test-group-2"))

  # Generate a nice local file name for this
  type_name <- paste0("wg-survey-results_", focal_type, ".csv")
  
  # Export locally
  write.csv(type_df, na = '', row.names = F,
            file = file.path("data", "standardized", type_name))
  
} # Close loop

# Check structure of just one survey type dataframe
dplyr::glimpse(type_df)


# End ----
