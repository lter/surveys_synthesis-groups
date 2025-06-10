## ---------------------------------------------------------- ##
# Download Post-2020 Survey Data
## ---------------------------------------------------------- ##

# Purpose:
## Download & anonymize needed survey data from Qualtrics

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, scicomptools, qualtRics)

# Make needed sub-folder(s)
dir.create(file.path("data", "raw"), showWarnings = F, recursive = T)

# Check for active Qualtrics API key
scicomptools::token_check(api = "qualtrics", secret = TRUE)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Download & Anonymize Survey Data ----
## ------------------------------------- ##

# Identify available Qualtrics data
survey_ids <- qualtRics::all_surveys() %>% 
  # Subset to only desired surveys
  dplyr::filter(stringr::str_detect(string = name, 
                                    pattern = paste0("#", 1:5, collapse = "|")))

# Check that out
survey_ids$name
## Should be 5; #1-#4 & #1B (SPARC)

# Loop across surveys...
for(focal_survey in unique(survey_ids$name)){
  # focal_survey <- "#1B NCEAS Synthesis Group Survey - SPARC Final"
  
  # Grab the ID of that 
  focal_id <- qualtRics::fetch_id(.data = survey_ids, survey_name = focal_survey)
  
  # Grab the data associated with that ID (may take a few seconds to complete)
  focal_df <- qualtRics::fetch_survey(surveyID = focal_id) %>% 
    # Remove identifying information here
    dplyr::select(-dplyr::starts_with(c("IPAddress", "ResponseId", "Recipient"))) %>% 
    # Remove 'display order' questions too
    dplyr::select(-dplyr::contains("_DO_"))

  # Generate a better file name for this locally
  focal_out_v1 <- gsub(pattern = "#|-", replacement = "", x = focal_survey)
  focal_out_v2 <- gsub(pattern = "  ", replacement = " ", x = focal_out_v1)
  focal_out <- gsub(pattern = " ", replacement = "_", x = focal_out_v2)
  
  # Export this locally into the 'raw' folder
  write.csv(x = focal_df, na = '', row.names = F,
            file = file.path("data", "raw", paste0(focal_out, ".csv")))
  
  # File completion message
  message("Exporting '", focal_survey, "' as '", focal_out, "'") }

# End ----
