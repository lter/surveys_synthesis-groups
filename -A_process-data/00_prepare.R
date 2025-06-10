## ---------------------------------------------------------- ##
# Prepare for Running 'Actual' Workflow Scripts
## ---------------------------------------------------------- ##

# Purpose:
## Download needed survey data & make sure needed accesses are working as needed

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, scicomptools, qualtRics, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Get Pre-2020 Data (from Box) ----
## ------------------------------------- ##

# Surveys administered before 2022 have response data saved in Box
## Specifically Marty Downs will need to grant you access to the "LTER Synthesis Group Survey Data" folder
### URL = https://ucsb.app.box.com/folder/156011942055?s=mtk3emhslwg16t3lzy8be5fkrukwfmvl

# Once you have access, download the following files:
### NOTE: file paths are *all relative to the Box folder linked above*
## 1. Pre-2020 survey 1 results
( box_path1 <- file.path(".", "LTER_Synthesis_WG_2016-17_Survey-1.csv") )
## 2. Pre-2020 survey 2 results
( box_path2 <- file.path("full text responses (surveys 2-4)", "LTER_Synthesis_WG_Survey-2_FullText.csv"))
## 3. Pre-2020 survey 3 results
( box_path3 <- file.path("full text responses (surveys 2-4)", "LTER_Synthesis_WG_Survey-3_FullText.csv"))
## 4. Pre-2020 survey 4 results
( box_path4 <- file.path("full text responses (surveys 2-4)", "LTER_Synthesis_WG_Survey-4_FullText.csv"))

## ------------------------------------- ##
# Check Qualtrics Access (for Post-2020 Data) ----
## ------------------------------------- ##

# Surveys administered after 2022 were done via Qualtrics and can be downloaded directly through R*
## * = If the proper setup steps are taken

# Check the Qualtrics
scicomptools::token_check(api = "qualtrics", secret = TRUE)

# # If the above says anything other than "API Key identified", you'll need to add the key
# # Do so by (1) identifying your Qualtrics API key and (2) running the following code
# qualtRics::qualtrics_api_credentials(
#   api_key = "",
#   # (^) to find:
#   ## 1) sign in to Qualtrics site (https://ucsb.co1.qualtrics.com/homepage/ui)
#   ## 2) click user icon in top right (default is first initial in a circle)
#   ## 3) in dropdown, select "Account Settings"
#   ## 4) click the rightmost setting tab, labeled "Qualtrics IDs"
#   ## 5a) in "API" panel (bottom middle), copy character string to right of "Token"
#   ## 5b) if no token present, click "Generate Token"
#   ## 6) paste token string into above `api` argument
#   base_url = "ucsb.co1.qualtrics.com",
#   # (^) this URL is correct for all NCEAS purposes (as of 2022)
#   overwrite = T, 
#   # (^) overwrites 
#   install = T)
# # (^) TRUE = keeps token on your computer forever | FALSE = only store for this session

# To activate, restart R/RStudio

# Re-check the Qualtrics key
scicomptools::token_check(api = "qualtrics", secret = TRUE)

## ------------------------------------- ##
# Check Drive Export Folder Access -----
## ------------------------------------- ##

# You also need access to the Drive folder where generated reports are uploaded

# This line should list some files if you have access
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IDI3xruhkmhq__uXa-p9fCwCDPSM_G9x"))

# You'll also need to have R and GoogleDrive set up
## For a tutorial, visit: https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# End ----
