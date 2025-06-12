# ---------------------------------------------------------- ##
# Upload Reports to Google Drive
## ---------------------------------------------------------- ##

# Purpose:
## Upload all report files to the relevant Drive folder
## Overwriting reports of the same name already there

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Upload Local Reports
## ------------------------------------- ##

# Identify local graphs
(local_reports <- dir(pattern = "*.pdf"))

# Upload them to the Drive (overwriting files of the same name)
purrr::walk(.x = local_reports,
            .f = ~ googledrive::drive_upload(media = .x, overwrite = T,
                                             path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IDI3xruhkmhq__uXa-p9fCwCDPSM_G9x")))

# End ----
