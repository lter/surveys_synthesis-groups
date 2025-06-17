# ---------------------------------------------------------- ##
# Upload Graphs to Google Drive
## ---------------------------------------------------------- ##

# Purpose:
## Upload all graph files to the relevant Drive folder
## Overwriting graphs of the same name already there

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------- ##
# Upload Local Graphs
## ------------------------------------- ##

# Identify local graphs
(local_graphs <- dir(path = file.path("graphs")))

# Upload them to the Drive (overwriting files of the same name)
purrr::walk(.x = local_graphs,
            .f = ~ googledrive::drive_upload(media = file.path("graphs", .x), overwrite = T,
                                             path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1m6vfzqW8a9c1AaalNugPaRDMda_9byxq")))

# End ----
