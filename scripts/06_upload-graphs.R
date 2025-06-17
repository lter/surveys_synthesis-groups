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

# Identify Drive folder where graphs are uploaded
graph_drive_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1m6vfzqW8a9c1AaalNugPaRDMda_9byxq")

# Identify Drive graphs
(drive_graphs <- googledrive::drive_ls(graph_drive_folder)$name)

# Want to update Drive graphs by overwriting with local ones?
update <- FALSE

# Identify graphs to upload
if(update == T){
  wanted_graphs <- local_graphs
} else {
  wanted_graphs <- setdiff(x = local_graphs, y = drive_graphs)
}

# Upload them to the Drive (overwriting files of the same name)
purrr::walk(.x = wanted_graphs,
            .f = ~ googledrive::drive_upload(media = file.path("graphs", .x), 
                                             overwrite = T,
                                             path = graph_drive_folder))

# End ----
