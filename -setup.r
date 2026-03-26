## ---------------------------------------------------------- ##
# Setup Scripts
## ---------------------------------------------------------- ##
# Purpose:
## Do any generally-useful setup tasks required by >1 other script

## ------------------------------------- ##
# Make Needed Folders ----
## ------------------------------------- ##

# Make needed data folder(s)
dir.create(file.path("data", "raw"), showWarnings = F, recursive = T)
dir.create(file.path("data", "standardized"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Other necessary folders
dir.create(file.path("graphs"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

# End ----
