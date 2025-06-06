# ---------------------------------------------------------- ##
# Report Helpers
## ---------------------------------------------------------- ##

# Purpose:
## Consolidate some non-function tools broadly useful for many aspects of graph/report creation

## ------------------------------------- ##
# Housekeeping ----
## ------------------------------------- ##

# Load libraries
library(ggplot2)

## ------------------------------------- ##
# `ggplot2` Theme
## ------------------------------------- ##

# Start with ggplot2 'black and white' theme
lno_theme <- ggplot2::theme_bw() + 
  # Then do custom modificaitons
  ggplot2::theme(
    # Always exclude legend titles
    legend.title = ggplot2::element_blank(),
    # Increase text size
    strip.text = element_text(size = 14),
    text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# End ----
