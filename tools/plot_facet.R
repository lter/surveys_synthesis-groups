## Creates graph faceted by cohort

# Define function
plot_facet <- function(data = NULL, ..., color = NULL){
  
  # Error out for missing data / response variable
  if(is.null(data) == TRUE | is.null(color) == TRUE)
    stop("All arguments must be specified")
  
  # Generate barplot faceted by cohort
  graph <- ggplot(data, aes(..., fill = cohort)) +
    geom_bar(stat = 'identity') +
    coord_flip() + 
    theme(axis.title.x = element_blank()) +
    facet_grid(cohort ~ .) +
    scale_fill_manual(values = color) +
    lno_theme +
    theme(legend.position = "none")
  
  # Return that plot
  return(graph) }
