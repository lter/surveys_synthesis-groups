#' @title Create a Graph Faceted by Cohort
#' 
#' @description Creates a graph that is faceted by cohort and has each color given a user-specified color
#' 
#' @param df (data.frame-like) Survey data containing--at least--a column named "cohort"
#' @param colors (character) Vector of colors to map to cohorts. Can be named or unnamed
#' 
#' @return (ggplot2 graph) Finished graph
#' 
plot_facet <- function(df = NULL, ..., colors = NULL){
  
  # Errors for 'df'
  if(is.null(df) || !"data.frame" %in% class(df) || !"cohort" %in% names(df))
    stop("'df' must be data.frame-like and contain a column named 'cohort")
  
  # Errors for 'colors'
  if(is.null(colors) || is.character(colors) != TRUE || length(colors) < length(unique(df$cohort)))
    stop("'colors' must be provided and contain as many values as there are cohorts")
  
  # Generate barplot faceted by cohort
  graph <- ggplot2::ggplot(.data = df, mapping = ggplot2::aes(..., fill = cohort)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::coord_flip() + 
    ggplot2::facet_grid(cohort ~ .) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.position = "none")
  
  # Return that plot
  return(graph) }

# End ----
