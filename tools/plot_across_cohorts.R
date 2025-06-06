#' @title Create a Stacked Barplot with 'Number Responses' on X-Axis
#' 
#' @description Creates a graph with response category (y-axis) against number of responses (y-axis). Cohorts are included either in different facets or as a stacked bar with different colors
#' 
#' @param df (data.frame-like) Survey data containing--at least--a column named "cohort"
#' @param resp (character) Single column name in 'df' corresponding to the question for which summarization is desired
#' @param facet (logical) Whether to facet by cohort or (if `FALSE`) stack cohort bars into one bar per response category (across cohorts)
#' @param colors (character) _Optional_ ector of colors to map to cohorts. If left blank, defaults to a nice reddish-purple for all cohorts
#' 
#' @return (ggplot2 graph) Finished graph
#' 
plot_across_cohorts <- function(df = NULL, resp = NULL, facet = TRUE, colors = NULL){
  
  # Errors for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE || !all(c("cohort", "count", "cat_total") %in% names(df)))
    stop("'df' must be data.frame-like and contain 'cohort', 'count', and 'cat_total' columns")
  
  # Errors for 'resp'
  if(is.null(resp) || is.character(resp) != TRUE || length(resp) != 1 || !resp %in% names(df))
    stop("'resp' must be a single column name found in 'df'")
  
  # Warning for 'facet'
  if(is.logical(facet) != TRUE){
    warning("'facet' must be logical, defaulting to TRUE")
    facet <- TRUE }
  
  # Errors for 'colors'
  if(is.null(colors) & facet == FALSE){ colors <- "#892b64" }
  
  # Bonus error for 'colors'
  if(is.null(colors) != TRUE & facet == FALSE){
    if(is.character(colors) != TRUE || length(colors) < length(unique(df$cohort)))
      stop("Providing multiple 'colors' is necessary if not faceting graph")
  }
  
  # Make the bones of the desired graph
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = count, 
                                                         y = reorder(.data[[resp]], cat_total), 
                                                         fill = cohort, color = "x")) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Number of Responses") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = "#000") +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 14),
                   axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 8),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_blank())
  
  # Minor bonus tweaks depending on whether faceting is desired
  if(facet == TRUE){
    # Faceted graphs should actually be faceted and should not have a legend
    q <- p + 
      ggplot2::facet_wrap(cohort ~ ., axes = "all_x") +
      ggplot2::theme(legend.position = "none")
    
  } else {
    # Unfaceted graphs need a legend placed in a smart way
    q <- p +
      ggplot2::theme(legend.position = "inside",
                     legend.position.inside = c(0.85, 0.15))
  }
  
  # Return created graph
  return(q) }

# End ----
