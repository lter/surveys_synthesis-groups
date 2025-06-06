#' @title Create a Stacked Barplot with 'Percent Participants' on Y-Axis
#' 
#' @description Creates a graph where each cohort (x-axis) gets a single bar and the colors in that stacked bar indicate the categories of possible answer to that question.
#' 
#' @param df (data.frame-like) Survey data containing--at least--a column named "cohort"
#' @param resp (character) Single column name in 'df' corresponding to the question for which summarization is desired
#' @param colors (character) Vector of colors to map to answer levels. Can be named or unnamed
#' @param hline_int (NULL / numeric) _Optional_ argument for any horizontal lines to put 'in front of' the bars. Can support any number of numbers if specified (one line created per user-supplied y-intercept)
#' @param hline_col (NULL / character) _Optional_ argument for coloring horizontal line(s). Useful for making sure line color contrasts nicely with stacked bar colors. Regardless of number of horizontal lines, **this argument only accepts one color**. Defaults to black (specified as a hexadecimal code)
#' 
#' @return (ggplot2 graph) Finished graph
#' 
plot_stack_perc <- function(df = NULL, resp = NULL, colors = NULL, 
                            hline_int = NULL, hline_col = "#000"){
  
  # Errors for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE || !all(c("cohort", "perc_resp") %in% names(df)))
    stop("'df' must be data.frame-like and contain 'cohort' and 'perc_resp' columns")
  
  # Errors for 'resp'
  if(is.null(resp) || is.character(resp) != TRUE || length(resp) != 1 || !resp %in% names(df))
    stop("'resp' must be a single column name found in 'df'")
  
  # Errors for 'colors'
  if(is.null(colors) || is.character(colors) != TRUE || length(colors) < length(unique(df$resp)))
    stop("'colors' must be provided and contain as many values as there are unique response values")
  
  # Errors for 'hline_int'
  if(!is.null(hline_int) & is.numeric(hline_int) != TRUE)
    stop("If 'hline_int' is provided, it must be a numeric vector")
  
  # Errors for 'hline_col' (only apply if 'hline_int' is specified)
  if(!is.null(hline_int)){
    if(is.null(hline_col) || is.character(hline_col) != TRUE || length(hline_col) != 1)
      stop("If 'hline_int' is provided, 'hline_col' must be a 1-element character vector")
  }
  
  # Generate desired plot
  q <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = cohort, y = perc_resp, 
                                                         fill = .data[[resp]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Cohort", y = "Percent of Participants") +
    ggplot2::scale_fill_manual(values = colors) +
    # Horizontal lines (if provided)
    ggplot2::geom_hline(yintercept = hline_int, linetype = 2, color = hline_col) +
    # Custom theme elements
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 14),
                   text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 16))
  
  # Return created graph
  return(q) }

# End ----