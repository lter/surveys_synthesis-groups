#' @title Pull Free Text Column from Survey Data
#' 
#' @description Strips a single column--typically a 'free text' response--and prepares it for inclusion in a report. This involves removing empty responses and pre-pending the cohort number so that responses sort in an intuitive way. Additionally, strings are wrapped so they don't run outside of the page margins in the report
#' 
#' @param df (data.frame-like) Survey data containing--at least--a column named "cohort"
#' @param text (character) Single column name in 'df' corresponding to the column to be extracted. Typically this is a 'free text' question
#' 
#' @return (character) Vector of colun contents
#' 
pull_text <- function(df = NULL, text = NULL){
  
  # Errors for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE || "cohort" %in% names(df) != TRUE)
    stop("'df' must be data.frame-like and contain a 'cohort' column")
  
  # Errors for 'text'
  if(is.null(text) || is.character(text) != TRUE || length(text) != 1 || !text %in% names(df))
    stop("'text' must be a single column name found in 'df'")
  
  # Extract desired column
  words <- df %>% 
    # Keep only rows with an answer
    dplyr::filter(!is.na(!!rlang::ensym(text)) & nchar(!!rlang::ensym(text)) != 0) %>% 
    # Paste together cohort with text
    dplyr::mutate(answer = paste0(cohort, " - ", !!rlang::ensym(text))) %>% 
    # Pull out that column as a vector
    dplyr::pull() %>% 
    # Sort it
    sort() %>% 
    # Now reverse it
    rev() %>% 
    # Wrap text
    base::strwrap(x = .) %>% 
    # Drop empty characters
    setdiff(x = ., y = c(""))
  
  # If there are any responses, return them
  if(length(words) >= 1){
    return(words)
    
    # Otherwise, return a boilerplate message
  } else {  
    return("No free text responses to this question.") 
  } 
}

# End ----
