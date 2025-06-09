## Pulls free text from designated column
pull_text <- function(data = NULL, text = NULL){
  
  # Error out for missing column
  if(is.null(data) == TRUE | is.null(text) == TRUE)
    stop("All arguments must be provided")
  
  # Extract desired column
  words <- data %>% 
    # Keep only rows with an answer
    dplyr::filter(!is.na(!!rlang::ensym(text)) & 
                    nchar(!!rlang::ensym(text)) != 0) %>% 
    # Paste together cohort with text
    dplyr::mutate(answer = paste0(cohort, " - ", !!rlang::ensym(text))) %>% 
    # Pull out that column and sort it
    dplyr::pull() %>% sort() %>% 
    # Wrap text
    base::strwrap(x = .)
  
  # Return that
  return(words) }
