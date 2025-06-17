#' @title Summarize Survey Data for a Question with Multiple Categories
#' 
#' @description Some questions include multiple categories and answers within those categories. E.g., 'how much time did you spend on X, Y, and Z?' would have a range of answers for each of X, Y, and Z. This function accepts a dataframe for a question like that and gets it into a format where it's ready for graphing.
#' 
#' @param df (data.frame-like) Full survey data
#' @param q_stem (character) Shared characters in all column names relating to this question (note this is only possible because of the specific wrangling done during QC). At least one column with that string must be found in 'df'.
#' @param grp (character) One of either "global" or "cohort" for the desired level of summarization
#' @param excl_qs (character) _Optional!_ 'Questions' (i.e., column names that match 'q_stem') to exclude _before_ calculating percent responses
#'
#' @return (data.frame) Summarized dataframe of responses within the specified grouping structure
#' 
multi_cat_prep <- function(df = NULL, q_stem = NULL, grp = "cohort", excl_qs = NULL){
  
  # Errors for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided and be data.frame-like")
  
  # Errors for 'q_stem'
  if(is.null(q_stem) || is.character(q_stem) != TRUE || length(q_stem) != 1 || any(stringr::str_detect(string = names(df), pattern = q_stem)) != T)
    stop("'q_stem' must be a single character value matching some columns found in 'df'")
  
  # Handle NULL value for 'excl_qs'
  if(is.null(excl_qs)){ excl_qs <- "NOTHING" }
  
  # Do prep that applies regardless of 'grp' choice
  df_v2 <- df %>% 
    # Pare down to desired columns only
    dplyr::select(cohort, survey_type, dplyr::contains(q_stem)) %>% 
    # Pivot to long format
    tidyr::pivot_longer(cols = -cohort:-survey_type, 
                        names_to = 'question', 
                        values_to = 'answer') %>%
    # Filter out non-responses (i.e., NAs)
    dplyr::filter(!is.na(answer) & nchar(as.character(answer)) != 0) %>% 
    # Drop questions that match 'excl_qs'
    dplyr::filter(!question %in% excl_qs)
  
  # Make a list for storing outputs
  q_list <- list()
  
  # Iterate across questions
  for(focal_q in sort(unique(df_v2$question))){
    
    # Do needed wrangling
    df_sub <- df_v2 %>% 
      # Subset to just this question
      dplyr::filter(question == focal_q) %>% 
      # Prepare for survey graphing
      survey_prep(df = ., resp = "answer", grp = grp) %>% 
      # Add the question back in
      dplyr::mutate(question = focal_q)
    
    # Add to question list
    q_list[[focal_q]] <- df_sub
    
  } # Close loop
  
  # Unlist the list
  df_v3 <- purrr::list_rbind(x = q_list)
  
  # Return 'prepared' data
  return(df_v3) }

# End ----
