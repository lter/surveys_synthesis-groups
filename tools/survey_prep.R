#' @title Summarize Survey Data for a Specific Question
#' 
#' @description Virtually all report graphs use participants per cohort or % responses per cohort. This function does that summarization quickly and (more or less) easily.
#' 
#' @param df (data.frame-like) Full survey data
#' @param resp (character) Single column name in 'df' corresponding to the question for which summarization is desired
#' @param grp (character) One of either "global" or "cohort" for the desired level of summarization
#'
#' @return (data.frame) Summarized dataframe of responses within the specified grouping structure
#' 
survey_prep <- function(df = NULL, resp = NULL, grp = "cohort"){
  
  # Errors for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided and be data.frame-like")
  
  # Errors for 'resp'
  if(is.null(resp) || is.character(resp) != TRUE || length(resp) != 1 || !resp %in% names(df))
    stop("'resp' must be a single column name found in 'df'")
  
  # Errors for 'grp'
  if(is.null(grp) || is.character(grp) != TRUE || length(grp) != 1 || !grp %in% c("global", "cohort"))
    stop("'grp' must be one of either 'global' or 'cohort'")
  
  # Do prep that applies regardless of 'grp' choice
  df_v2 <- df %>% 
    dplyr::filter(!is.na(!!rlang::ensym(resp)) & 
                    nchar(!!rlang::ensym(resp)) != 0)
  
  # Then handle other stuff conditionally
  if(grp == "global"){
    
    # Do it    
    df_v3 <- df_v2 %>% 
      # Grab relevant column(s)
      dplyr::select(dplyr::all_of(c(resp))) %>% 
      # Calculate total respondents
      dplyr::mutate(total_particip = dplyr::n()) %>% 
      # Calculate responses per response category
      dplyr::group_by(!!rlang::ensym(resp)) %>% 
      dplyr::summarize(total_particip = dplyr::first(total_particip),
                       count = dplyr::n()) %>%
      dplyr::ungroup() %>% 
      # And calculate percent of responses per that category
      dplyr::mutate(perc_resp = (count / total_particip) * 100)
    
  } else if(grp == "cohort"){
    
    # Do it    
    df_v3 <- df_v2 %>% 
      # Grab relevant column(s)
      dplyr::select(dplyr::all_of(c("cohort", resp))) %>% 
      # Calculate total respondents (per cohort)
      dplyr::group_by(cohort) %>% 
      dplyr::mutate(total_particip = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      # Calculate responses per response category
      dplyr::group_by(cohort, !!rlang::ensym(resp)) %>% 
      dplyr::summarize(total_particip = dplyr::first(total_particip),
                       count = dplyr::n(),
                       .groups = "keep") %>%
      dplyr::ungroup() %>% 
      # And calculate percent of responses per that category
      dplyr::mutate(perc_resp = (count / total_particip) * 100) %>% 
      # Compute total responses / category (**regardless of cohort**)
      dplyr::group_by(!!rlang::ensym(resp)) %>% 
      dplyr::mutate(cat_total = sum(count, na.rm = T)) %>% 
      dplyr::ungroup()
    
  }
  
  # Return 'prepared' data
  return(df_v3) }

# End ----
