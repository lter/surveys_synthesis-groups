## Virtually all report graphs are of participants / cohort or % responses / cohort
## `survey_prep` quickly/easily generates the desired summarization for a specified response variable

# Plotting prep function
survey_prep <- function(response = NULL, data = NULL, group = "cohort"){
  
  # Error out if anything isn't provided
  if(is.null(data) == TRUE | is.null(group) == TRUE | is.null(response) == TRUE)
    stop("All arguments must be specified")
  
  # Error out if groups or response are not characters
  if(is.character(response) != TRUE | is.character(group) != TRUE)
    stop("Response variable (column name) & grouping level must be supplied a characters")
  
  # Error out for unsupported group argument specification
  if(!group %in% c("cohort", "global"))
    stop("`group` must be one of: 'cohort', 'global'")
  
  # Process global summary
  if(group == "global"){
    
    # Wrangle data
    prepped_df <- data %>% 
      # Keep only response / group columns
      dplyr::select(dplyr::all_of(c(response))) %>% 
      # Filter out missing information for the given response variable
      dplyr::filter(!is.na(!!rlang::ensym(response)) & 
                      nchar(!!rlang::ensym(response)) != 0) %>% 
      # Calculate total participants
      dplyr::mutate(total_particip = dplyr::n()) %>% 
      # Calculate percent response category overall
      dplyr::group_by(!!rlang::ensym(response)) %>% 
      dplyr::summarize(total_particip = dplyr::first(total_particip),
                       count = dplyr::n()) %>%
      dplyr::mutate(perc_resp = (count / total_particip) * 100) %>% 
      # Return as dataframe
      as.data.frame()
    
  } # Close global conditional
  
  # Process cohort-level summary
  if(group == "cohort"){
    
    # Process data
    prepped_df <- data %>% 
      # Keep only response / group columns
      dplyr::select(dplyr::all_of(c(response, "cohort"))) %>% 
      # Filter out missing information for the given response variable
      dplyr::filter(!is.na(!!rlang::ensym(response)) & 
                      nchar(!!rlang::ensym(response)) != 0) %>% 
      # Calculate total participants / cohort
      dplyr::group_by(cohort) %>% 
      dplyr::mutate(total_particip = dplyr::n()) %>% 
      # Calculate percent response category within cohort
      dplyr::group_by(cohort, !!rlang::ensym(response)) %>% 
      dplyr::summarize(total_particip = dplyr::first(total_particip),
                       count = dplyr::n()) %>%
      dplyr::mutate(perc_resp = (count / total_particip) * 100) %>% 
      # Return as dataframe
      as.data.frame()
    
  } # Close cohort conditional
  
  # Return finished product
  return(prepped_df) }
