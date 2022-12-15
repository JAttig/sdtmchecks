#' @title Compare two outputs of the run_all_checks function
#'
#' @description This function compares two outputs of run_all_checks and returns which checks are identical
#' and which are not.
#'
#' @param result1 An output of run_all_checks.
#' 
#' @param result2 An output of run_all_checks.
#'
#' @details to look up documentation for the data checks package, please use command ??sdtmchecks
#'
#' @return named list with results from comparing individual checks
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 
#' # Get packages
#' library(sdtmchecks)
#' library(rice)
#' library(tidyverse)
#'
#' # Get data
#' data = rice::rice_read_all("root/clinical_studies/RO5541267/CDT30075/GO30140/data_analysis/DLR5/data/sdtmv")
#' lapply(seq_along(data), function(i) {assign(names(data)[[i]], data[[i]], envir = .GlobalEnv)})
#' 
#' # Run two types of checks
#' all_rec1 <- run_all_checks()
#' all_rec2 <- run_all_checks(type = "PRO")
#' all_rec3 <- all_rec2
#' all_rec3[[1]] = "changed"
#' 
#' compare_sdtmchecks(all_rec1, all_rec2)
#' compare_sdtmchecks(all_rec1, all_rec3)
#' }

compare_sdtmchecks <- function(result1, result2){
  
  # Get checks in common
  checks_in_1 <- names(result1)
  checks_in_2 <- names(result2)
  checks_in_common <- intersect(checks_in_1, checks_in_2)

  # Get checks unique to one of the two lists
  checks_only_in_1 <- checks_in_1[!checks_in_1 %in% checks_in_common]
  checks_only_in_2 <- checks_in_2[!checks_in_2 %in% checks_in_common]
  
  # Set up list comparing checks
  check_comparison <- list()
  
  # Compare checks in common
  for(check in checks_in_common){
    
    # below code is not yet good - need to ignore data argument as that could be different
    comparison_result <- identical(result1[[check]], result2[[check]])
    comparison_text <- ifelse(comparison_result,
                              "Identical result",
                              "Differences")
    check_comparison[[check]] <- list("Comparison_Result" = comparison_result,
                                       "Comparison_Text" = comparison_text)
    
  }
  
  # Compare checks only in 1st list
  for(check in checks_only_in_1){
    
    comparison_result <- NULL
    comparison_text <- "Check present in list 1 but not in list 2."
    check_comparison[[check]] <- list("Comparison_Result" = comparison_result,
                                      "Comparison_Text" = comparison_text)
    
  }
  
  # Compare checks only in 2nd list
  for(check in checks_only_in_2){
    
    comparison_result <- NULL
    comparison_text <- "Check present in list 2 but not in list 1."
    check_comparison[[check]] <- list("Comparison_Result" = comparison_result,
                                      "Comparison_Text" = comparison_text)
    
  }

  return(check_comparison)
}

