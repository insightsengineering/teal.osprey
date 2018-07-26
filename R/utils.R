
#'
#' Utility function for quick filter
#' 
#'
#' @param filter_opt vector of string names of flag variable to filter (keep Y rows only)
#' @param ANL input dataset
#' 
#' @return a filtered dataframe
#' 
#' @template author_zhanc107
#'

quick_filter <- function(filter_opt, ANL){

  for(i in seq(1:length(filter_opt))){
    ANL <- ANL[ANL[, filter_opt[i]] == "Y", ]
  }
  
  return(ANL)
  
}


