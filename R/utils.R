
#'
#' Utility function for quick filter
#' 
#' How to add your own filter of interest: 1. add strings of interest under 
#' commented region 2. for more filters add strings for filter name under commented
#' region. This function is currently used in tm_t_ae to add filters on interest
#' to the dataset.
#'
#' @param filter_opt desired filter option
#' @param ANL input dataset
#' @param class_var class variable to filter
#' @param term_var term variable to filter 
#' 
#' @return a filtered dataframe
#' 
#' @template author_zhanc107
#'

quick_filter <- function(filter_opt, ANL, class_var, term_var){
  ########## [2] ADD FILTER OPTIONS-----------------------
  
  all_filter_opt <- c("filter1", "filter2")
  
  ###########################################
  filt <- match.arg(filter_opt, all_filter_opt)
  
  if(filt == all_filter_opt[1]){ # sample filter 1
    
    ######## [1] ADD CLASSES OR TERMS OF INTEREST------------
    
    class_filter <- c("Investigations")
    term_filter <- NULL# add terms of interest
    
    #########################################
    
    if(!is.null(class_filter)){
      ANL <- ANL[ANL[,class_var] %in% class_filter, ]
    }
    if(!is.null(term_filter)){
      ANL <- ANL[ANL[,term_var] %in% term_filter, ]
    }
    
  } else if(filt == all_filter_opt[2]){ #sample filter 2
    
    ########[1] ADD CLASSES OR TERMS OF INTEREST------------
    
    class_filter <- c("Investigations", "Vascular disorders", "Eye disorders")#class
    term_filter <- NULL#add terms of interest
    
    #########################################
    
    if(!is.null(class_filter)){
      ANL <- ANL[ANL[,class_var] %in% class_filter, ]
    }
    if(!is.null(term_filter)){
      ANL <- ANL[ANL[,term_var] %in% term_filter, ]
    }
  } 
  
  return(ANL)
  
}


