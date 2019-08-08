
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


#Automatically switch variable labels for standard AE variables in AE osprey functions
label_aevar <- function(x) {
  #Display full variable labels for standard AE variables
  ae_varlabel <- c(AEBODSYS = "MedDRA System Organ Class",
                   AESOC    = "MedDRA Primary System Organ Class",
                   AEHLGT   = "MedDRA High Level Group Term",
                   AEHLT    = "MedDRA High Level Term",
                   AELLT    = "MedDRA Lowest Level Term",
                   AEDECOD  = "MedDRA Preferred Term",
                   AETERM   = "Reported Adverse Event Term",
                   AEMODIFY = "Modified Reported Term",
                   AETOXGR  = "NCI-CTCAE Grade",
                   AEITOXGR = "Initial Toxicity Grade"
  )

  which_aevar <- match(x, names(ae_varlabel))
  out_label <- ifelse(is.na(which_aevar), x, ae_varlabel[which_aevar])
  return(out_label)
}
