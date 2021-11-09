#' Shared Parameters
#'
#' @description Contains arguments that are shared between multiple functions
#'   in the package to avoid repetition using \code{inheritParams}.
#'
#' @param plot_height optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the height of the main plot.
#' @param plot_width optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the width of the main plot and renders a slider on the plot to interactively adjust the plot width.
#' @param label (\code{character}) module label in the teal app. Please note that this module is developed based on
#' ADaM data structure and ADaM variables.
#'
#' @name shared_params
NULL

#'
#' Utility function for quick filter
#'
#'
#' @param filter_opt vector of string names of flag variable to filter (keep Y rows only)
#' @param ANL input dataset
#'
#' @return a filtered dataframe
#'
#' @export
#'
#' @template author_zhanc107
#'

quick_filter <- function(filter_opt, ANL) { # nolint
  for (i in seq_along(filter_opt)) {
    ANL <- ANL[ANL[, filter_opt[i]] == "Y", ] # nolint
  }
  return(ANL)
}

#' Automatically switch variable labels for standard AE variables in AE osprey functions
#'
#' @param x variable key
#'
#' @export
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

#' retrieve name of ci method
#' @param x ci method to retrieve its name
name_ci <- function(x) {
  names(ci_choices)[which(ci_choices == x)]
}

ci_choices <- setNames(
  c("wald", "waldcc", "ac", "scorecc", "score", "mn", "mee", "blj", "ha"),
  c(
    "Wald", "Corrected Wald", "Agresti-Caffo", "Newcombe",
    "Score", "Miettinen and Nurminen", "Mee",
    "Brown, Li's Jeffreys", "Hauck-Anderson"
    )
  )

#' retrieve detailed name of ci method
#' @param x ci method to retrieve its name
name_ci <- function(x = ci_choices) {
  x <- match.arg(x)
  return(paste0(names(x), " (", x, ")"))
}


#' takes input_string, splits by ","  and returns a numeric vector
#' with NAs where the split-strings are not numeric.
#' e.g. as_numeric_from_comma_separated_string("4  ,hello,5,, 3")
#' is c(4, NA, 5, NA, 3).
#' If input argument is NULL or just whitespace then NULL is returned
#' @param input_string string to be split into numeric vector
as_numeric_from_comma_sep_str <- function(input_string) {
  if (!is.null(input_string) && trimws(input_string) != "") {
    ref_line <- unlist(strsplit(trimws(input_string), ","))
    ref_line <- as.numeric(ref_line)
  } else {
    ref_line <- NULL
  }
  return(ref_line)
}