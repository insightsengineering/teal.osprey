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
#' `ADaM` data structure and `ADaM` variables.
#'
#' @name shared_params
#' @keywords internal
#'
NULL

#' Utility function for quick filter
#' `r lifecycle::badge("stable")`
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

#' Automatically switch variable labels for standard `AE` variables in `AE` osprey functions
#' `r lifecycle::badge("stable")`
#'
#' @param x variable key
#'
#' @export
label_aevar <- function(x) {
  lifecycle::deprecate_soft(
    when = "0.1.15",
    what = "label_aevar()",
    details = "label_aevar is deprecated and will be unexported in the next release."
  )

  # Display full variable labels for standard AE variables
  ae_varlabel <- c(
    AEBODSYS = "MedDRA System Organ Class",
    AESOC = "MedDRA Primary System Organ Class",
    AEHLGT = "MedDRA High Level Group Term",
    AEHLT = "MedDRA High Level Term",
    AELLT = "MedDRA Lowest Level Term",
    AEDECOD = "MedDRA Preferred Term",
    AETERM = "Reported Adverse Event Term",
    AEMODIFY = "Modified Reported Term",
    AETOXGR = "NCI-CTCAE Grade",
    AEITOXGR = "Initial Toxicity Grade"
  )

  which_aevar <- match(x, names(ae_varlabel))
  out_label <- ifelse(is.na(which_aevar), x, ae_varlabel[which_aevar])
  return(out_label)
}

#' retrieve name of ci method
#' @param x ci method to retrieve its name
#' @keywords internal
#'
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
#' @keywords internal
#'
as_numeric_from_comma_sep_str <- function(input_string) {
  if (!is.null(input_string) && trimws(input_string) != "") {
    ref_line <- unlist(strsplit(trimws(input_string), ","))
    ref_line <- as.numeric(ref_line)
  } else {
    ref_line <- NULL
  }
  return(ref_line)
}

#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method.
#'
#' @param pattern (`character`) pattern of files to be included
#'
#' @return HTML code that includes `CSS` files
#' @keywords internal
include_css_files <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "teal.osprey", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  if (length(css_files) == 0) {
    return(NULL)
  }
  return(shiny::singleton(shiny::tags$head(lapply(css_files, shiny::includeCSS))))
}
