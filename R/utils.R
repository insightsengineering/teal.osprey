#' Shared Parameters
#'
#' @description Contains arguments that are shared between multiple functions
#'   in the package to avoid repetition using `inheritParams`.
#'
#' @param plot_height (`numeric`) optional vector of length three with `c(value, min, max)`. Specifies
#'   the height of the main plot.
#' @param plot_width (`numeric`) optional vector of length three with `c(value, min, max)`. Specifies
#'   the width of the main plot and renders a slider on the plot to interactively adjust the plot width.
#' @param label (`character`) module label in the teal app. Please note that this module is developed based on
#' `ADaM` data structure and `ADaM` variables.
#'
#' @name shared_params
#' @keywords internal
#'
NULL

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
quick_filter <- function(filter_opt, ANL) {
  for (i in seq_along(filter_opt)) {
    ANL <- ANL[ANL[, filter_opt[i]] == "Y", ]
  }
  ANL
}

#' Automatically switch variable labels for standard `AE` variables in `AE` osprey functions
#' `r lifecycle::badge("deprecated")`
#' `label_aevar` is deprecated and will be unexported in the next release.
#'
#' @param x variable key
#'
#' @export
label_aevar <- function(x) {
  lifecycle::deprecate_stop(
    when = "0.1.15",
    what = "label_aevar()",
    details = "label_aevar is deprecated and will be unexported in the next release."
  )
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
  ref_line
}

#' Get Choices
#'
#' This function returns choices based on the class of the input.
#' If the input is of class `delayed_data`, it returns the `subset` of the input.
#' If `subset` is NULL and the input contains `var_label` and `var_choices`,
#' it throws an error prompting to resolve delayed inputs.
#' Otherwise, it returns the input as is.
#'
#' @param choices An object that contains choices.
#' @return A vector of choices.
#' @keywords internal
get_choices <- function(choices) {
  if (inherits(choices, "delayed_data")) {
    if (is.null(choices$subset)) {
      if (!is.null(choices$var_label) && !is.null(choices$var_choices)) {
        stop(
          "Resolve delayed inputs by evaluating the code within the provided datasets.
          Check ?teal.transform::resolve_delayed for more information."
        )
      } else {
        stop("Subset is NULL and necessary fields are missing.")
      }
    } else {
      choices$subset
    }
  } else {
    choices
  }
}

#' @keywords internal
#' @noRd
left_bordered_div <- function(...) {
  tags$div(
    style = "
      border-left: 3px solid #e3e3e3;
      padding-left: 0.6em;
      border-radius: 5px;
      margin-left: -0.6em;
      margin-bottom: 0.5em;
    ",
    ...
  )
}
