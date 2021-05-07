#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in teal modules
#' that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#' the teal module users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams argument_convention`
#'
#' @param label (\code{character})\cr menu item label of the module in the teal app
#' @param dataname (\code{character}) analysis data used in the teal module, needs to be
#' available in the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices
#' and the pre-selected option for variable names that can be used as \code{arm_var}.
#' @param paramcd (`string` or `choices_selected` with teal module)\cr
#'   variable value designating the studied parameter.
#' @param plot_height `numeric` vector to indicate default value, minimum and maximum values.
#' @param plot_width `numeric` vector to indicate default value, minimum and maximum values.
#' @name argument_convention
#'
NULL
