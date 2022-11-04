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
#' @param label (`character(1)`)\cr
#'  menu item label of the module in the teal app.
#'
#' @param dataname (\code{character(1)})\cr
#'  analysis data used in the teal module, needs to be
#'  available in the list passed to the `data` argument of [teal::init()].
#'
#' @param arm_var (`choices_selected`)\cr
#'  object with all available choices and the pre-selected option for variable
#'  names that can be used as `arm_var`. See [teal.transform::choices_selected()] for
#'  details. Column `arm_var` in the `dataname` has to be a factor.
#'
#' @param paramcd (`character(1)` or `choices_selected`)\cr
#'  variable value designating the studied parameter.
#'  See [teal.transform::choices_selected()] for details.
#'
#' @param fontsize (`numeric(1)` or `numeric(3)`)\cr
#'  Defines initial possible range of font-size. `fontsize` is set for
#'  [teal.widgets::optionalSliderInputValMinMax()] which controls font-size in the output
#'  plot.
#'
#' @param plot_height (`numeric(3)`)\cr
#'  vector to indicate default value, minimum and maximum values.
#'
#' @param plot_width (`numeric(3)`)\cr
#'  vector to indicate default value, minimum and maximum values.
#'
#' @return the [teal::module()] object.
#'
#' @name argument_convention
#' @keywords internal
#'
NULL
