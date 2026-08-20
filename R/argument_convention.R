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
#' @param dataname (`character(1)`)\cr
#'  analysis data used in the teal module, needs to be
#'  available in the list passed to the `data` argument of [teal::init()].
#'
#' @param parentname (`character(1)`)\cr
#'  analysis data used for several variables in the teal module, needs to be
#'  available in the list passed to the `data` argument of [teal::init()]. The default is
#' `"ADSL"`
#'
#' @param arm_var Either a ([`teal.picks::variables()`]) object or a
#'  ([`teal.transform::choices_selected()`]) object.\cr
#'  `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'  Object with all available choices and the pre-selected option for variable
#'  names that can be used as `arm_var`. Column `arm_var` in the `dataname`
#'  has to be a factor.
#'
#' @param paramcd Either a ([`teal.picks::variables()`]) object or a
#'  ([`teal.transform::choices_selected()`]) object.\cr
#'  `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'  Variable value designating the studied parameter.
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
#' @param transformators (`list` of `teal_transform_module`) optional,
#' input data transforms applied after filtering (UI in the filter sidebar under
#' **Transform Data**). See `vignette("transform-input-data", package = "teal")`.
#'
#' @param decorators `r lifecycle::badge("experimental")`
#' (named `list` of `teal_transform_module`) optional,
#' decorators for the module `plot` output.
#'
#' @return the [teal::module()] object.
#'
#' @name argument_convention
#' @keywords internal
#'
NULL
