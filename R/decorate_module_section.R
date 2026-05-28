#' Decorating and transforming `teal.osprey` modules
#'
#' @description
#' Documentation for the `transformators` and `decorators` arguments accepted by all
#' `tm_g_*` modules in this package.
#'
#' @section Decorating Module:
#'
#' All `teal.osprey` plot modules expose a single decoratable output object named `plot`.
#' Decorators are passed as a named `list` of [`teal::teal_transform_module()`] objects;
#' the name of each list element must match the output object name (`"plot"`).
#'
#' ```r
#' tm_g_waterfall(
#'   ...,
#'   decorators = list(
#'     plot = teal::teal_transform_module(...) # applied only to `plot`
#'   )
#' )
#' ```
#'
#' Decorator UI controls appear in the module **encoding** panel. Decorators run in the
#' module server after the plot is created and before it is rendered.
#'
#' **Important:** decorators must not change the class of `plot`. Use transformations
#' appropriate to the object type returned by the underlying [osprey] function.
#'
#' | Module | Output | Typical class of `plot` |
#' |--------|--------|-------------------------|
#' | `tm_g_spiderplot` | `plot` | `ggplot` |
#' | `tm_g_butterfly` | `plot` | `grob` / `gtable` |
#' | `tm_g_waterfall` | `plot` | `grob` / `gtable` |
#' | `tm_g_swimlane` | `plot` | `grob` / `gtable` |
#' | `tm_g_patient_profile` | `plot` | `grob` (`cowplot` layout) |
#' | `tm_g_ae_oview` | `plot` | `grob` |
#' | `tm_g_ae_sub` | `plot` | `grob` |
#' | `tm_g_events_term_id` | `plot` | `grob` |
#' | `tm_g_heat_bygrade` | `plot` | `grob` / `gtable` |
#'
#' - For **`ggplot`** outputs (`tm_g_spiderplot` only), use `ggplot2` modifiers
#'   (for example via [`teal::make_teal_transform_server()`]).
#' - For **`grob`** outputs (all other modules), use [`tern::decorate_grob()`] or
#'   other grid-compatible adjustments. Applying `ggplot2` layers to `plot` in
#'   those modules will fail silently or break rendering.
#'
#' Four modules (`tm_g_ae_oview`, `tm_g_ae_sub`, `tm_g_events_term_id`, `tm_g_heat_bygrade`)
#' also provide built-in title and footnote controls via [`ui_g_decorate()`] and
#' [`srv_g_decorate()`]. User-defined decorators run **before** that built-in decoration step.
#'
#' @section Transforming input data:
#'
#' All `tm_g_*` modules also accept `transformators`, a named `list` of
#' [`teal::teal_transform_module()`] objects that modify module **input** data after
#' filtering. Their UI appears in the app filter sidebar under **Transform Data**.
#'
#' See `vignette("transform-input-data", package = "teal")` for transformators and
#' `vignette("transform-module-output", package = "teal")` for decorators.
#'
#' A demo app using every module with both mechanisms is in
#' `system.file("examples", "app_decorators_transformators.R", package = "teal.osprey")`.
#'
#' @name decorate_module_section
#' @keywords internal
NULL
