#' @title Teal module for the `AE` overview
#'
#' @description
#'
#' Display the `AE` overview plot as a shiny module
#'
#' This is an S3 generic that dispatches on the class of `flag_var_anl`:
#' - [choices_selected][teal.transform::choices_selected()] dispatches to the
#'   default method.
#' - [picks][teal.picks::picks()] dispatches to the picks method.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param flag_var_anl Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with variables used to count adverse event
#'   sub-groups (e.g. Serious events, Related events, etc.)
#' @param dataname (`character(1)`) Name of the events dataset. Required when
#'   using the default method with [choices_selected][teal.transform::choices_selected()].
#'   Ignored by the `.picks` method.
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @examples
#' data <- teal_data() %>%
#'   within({
#'     library(dplyr)
#'     ADSL <- rADSL
#'     ADAE <- rADAE
#'     .add_event_flags <- function(dat) {
#'       dat <- dat %>%
#'         mutate(
#'           TMPFL_SER = AESER == "Y",
#'           TMPFL_REL = AEREL == "Y",
#'           TMPFL_GR5 = AETOXGR == "5",
#'           AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
#'           AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
#'         )
#'       labels <- c(
#'         "Serious AE", "Related AE", "Grade 5 AE",
#'         "AE related to A: Drug X", "AE related to B: Placebo"
#'       )
#'       cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'       for (i in seq_along(labels)) {
#'         attr(dat[[cols[i]]], "label") <- labels[i]
#'       }
#'       dat
#'     }
#'     ADAE <- .add_event_flags(ADAE)
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADAE <- data[["ADAE"]]
#' # Using default method (choices selected)
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ae_oview(
#'       label = "AE Overview",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         selected = "ACTARM",
#'         choices = c("ACTARM", "ACTARMCD")
#'       ),
#'       flag_var_anl = choices_selected(
#'         selected = "AEREL1",
#'         choices = variable_choices(
#'           ADAE,
#'           c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'         ),
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_ae_oview <- function(label,
                          dataname,
                          arm_var,
                          flag_var_anl,
                          fontsize,
                          plot_height,
                          plot_width,
                          transformators) {
  UseMethod("tm_g_ae_oview", arm_var)
}

#' @rdname tm_g_ae_oview
#' @export
tm_g_ae_oview.default <- function(label,
                                  dataname,
                                  arm_var,
                                  flag_var_anl,
                                  fontsize = c(5, 3, 7),
                                  plot_height = c(600L, 200L, 2000L),
                                  plot_width = NULL,
                                  transformators = list()) {
  message("Initializing tm_g_ae_oview")

  checkmate::assert_class(arm_var, classes = "choices_selected")
  checkmate::assert_class(flag_var_anl, classes = "choices_selected")

  checkmate::assert(
    checkmate::check_number(fontsize, finite = TRUE),
    checkmate::assert(
      combine = "and",
      .var.name = "fontsize",
      checkmate::check_numeric(fontsize, len = 3, any.missing = FALSE, finite = TRUE),
      checkmate::check_numeric(fontsize[1], lower = fontsize[2], upper = fontsize[3])
    )
  )
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1],
    lower = plot_height[2], upper = plot_height[3],
    .var.name = "plot_height"
  )
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  tm_g_ae_oview.pick(
    label = label,
    dataname = dataname,
    arm_var = teal.picks::as.picks(arm_var),
    flag_var_anl = teal.picks::as.picks(flag_var_anl),
    fontsize,
    plot_height,
    plot_width,
    transformators
  )
}
