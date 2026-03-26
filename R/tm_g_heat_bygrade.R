#' Teal module for the heatmap by grade
#'
#' @description
#'
#' Display the heatmap by grade as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param sl_dataname (`character`) subject level dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()]
#' @param ex_dataname (`character`) exposures dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()] \cr
#' @param ae_dataname (`character`) adverse events dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()] \cr
#' @param cm_dataname (`character`) concomitant medications dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()] \cr
#' specify to `NA` if no concomitant medications data is available
#' @param id_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) unique subject ID variable
#' @param visit_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) analysis visit variable
#' @param ongo_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) study ongoing status variable.
#' This variable is a derived logical variable. Usually it can be derived from `EOSSTT`.
#' @param anno_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) annotation variable
#' @param heat_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) heatmap variable
#' @param conmed_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) concomitant medications variable,
#' specify to `NA` if no concomitant medications data is available
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
tm_g_heat_bygrade <- function(
  label,
  sl_dataname,
  ex_dataname,
  ae_dataname,
  cm_dataname = NA,
  id_var,
  visit_var,
  ongo_var,
  anno_var,
  heat_var,
  conmed_var = NULL,
  fontsize,
  plot_height,
  plot_width = NULL,
  transformators = list()
) {
  message("Initializing tm_g_heat_bygrade")
  UseMethod("tm_g_heat_bygrade", id_var)
}


#' @rdname tm_g_heat_bygrade
#' @examples
#' # Using default (choices selected) method
#' data <- teal_data() %>%
#'   within({
#'     library(dplyr)
#'     library(nestcolor)
#'     ADSL <- rADSL %>% slice(1:30)
#'     ADEX <- rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADAE <- rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADCM <- rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
#'     # This preprocess is only to force legacy standard on ADCM
#'     ADCM <- ADCM %>%
#'       select(-starts_with("ATC")) %>%
#'       unique()
#'     # function to derive AVISIT from ADEX
#'     .add_visit <- function(data_need_visit) {
#'       visit_dates <- ADEX %>%
#'         filter(PARAMCD == "DOSE") %>%
#'         distinct(USUBJID, AVISIT, ASTDTM) %>%
#'         group_by(USUBJID) %>%
#'         arrange(ASTDTM) %>%
#'         mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
#'         rename(this_vis = ASTDTM)
#'       data_visit <- data_need_visit %>%
#'         select(USUBJID, ASTDTM) %>%
#'         left_join(visit_dates, by = "USUBJID") %>%
#'         filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
#'         left_join(data_need_visit) %>%
#'         distinct()
#'       return(data_visit)
#'     }
#'     # derive AVISIT for ADAE and ADCM
#'     ADAE <- .add_visit(ADAE)
#'     ADCM <- .add_visit(ADCM)
#'     # derive ongoing status variable for ADEX
#'     ADEX <- ADEX %>%
#'       filter(PARCAT1 == "INDIVIDUAL") %>%
#'       mutate(ongo_status = (EOSSTT == "ONGOING"))
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADCM <- data[["ADCM"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_heat_bygrade(
#'       label = "Heatmap by grade",
#'       sl_dataname = "ADSL",
#'       ex_dataname = "ADEX",
#'       ae_dataname = "ADAE",
#'       cm_dataname = "ADCM",
#'       id_var = choices_selected(
#'         selected = "USUBJID",
#'         choices = c("USUBJID", "SUBJID")
#'       ),
#'       visit_var = choices_selected(
#'         selected = "AVISIT",
#'         choices = c("AVISIT")
#'       ),
#'       ongo_var = choices_selected(
#'         selected = "ongo_status",
#'         choices = c("ongo_status")
#'       ),
#'       anno_var = choices_selected(
#'         selected = c("SEX", "COUNTRY"),
#'         choices = c("SEX", "COUNTRY", "USUBJID")
#'       ),
#'       heat_var = choices_selected(
#'         selected = "AETOXGR",
#'         choices = c("AETOXGR")
#'       ),
#'       conmed_var = choices_selected(
#'         selected = "CMDECOD",
#'         choices = c("CMDECOD")
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
tm_g_heat_bygrade.default <- function(label, # nolint: object_name_linter.
                                      sl_dataname,
                                      ex_dataname,
                                      ae_dataname,
                                      cm_dataname = NA,
                                      id_var,
                                      visit_var,
                                      ongo_var,
                                      anno_var,
                                      heat_var,
                                      conmed_var = NULL,
                                      fontsize = c(5, 3, 7),
                                      plot_height = c(600L, 200L, 2000L),
                                      plot_width = NULL,
                                      transformators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_string(sl_dataname)
  checkmate::assert_string(ex_dataname)
  checkmate::assert_string(ae_dataname)
  checkmate::assert_string(cm_dataname, na.ok = TRUE)
  checkmate::assert_class(id_var, classes = "choices_selected")
  checkmate::assert_class(visit_var, classes = "choices_selected")
  checkmate::assert_class(ongo_var, classes = "choices_selected")
  checkmate::assert_class(anno_var, classes = "choices_selected")
  checkmate::assert_class(heat_var, classes = "choices_selected")
  checkmate::assert_class(conmed_var, classes = "choices_selected", null.ok = TRUE)
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
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  id_var <- teal.picks::as.picks(id_var)
  visit_var <- teal.picks::as.picks(visit_var)
  ongo_var <- teal.picks::as.picks(ongo_var)
  anno_var <- teal.picks::as.picks(anno_var)
  heat_var <- teal.picks::as.picks(heat_var)
  if (!is.null(conmed_var)) conmed_var <- teal.picks::as.picks(conmed_var)

  tm_g_heat_bygrade.pick(
    label = label,
    sl_dataname = sl_dataname,
    ex_dataname = ex_dataname,
    ae_dataname = ae_dataname,
    cm_dataname = cm_dataname,
    id_var = id_var,
    visit_var = visit_var,
    ongo_var = ongo_var,
    anno_var = anno_var,
    heat_var = heat_var,
    conmed_var = conmed_var,
    fontsize = fontsize,
    plot_height = plot_height,
    plot_width = plot_width,
    transformators = transformators
  )
}
