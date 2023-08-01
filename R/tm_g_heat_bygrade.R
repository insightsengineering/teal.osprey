#' Teal module for the heatmap by grade
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Display the heatmap by grade as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param sl_dataname (\code{character}) subject level dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}}
#' @param ex_dataname (\code{character}) exposures dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' @param ae_dataname (\code{character}) adverse events dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' @param cm_dataname (\code{character}) concomitant medications dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no concomitant medications data is available
#' @param id_var (\code{choices_seleced}) unique subject ID variable
#' @param visit_var (\code{choices_seleced}) analysis visit variable
#' @param ongo_var (\code{choices_seleced}) study ongoing status variable,
#' This variable is a derived logical variable. Usually it can be derived from \code{EOSSTT}.
#' @param anno_var (\code{choices_seleced}) annotation variable
#' @param heat_var (\code{choices_seleced}) heatmap variable
#' @param conmed_var (\code{choices_seleced}) concomitant medications variable,
#' specify to \code{NA} if no concomitant medications data is available
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(nestcolor)
#' ADSL <- osprey::rADSL %>% slice(1:30)
#' ADEX <- osprey::rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADAE <- osprey::rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADCM <- osprey::rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
#'
#' # This preprocess is only to force legacy standard on ADCM
#' ADCM <- ADCM %>%
#'   select(-starts_with("ATC")) %>%
#'   unique()
#'
#' # function to derive AVISIT from ADEX
#' add_visit <- function(data_need_visit) {
#'   visit_dates <- ADEX %>%
#'     filter(PARAMCD == "DOSE") %>%
#'     distinct(USUBJID, AVISIT, ASTDTM) %>%
#'     group_by(USUBJID) %>%
#'     arrange(ASTDTM) %>%
#'     mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
#'     rename(this_vis = ASTDTM)
#'   data_visit <- data_need_visit %>%
#'     select(USUBJID, ASTDTM) %>%
#'     left_join(visit_dates, by = "USUBJID") %>%
#'     filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
#'     left_join(data_need_visit) %>%
#'     distinct()
#'   return(data_visit)
#' }
#' # derive AVISIT for ADAE and ADCM
#' ADAE <- add_visit(ADAE)
#' ADCM <- add_visit(ADCM)
#' # derive ongoing status variable for ADEX
#' ADEX <- ADEX %>%
#'   filter(PARCAT1 == "INDIVIDUAL") %>%
#'   mutate(ongo_status = (EOSSTT == "ONGOING"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADEX", ADEX),
#'     cdisc_dataset("ADAE", ADAE),
#'     cdisc_dataset("ADCM", ADCM, keys = c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "CMDECOD")),
#'     code = "
#'     ADSL <- osprey::rADSL %>% slice(1:30)
#'     ADEX <- osprey::rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADAE <- osprey::rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADCM <- osprey::rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADCM <- ADCM %>% select(-starts_with(\"ATC\")) %>% unique()
#'     ADEX  <- ADEX %>%
#'       filter(PARCAT1 == 'INDIVIDUAL') %>%
#'       mutate(ongo_status = (EOSSTT == 'ONGOING'))
#'     add_visit <- function(data_need_visit) {
#'       visit_dates <- ADEX %>%
#'         filter(PARAMCD == 'DOSE') %>%
#'         distinct(USUBJID, AVISIT, ASTDTM) %>%
#'         group_by(USUBJID) %>%
#'         arrange(ASTDTM) %>%
#'         mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
#'         rename(this_vis = ASTDTM)
#'       data_visit <- data_need_visit %>%
#'         select(USUBJID, ASTDTM) %>%
#'         left_join(visit_dates, by = 'USUBJID') %>%
#'         filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
#'         left_join(data_need_visit) %>% distinct()
#'       return(data_visit)
#'     }
#'     ADAE <- add_visit(ADAE)
#'     ADCM <- add_visit(ADCM)
#'     ",
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_heat_bygrade(
#'       label = "Heatmap by grade",
#'       sl_dataname = "ADSL",
#'       ex_dataname = "ADEX",
#'       ae_dataname = "ADAE",
#'       cm_dataname = "ADCM",
#'       id_var = teal.transform::choices_selected(
#'         selected = "USUBJID",
#'         choices = c("USUBJID", "SUBJID")
#'       ),
#'       visit_var = teal.transform::choices_selected(
#'         selected = "AVISIT",
#'         choices = c("AVISIT")
#'       ),
#'       ongo_var = teal.transform::choices_selected(
#'         selected = "ongo_status",
#'         choices = c("ongo_status")
#'       ),
#'       anno_var = teal.transform::choices_selected(
#'         selected = c("SEX", "COUNTRY"),
#'         choices = c("SEX", "COUNTRY", "USUBJID")
#'       ),
#'       heat_var = teal.transform::choices_selected(
#'         selected = "AETOXGR",
#'         choices = c("AETOXGR")
#'       ),
#'       conmed_var = teal.transform::choices_selected(
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
tm_g_heat_bygrade <- function(label,
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
                              plot_width = NULL) {
  logger::log_info("Initializing tm_g_heat_bygrade")
  args <- as.list(environment())

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

  module(
    label = label,
    server = srv_g_heatmap_bygrade,
    server_args = list(
      label = label,
      sl_dataname = sl_dataname,
      ex_dataname = ex_dataname,
      ae_dataname = ae_dataname,
      cm_dataname = cm_dataname,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_heatmap_bygrade,
    ui_args = args,
    datanames = "all"
  )
}

ui_g_heatmap_bygrade <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        plot_decorate_output(id = ns(NULL))
      ),
      encoding = div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        teal.widgets::optionalSelectInput(
          ns("id_var"),
          "ID Variable",
          choices = args$id_var$choices,
          selected = args$id_var$selected,
          multiple = FALSE
        ),
        teal.widgets::optionalSelectInput(
          ns("visit_var"),
          "Visit Variable",
          choices = args$visit_var$choices,
          selected = args$visit_var$selected,
          multiple = FALSE
        ),
        teal.widgets::optionalSelectInput(
          ns("ongo_var"),
          "Study Ongoing Status Variable",
          choices = args$ongo_var$choices,
          selected = args$ongo_var$selected,
          multiple = FALSE
        ),
        teal.widgets::optionalSelectInput(
          ns("anno_var"),
          "Annotation Variables",
          choices = args$anno_var$choices,
          selected = args$anno_var$selected,
          multiple = TRUE
        ),
        teal.widgets::optionalSelectInput(
          ns("heat_var"),
          "Heat Variable",
          choices = args$heat_var$choices,
          selected = args$heat_var$selected,
          multiple = FALSE
        ),
        helpText("Plot conmed"),
        div(
          class = "pretty-left-border",
          if (!is.na(args$cm_dataname)) {
            checkboxInput(
              ns("plot_cm"),
              "Yes",
              value = !is.na(args$cm_dataname)
            )
          }
        ),
        conditionalPanel(
          paste0("input['", ns("plot_cm"), "']"),
          teal.widgets::optionalSelectInput(
            ns("conmed_var"),
            "Conmed Variable",
            choices = args$conmed_var$choices,
            selected = args$conmed_var$selected,
            multiple = FALSE
          ),
          selectInput(
            ns("conmed_level"),
            "Conmed Levels",
            choices = args$conmed_var$choices,
            selected = args$conmed_var$selected,
            multiple = TRUE
          )
        ),
        ui_g_decorate(
          ns(NULL),
          fontsize = args$fontsize,
          titles = "Heatmap by Grade",
          footnotes = ""
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      )
    )
  )
}

srv_g_heatmap_bygrade <- function(id,
                                  data,
                                  filter_panel_api,
                                  reporter,
                                  sl_dataname,
                                  ex_dataname,
                                  ae_dataname,
                                  cm_dataname,
                                  label,
                                  plot_height,
                                  plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")
  if (!is.na(sl_dataname)) checkmate::assert_names(sl_dataname, subset.of = names(data))
  if (!is.na(ex_dataname)) checkmate::assert_names(ex_dataname, subset.of = names(data))
  if (!is.na(ae_dataname)) checkmate::assert_names(ae_dataname, subset.of = names(data))
  if (!is.na(cm_dataname)) checkmate::assert_names(cm_dataname, subset.of = names(data))

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      ADSL <- data[[sl_dataname]]() # nolint
      ADEX <- data[[ex_dataname]]() # nolint
      ADAE <- data[[ae_dataname]]() # nolint
      if (isTRUE(input$plot_cm)) {
        ADCM <- data[[cm_dataname]]() # nolint
      }

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("id_var", shinyvalidate::sv_required(
        message = "ID Variable is required"
      ))
      iv$add_rule("visit_var", shinyvalidate::sv_required(
        message = "Visit Variable is required"
      ))
      iv$add_rule("ongo_var", shinyvalidate::sv_required(
        message = "Study Ongoing Status Variable is required"
      ))
      iv$add_rule("ongo_var", shinyvalidate::sv_in_set(
        set = names(ADEX),
        message_fmt = sprintf("Study Ongoing Status must be a variable in %s", ex_dataname)
      ))
      iv$add_rule("ongo_var", ~ if (!is.logical(ADEX[[req(.)]])) {
        "Study Ongoing Status must be a logical variable"
      })
      iv$add_rule("anno_var", shinyvalidate::sv_required(
        message = "Annotation Variables is required"
      ))
      iv$add_rule("anno_var", ~ if (length(.) > 2L) {
        "No more than two Annotation Variables are allowed"
      })
      iv$add_rule("anno_var", shinyvalidate::sv_in_set(
        set = names(ADSL),
        message_fmt = sprintf("Study Ongoing Status must be a variable in %s", sl_dataname)
      ))
      iv$add_rule("anno_var", ~ if (isTRUE(input$id_var %in% .)) {
        sprintf("Deselect %s in Annotation Variables", input$id_var)
      })
      iv$add_rule("heat_var", shinyvalidate::sv_required(
        message = "Heat Variable is required"
      ))
      iv$enable()
      iv
    })
    iv_cm <- reactive({
      ADSL <- data[[sl_dataname]]() # nolint
      ADEX <- data[[ex_dataname]]() # nolint
      ADAE <- data[[ae_dataname]]() # nolint
      if (isTRUE(input$plot_cm)) {
        ADCM <- data[[cm_dataname]]() # nolint
      }

      iv_cm <- shinyvalidate::InputValidator$new()
      iv_cm$condition(~ isTRUE(input$plot_cm))
      iv_cm$add_rule("conmed_var", shinyvalidate::sv_required(
        message = "Conmed Variable is required"
      ))
      iv_cm$add_rule("conmed_var", shinyvalidate::sv_in_set(
        set = names(ADCM),
        message_fmt = sprintf("Conmed Variable must be a variable in %s", cm_dataname)
      ))
      iv_cm$add_rule("conmed_var", ~ if (!is.factor(ADCM[[.]])) {
        "Study Ongoing Status must be a factor variable"
      })
      iv_cm$add_rule("conmed_level", shinyvalidate::sv_required(
        "Select Conmed Levels"
      ))
      iv_cm$add_rule("conmed_level", ~ if (length(.) > 3L) {
        "No more than three Conmed Levels are allowed"
      })
      iv_cm$enable()
      iv_cm
    })

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    ) # nolint
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    if (!is.na(cm_dataname)) {
      observeEvent(input$conmed_var, {
        ADCM <- data[[cm_dataname]]() # nolint
        choices <- levels(ADCM[[input$conmed_var]])

        updateSelectInput(
          session,
          "conmed_level",
          selected = choices[1:3],
          choices = choices
        )
      })
    }

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        ADSL <- data[[sl_dataname]]() # nolint
        ADEX <- data[[ex_dataname]]() # nolint
        ADAE <- data[[ae_dataname]]() # nolint

        teal::validate_has_data(ADSL, min_nrow = 1, msg = sprintf("%s contains no data", sl_dataname))
        teal::validate_inputs(iv(), iv_cm())
        if (isTRUE(input$plot_cm)) {
          shiny::validate(shiny::need(all(input$conmed_level %in% ADCM[[input$conmed_var]]), "Updating Conmed Levels"))
        }

        qenv <- teal.code::new_qenv(tdata2env(data), code = teal::get_code_tdata(data))
        if (isTRUE(input$plot_cm)) {
          ADCM <- data[[cm_dataname]]() # nolint
          qenv <- teal.code::eval_code(
            qenv,
            code = substitute(
              expr = {
                conmed_data <- ADCM %>%
                  filter(conmed_var_name %in% conmed_level)
                conmed_data[[conmed_var]] <-
                  factor(conmed_data[[conmed_var]], levels = unique(conmed_data[[conmed_var]]))
                formatters::var_labels(conmed_data)[conmed_var] <-
                  formatters::var_labels(ADCM, fill = FALSE)[conmed_var]
              },
              env = list(
                ADCM = as.name(cm_dataname),
                conmed_var = input$conmed_var,
                conmed_var_name = as.name(input$conmed_var),
                conmed_level = input$conmed_level
              )
            )
          )
        }

        qenv <- teal.code::eval_code(
          qenv,
          code = bquote(
            plot <- osprey::g_heat_bygrade(
              id_var = .(input$id_var),
              exp_data = .(as.name(ex_dataname)) %>% filter(PARCAT1 == "INDIVIDUAL"),
              visit_var = .(input$visit_var),
              ongo_var = .(input$ongo_var),
              anno_data = .(as.name(sl_dataname))[c(.(input$anno_var), .(input$id_var))],
              anno_var = .(input$anno_var),
              heat_data = .(as.name(ae_dataname)) %>%
                select(.(as.name(input$id_var)), .(as.name(input$visit_var)), .(as.name(input$heat_var))),
              heat_color_var = .(input$heat_var),
              conmed_data = .(if (isTRUE(input$plot_cm)) as.name("conmed_data")),
              conmed_var = .(if (isTRUE(input$plot_cm)) input$conmed_var),
            )
          )
        )
        teal.code::eval_code(qenv, quote(plot))
      })
    )

    plot_r <- reactive(output_q()[["plot"]])

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      title = paste("R code for", label),
      verbatim_content = reactive(teal.code::get_code(output_q()))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Heatmap by Grade")
        card$append_text("Heatmap by Grade", "header2")
        if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(output_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
