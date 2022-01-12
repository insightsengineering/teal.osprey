#' Teal module for the heatmap by grade
#'
#' Display the heatmap by grade as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
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
#' library(scda)
#' library(dplyr)
#' ADSL <- synthetic_cdisc_data("latest")$adsl %>% slice(1:30)
#' ADEX <- synthetic_cdisc_data("latest")$adex %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADAE <- synthetic_cdisc_data("latest")$adae %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADCM <- synthetic_cdisc_data("latest")$adcm %>% filter(USUBJID %in% ADSL$USUBJID)
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
#'     left_join(data_need_visit)
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
#'     ADSL <- synthetic_cdisc_data(\"latest\")$adsl %>% slice(1:30)
#'     ADEX <- synthetic_cdisc_data(\"latest\")$adex %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADAE <- synthetic_cdisc_data(\"latest\")$adae %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADCM <- synthetic_cdisc_data(\"latest\")$adcm %>% filter(USUBJID %in% ADSL$USUBJID)
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
#'         left_join(data_need_visit)
#'       return(data_visit)
#'     }
#'     ADAE <- add_visit(ADAE)
#'     ADCM <- add_visit(ADCM)
#'     ",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_heat_bygrade <- function(label,
                              sl_dataname,
                              ex_dataname,
                              ae_dataname,
                              cm_dataname,
                              id_var,
                              visit_var,
                              ongo_var,
                              anno_var,
                              heat_var,
                              conmed_var = NULL,
                              fontsize = c(5, 3, 7),
                              plot_height = c(600L, 200L, 2000L),
                              plot_width = NULL) {
  args <- as.list(environment())

  checkmate::assert_string(label)
  checkmate::assert_string(sl_dataname)
  checkmate::assert_string(ex_dataname)
  checkmate::assert_string(ae_dataname)
  checkmate::assert_string(cm_dataname, null.ok = TRUE)
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
    filters = "all"
  )
}

ui_g_heatmap_bygrade <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  standard_layout(
    output = white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = div(
      optionalSelectInput(
        ns("id_var"),
        "ID Variable",
        choices = args$id_var$choices,
        selected = args$id_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("visit_var"),
        "Visit Variable",
        choices = args$visit_var$choices,
        selected = args$visit_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("ongo_var"),
        "Study Ongoing Status Variable",
        choices = args$ongo_var$choices,
        selected = args$ongo_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("anno_var"),
        "Annotation Variables",
        choices = args$anno_var$choices,
        selected = args$anno_var$selected,
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("heat_var"),
        "Heat Variable",
        choices = args$heat_var$choices,
        selected = args$heat_var$selected,
        multiple = FALSE
      ),
      helpText("Plot conmed"),
      div(
        style =
          "border-left: 3px solid #e3e3e3;
        padding-left: 0.6em;
        border-radius: 5px;
        margin-left: -0.6m;",
        uiOutput(ns("plot_cm_output"))
      ),
      conditionalPanel(
        paste0("input['", ns("plot_cm"), "']"),
        optionalSelectInput(
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
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_g_heatmap_bygrade <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  sl_dataname,
                                  ex_dataname,
                                  ae_dataname,
                                  cm_dataname,
                                  label,
                                  plot_height,
                                  plot_width) {
  init_chunks()
  font_size <- callModule(srv_g_decorate, id = NULL, plt = plt, plot_height = plot_height, plot_width = plot_width) # nolint

  observeEvent(cm_dataname, {
    if (!is.na(cm_dataname)) {
      output$plot_cm_output <- renderUI({
        checkboxInput(
          session$ns("plot_cm"),
          "Yes",
          value = !is.na(cm_dataname)
        )
      })
    }
  })

  observeEvent(input$plot_cm, {
    ADCM_FILTERED <- datasets$get_data(cm_dataname, filtered = TRUE) # nolint
    ADCM_label <- rtables::var_labels(datasets$get_data(cm_dataname, filtered = FALSE)) # nolint
    rtables::var_labels(ADCM_FILTERED) <- ADCM_label
    choices <- levels(ADCM_FILTERED[[input$conmed_var]])

    updateSelectInput(
      session,
      "conmed_level",
      selected = choices[1:3],
      choices = choices
    )
  })

  plt <- reactive({
    validate(need(input$id_var, "Please select a ID variable."))
    validate(need(input$visit_var, "Please select a visit variable."))
    validate(need(input$ongo_var, "Please select a Study Ongoing Status variable."))
    validate(need(input$heat_var, "Please select a heat variable."))
    validate(need(length(input$anno_var) <= 2, "Please include no more than 2 annotation variables"))

    ADSL_FILTERED <- datasets$get_data(sl_dataname, filtered = TRUE) # nolint
    ADEX_FILTERED <- datasets$get_data(ex_dataname, filtered = TRUE) # nolint
    ADAE_FILTERED <- datasets$get_data(ae_dataname, filtered = TRUE) # nolint

    # assign labels back to the data
    rtables::var_labels(ADSL_FILTERED) <-
      rtables::var_labels(datasets$get_data(sl_dataname, filtered = FALSE))
    rtables::var_labels(ADEX_FILTERED) <-
      rtables::var_labels(datasets$get_data(ex_dataname, filtered = FALSE))
    rtables::var_labels(ADAE_FILTERED) <-
      rtables::var_labels(datasets$get_data(ae_dataname, filtered = FALSE))

    validate(need(nrow(ADSL_FILTERED) > 0, "Please select at least one subject"))

    validate(need(
      input$ongo_var %in% names(ADEX_FILTERED),
      paste("Study Ongoing Status must be a variable in", ex_dataname, sep = " ")
    ))

    validate(need(
      checkmate::test_logical(ADEX_FILTERED[[input$ongo_var]], min.len = 1),
      "Study Ongoing Status must be a logical variable"
    ))

    validate(need(
      all(input$anno_var %in% names(ADSL_FILTERED)),
      paste("Please only select annotation variable(s) in", sl_dataname, sep = " ")
    ))

    validate(need(
      !(input$id_var %in% input$anno_var),
      paste("Please de-select", input$id_var, "in annotation variable(s)", sep = " ")
    ))

    if (input$plot_cm) {
      ADCM_FILTERED <- datasets$get_data(cm_dataname, filtered = TRUE) # nolint
      ADCM_label <- rtables::var_labels(datasets$get_data(cm_dataname, filtered = FALSE)) # nolint
      rtables::var_labels(ADCM_FILTERED) <- ADCM_label
      validate(
        need(
          input$conmed_var %in% names(ADCM_FILTERED),
          paste("Please select a Conmed Variable in", cm_dataname, sep = " ")
        )
      )
      validate(need(
        all(input$conmed_level %in% levels(ADCM_FILTERED[[input$conmed_var]])),
        "Updating Conmed Levels"
      ))
    }

    chunks_reset(envir = environment())

    if (input$plot_cm) {
      validate(need(!is.na(input$conmed_var), "Please select a conmed variable."))
      chunks_push(bquote({
        conmed_data <- ADCM_FILTERED %>%
          filter(!!sym(.(input$conmed_var)) %in% .(input$conmed_level))
        conmed_var <- .(input$conmed_var)
        conmed_data[[conmed_var]] <-
          factor(conmed_data[[conmed_var]], levels = unique(conmed_data[[conmed_var]]))
        rtables::var_labels(conmed_data)[conmed_var] <- rtables::var_labels(ADCM_FILTERED)[conmed_var]
      }))
    } else {
      chunks_push(bquote({
        conmed_data <- conmed_var <- NULL
      }))
    }
    chunks_safe_eval()

    validate(
      need(length(input$conmed_level) <= 3, "Please select no more than 3 conmed levels")
    )

    chunks_push(bquote({
      exp_data <- ADEX_FILTERED %>%
        filter(PARCAT1 == "INDIVIDUAL")

      osprey::g_heat_bygrade(
        id_var = .(input$id_var),
        exp_data = exp_data,
        visit_var = .(input$visit_var),
        ongo_var = .(input$ongo_var),
        anno_data = ADSL_FILTERED[c(.(input$anno_var), .(input$id_var))],
        anno_var = .(input$anno_var),
        heat_data = ADAE_FILTERED %>% select(!!.(input$id_var), !!.(input$visit_var), !!.(input$heat_var)),
        heat_color_var = .(input$heat_var),
        conmed_data = conmed_data,
        conmed_var = conmed_var
      )
    }))

    chunks_safe_eval()
  })


  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = paste("R code for", label),
    datanames = datasets$datanames()
  )
}
