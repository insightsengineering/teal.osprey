#' Events by Term Plot Teal Module
#'
#' Display Events by Term plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param term_var \code{\link[teal.transform]{choices_selected}} object with all available choices
#' and pre-selected option names that can be used to specify the term for events
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @author Liming Li (lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_events_term_id(
#'       label = "Common AE",
#'       dataname = "ADAE",
#'       term_var = choices_selected(
#'         selected = "AEDECOD",
#'         choices = c(
#'           "AEDECOD", "AETERM",
#'           "AEHLT", "AELLT", "AEBODSYS"
#'         )
#'       ),
#'       arm_var = choices_selected(
#'         selected = "ACTARMCD",
#'         choices = c("ACTARM", "ACTARMCD")
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_events_term_id <- function(label,
                                dataname,
                                term_var,
                                arm_var,
                                fontsize = c(5, 3, 7),
                                plot_height = c(600L, 200L, 2000L),
                                plot_width = NULL) {
  logger::log_info("Initializing tm_g_events_term_id")
  checkmate::assert_string(label)
  checkmate::assert_class(term_var, classes = "choices_selected")
  checkmate::assert_class(arm_var, classes = "choices_selected")
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

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_events_term_id,
    server_args = list(label = label, dataname = dataname, plot_height = plot_height, plot_width = plot_width),
    ui = ui_g_events_term_id,
    ui_args = args,
    filters = dataname
  )
}

ui_g_events_term_id <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = div(
      ### Reporter
      shiny::tags$div(
        teal.reporter::add_card_button_ui(ns("addReportCard")),
        teal.reporter::download_report_button_ui(ns("downloadButton")),
        teal.reporter::reset_report_button_ui(ns("resetButton"))
      ),
      shiny::tags$br(),
      ###
      teal.widgets::optionalSelectInput(
        ns("term"),
        "Term Variable",
        choices = args$term_var$choices,
        selected = args$term_var$selected
      ),
      teal.widgets::optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      teal.widgets::optionalSelectInput(
        ns("sort"),
        "Sort By",
        choices = c(
          "Term" = "term",
          "Risk Difference" = "riskdiff",
          "Mean Risk" = "meanrisk"
        ),
        selected = NULL
      ),
      teal.widgets::panel_item(
        "Confidence interval settings",
        teal.widgets::optionalSelectInput(
          ns("diff_ci_method"),
          "Method for Difference of Proportions CI",
          choices = ci_choices,
          selected = ci_choices[1]
        ),
        teal.widgets::optionalSliderInput(
          ns("conf_level"),
          "Confidence Level",
          min = 0.5,
          max = 1,
          value = 0.95
        )
      ),
      teal.widgets::panel_item(
        "Additional plot settings",
        teal.widgets::optionalSelectInput(
          ns("axis"),
          "Axis Side",
          choices = c("Left" = "left", "Right" = "right"),
          selected = "left"
        ),
        sliderInput(
          ns("raterange"),
          "Overall Rate Range",
          min = 0,
          max = 1,
          value = c(0.1, 1),
          step = 0.01
        ),
        sliderInput(
          ns("diffrange"),
          "Rate Difference Range",
          min = -1,
          max = 1,
          value = c(-0.5, 0.5),
          step = 0.01
        ),
        checkboxInput(ns("reverse"),
          "Reverse Order",
          value = FALSE
        )
      ),
      ui_g_decorate(
        ns(NULL),
        fontsize = args$fontsize,
        titles = "Common AE Table",
        footnotes = ""
      )
    ),
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_g_events_term_id <- function(id,
                                 datasets,
                                 reporter,
                                 dataname,
                                 label,
                                 plot_height,
                                 plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  moduleServer(id, function(input, output, session) {
    font_size <- srv_g_decorate(id = NULL, plt = plt, plot_height = plot_height, plot_width = plot_width) # nolint

    teal.code::init_chunks()

    observeEvent(list(input$diff_ci_method, input$conf_level), {
      req(!is.null(input$diff_ci_method) && !is.null(input$conf_level))
      diff_ci_method <- input$diff_ci_method
      conf_level <- input$conf_level
      updateTextAreaInput(
        session,
        "foot",
        value = sprintf(
          "Note: %d%% CI is calculated using %s",
          round(conf_level * 100),
          name_ci(diff_ci_method)
        )
      )
    })


    observeEvent(input$sort,
      handlerExpr = {
        sort <- if (is.null(input$sort)) " " else input$sort
        updateTextInput(
          session,
          "title",
          value = sprintf(
            "Common AE Table %s",
            c(
              "term" = "Sorted by Term",
              "riskdiff" = "Sorted by Risk Difference",
              "meanrisk" = "Sorted by Mean Risk",
              " " = ""
            )[sort]
          )
        )
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$arm_var,
      handlerExpr = {
        arm_var <- input$arm_var
        ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

        choices <- levels(ANL_FILTERED[[arm_var]])

        validate(need(length(choices) > 0, "Please include multiple treatment"))
        if (length(choices) == 1) {
          trt_index <- 1
        } else {
          trt_index <- 2
        }

        updateSelectInput(
          session,
          "arm_ref",
          selected = choices[1],
          choices = choices
        )
        updateSelectInput(
          session,
          "arm_trt",
          selected = choices[trt_index],
          choices = choices
        )
      },
      ignoreNULL = TRUE
    )

    plt <- reactive({
      validate(
        need(input$term, "'Term Variable' field is missing"),
        need(input$arm_var, "'Arm Variable' field is missing")
      )

      validate(need(
        input$arm_trt != input$arm_ref,
        paste("Treatment arm and control arm cannot be the same.",
          "Please select a different treatment arm or control arm",
          sep = "\n"
        )
      ))

      ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
      ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
      formatters::var_labels(ANL_FILTERED) <- formatters::var_labels(
        datasets$get_data(dataname, filtered = FALSE),
        fill = FALSE
      )

      anl_name <- paste0(dataname, "_FILTERED")
      assign(anl_name, ANL_FILTERED)

      validate(need(
        all(c(input$arm_trt, input$arm_ref) %in% unique(ANL_FILTERED[[input$arm_var]])),
        "Cannot generate plot. The dataset does not contain subjects from both the control and treatment arms."
      ))

      teal.code::chunks_reset(envir = environment())

      adsl_vars <- unique(c("USUBJID", "STUDYID", input$arm_var)) # nolint
      anl_vars <- c("USUBJID", "STUDYID", input$term) # nolint

      teal.code::chunks_push(bquote({
        ANL <- merge( # nolint
          x = ADSL_FILTERED[, .(adsl_vars), drop = FALSE],
          y = .(as.name(anl_name))[, .(anl_vars), drop = FALSE],
          all.x = FALSE,
          all.y = FALSE,
          by = c("USUBJID", "STUDYID")
        )
      }))

      teal.code::chunks_safe_eval()
      validate(need(nrow(teal.code::chunks_get_var("ANL")) > 10, "need at least 10 data points"))

      teal.code::chunks_push(bquote({
        term <- ANL[[.(input$term)]]
        id <- ANL$USUBJID
        arm <- ANL[[.(input$arm_var)]]
        arm_N <- table(ADSL_FILTERED[[.(input$arm_var)]]) # nolint
        ref <- .(input$arm_ref)
        trt <- .(input$arm_trt)

        osprey::g_events_term_id(
          term = term,
          id = id,
          arm = arm,
          arm_N = arm_N,
          ref = .(input$arm_ref),
          trt = .(input$arm_trt),
          sort_by = .(input$sort),
          rate_range = .(input$raterange),
          diff_range = .(input$diffrange),
          reversed = .(input$reverse),
          conf_level = .(input$conf_level),
          diff_ci_method = .(input$diff_ci_method),
          axis_side = .(input$axis),
          fontsize = .(font_size()),
          draw = TRUE
        )
      }))

      teal.code::chunks_safe_eval()
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = unique(c(
        dataname,
        vapply(X = dataname, FUN.VALUE = character(1), function(x) {
          if (inherits(datasets, "CDISCFilteredData")) datasets$get_parentname(x)
        })
      ))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Events by Term")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Events by Term Plot", "header3")
        card$append_plot(plt())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card
      }

      teal.reporter::add_card_button_srv("addReportCard", reporter = reporter, card_fun = card_fun)
      teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
      teal.reporter::reset_report_button_srv("resetButton", reporter)
    }
  })
}
