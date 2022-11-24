#' Events by Term Plot Teal Module
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#' library(nestcolor)
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
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
    filters = c("ADSL", dataname)
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
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    )
  )
}

srv_g_events_term_id <- function(id,
                                 data,
                                 filter_panel_api,
                                 reporter,
                                 dataname,
                                 label,
                                 plot_height,
                                 plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("arm_var", shinyvalidate::sv_required())
    iv$add_rule("term", shinyvalidate::sv_required())
    iv$enable()

    decorate_output <- srv_g_decorate(
      id = NULL, plt = plot_r, plot_height = plot_height, plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

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
      ignoreNULL = TRUE,
      handlerExpr = {
        arm_var <- input$arm_var
        ANL <- data[[dataname]]() # nolint

        choices <- levels(ANL[[arm_var]])

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
      }
    )

    output_q <- reactive({
      ANL <- data[[dataname]]() # nolint

      validate(need(iv$is_valid(), "Misspecification error: please observe red flags in the encodings."))

      validate(need(
        is.factor(ANL[[input$arm_var]]),
        "Selected arm variable needs to be a factor. Contact an app developer."
      ))

      iv_comp <- shinyvalidate::InputValidator$new()
      iv_comp$add_rule("arm_trt", shinyvalidate::sv_not_equal(
        input$arm_ref,
        message_fmt = "Must not be equal to Control"
      ))
      iv_comp$add_rule("arm_ref", shinyvalidate::sv_not_equal(
        input$arm_trt,
        message_fmt = "Must not be equal to Treatment"
      ))
      iv_comp$enable()
      validate(need(iv_comp$is_valid(), "Misspecification error: please observe red flags in the encodings."))

      validate(need(
        all(c(input$arm_trt, input$arm_ref) %in% unique(ANL[[input$arm_var]])),
        "Cannot generate plot. The dataset does not contain subjects from both the control and treatment arms."
      ))

      adsl_vars <- unique(c("USUBJID", "STUDYID", input$arm_var)) # nolint
      anl_vars <- c("USUBJID", "STUDYID", input$term) # nolint

      q1 <- teal.code::eval_code(
        teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)),
        code = bquote(
          ANL <- merge( # nolint
            x = ADSL[, .(adsl_vars), drop = FALSE],
            y = .(as.name(dataname))[, .(anl_vars), drop = FALSE],
            all.x = FALSE,
            all.y = FALSE,
            by = c("USUBJID", "STUDYID")
          )
        )
      )

      validate(need(nrow(q1[["ANL"]]) > 10, "ANL needs at least 10 data points"))

      q2 <- teal.code::eval_code(
        q1,
        code = bquote(
          plot <- osprey::g_events_term_id(
            term = ANL[[.(input$term)]],
            id = ANL$USUBJID,
            arm = ANL[[.(input$arm_var)]],
            arm_N = table(ADSL[[.(input$arm_var)]]),
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
        )
      )

      teal.code::eval_code(q2, quote(plot))
    })

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
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Events by Term")
        card$append_text("Events by Term", "header2")
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
