#' Events by Term Plot Teal Module
#'
#' @description
#'
#' Display Events by Term plot as a shiny module.
#'
#' This is an S3 generic that dispatches on the class of `term_var`:
#' - [choices_selected][teal.transform::choices_selected()] dispatches to the
#'   default method.
#' - [picks][teal.picks::picks()] dispatches to the picks method.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param term_var A variable selection object. Either a
#'   [teal.transform::choices_selected()] object (dispatches to the `.default`
#'   method) or a [teal.picks::picks()] object (dispatches to the `.picks`
#'   method).
#' @param dataname (`character(1)`) Name of the events dataset. Required when
#'   using the default method with [choices_selected][teal.transform::choices_selected()].
#'   Ignored by the `.picks` method.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @author Liming Li (lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' data <- teal_data() %>%
#'   within({
#'     ADSL <- rADSL
#'     ADAE <- rADAE
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' # Using the default method (choices_selected)
#' app <- init(
#'   data = data,
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
tm_g_events_term_id <- function(label = "Common AE",
                                dataname = NULL,
                                term_var = teal.picks::picks(
                                  teal.picks::datasets(),
                                  teal.picks::variables(
                                    choices = teal.picks::is_categorical(min.len = 2),
                                    selected = 1L
                                  )
                                ),
                                arm_var = teal.picks::picks(
                                  teal.picks::datasets(),
                                  teal.picks::variables(
                                    choices = teal.picks::is_categorical(min.len = 2),
                                    selected = 1L
                                  )
                                ),
                                fontsize = c(5, 3, 7),
                                plot_height = c(600L, 200L, 2000L),
                                plot_width = NULL,
                                transformators = list()) {
  UseMethod("tm_g_events_term_id", term_var)
}

#' @rdname tm_g_events_term_id
#' @export
tm_g_events_term_id.default <- function(label = "Common AE", # nolint: object_name_linter.
                                        dataname = NULL,
                                        term_var,
                                        arm_var,
                                        fontsize = c(5, 3, 7),
                                        plot_height = c(600L, 200L, 2000L),
                                        plot_width = NULL,
                                        transformators = list()) {
  message("Initializing tm_g_events_term_id")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
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
    transformators = transformators,
    datanames = c("ADSL", dataname)
  )
}

ui_g_events_term_id <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      teal.widgets::optionalSelectInput(
        ns("term"),
        "Term Variable",
        choices = get_choices(args$term_var$choices),
        selected = args$term_var$selected
      ),
      teal.widgets::optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = get_choices(args$arm_var$choices),
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = get_choices(args$arm_var$choices),
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = get_choices(args$arm_var$choices),
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
    )
  )
}

srv_g_events_term_id <- function(id,
                                 data,
                                 dataname,
                                 label,
                                 plot_height,
                                 plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")
    iv <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("term", shinyvalidate::sv_required(
        message = "Term Variable is required"
      ))
      iv$add_rule("arm_var", shinyvalidate::sv_required(
        message = "Arm Variable is required"
      ))
      rule_diff <- function(value, other) {
        if (isTRUE(value == other)) "Control and Treatment must be different"
      }
      iv$add_rule("arm_trt", rule_diff, other = input$arm_ref)
      iv$add_rule("arm_ref", rule_diff, other = input$arm_trt)
      iv$enable()
      iv
    })

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
      {
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
      {
        arm_var <- input$arm_var
        ANL <- data()[[dataname]]

        choices <- levels(ANL[[arm_var]])

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

    output_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )

      ANL <- obj[[dataname]]

      teal::validate_inputs(iv())

      shiny::validate(
        shiny::need(is.factor(ANL[[input$arm_var]]), "Arm Var must be a factor variable. Contact developer."),
        shiny::need(
          input$arm_trt %in% ANL[[req(input$arm_var)]] && input$arm_ref %in% ANL[[req(input$arm_var)]],
          "Cannot generate plot. The dataset does not contain subjects from both the control and treatment arms."
        )
      )

      adsl_vars <- unique(c("USUBJID", "STUDYID", input$arm_var))
      anl_vars <- c("USUBJID", "STUDYID", input$term)

      q1 <- teal.code::eval_code(
        obj,
        code = bquote(
          ANL <- merge(
            x = ADSL[, .(adsl_vars), drop = FALSE],
            y = .(as.name(dataname))[, .(anl_vars), drop = FALSE],
            all.x = FALSE,
            all.y = FALSE,
            by = c("USUBJID", "STUDYID")
          )
        )
      )

      teal::validate_has_data(q1[["ANL"]],
        min_nrow = 10,
        msg = "Analysis data set must have at least 10 data points"
      )

      teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

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
    })

    plot_r <- reactive(output_q()[["plot"]])
    set_chunk_dims(pws, output_q)
  })
}
