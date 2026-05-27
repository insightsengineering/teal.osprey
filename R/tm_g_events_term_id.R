#' Events by Term Plot Teal Module
#'
#' Display an events-by-term plot as a Shiny module using [teal.picks::picks()] encodings.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param term_var (`picks`)\cr
#'   [teal.picks::picks()] object for the event term variable (single selection).
#' @param arm_var (`picks`)\cr
#'   [teal.picks::picks()] object for the treatment arm variable (single selection).
#'   The arm variable must be a factor in the analysis data.
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
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_events_term_id(
#'       label = "Common AE",
#'       term_var = teal.picks::picks(
#'         teal.picks::datasets("ADAE"),
#'         teal.picks::variables(
#'           choices = teal.picks::is_categorical(min.len = 2),
#'           selected = "AEDECOD"
#'         )
#'       ),
#'       arm_var = teal.picks::picks(
#'         teal.picks::datasets("ADSL"),
#'         teal.picks::variables(
#'           choices = teal.picks::is_categorical(min.len = 2),
#'           selected = "ACTARMCD"
#'         )
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
  checkmate::assert_string(label)

  checkmate::assert_class(term_var, "picks", .var.name = "term_var")
  checkmate::assert_false(
    teal.picks::is_pick_multiple(term_var$variables),
    .var.name = "`term_var` must use variables(..., multiple = FALSE)"
  )
  checkmate::assert_class(arm_var, "picks", .var.name = "arm_var")
  checkmate::assert_false(
    teal.picks::is_pick_multiple(arm_var$variables),
    .var.name = "`arm_var` must use variables(..., multiple = FALSE)"
  )

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
  checkmate::assert_numeric(
    plot_height[1],
    lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height"
  )
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  pick_slots <- list(term_var = term_var, arm_var = arm_var)
  all_datanames <- .picks_all_datanames(pick_slots)

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_events_term_id,
    server = srv_g_events_term_id,
    ui_args = args[names(args) %in% names(formals(ui_g_events_term_id))],
    server_args = args[names(args) %in% names(formals(srv_g_events_term_id))],
    transformators = transformators,
    datanames = all_datanames
  )
}

#' @keywords internal
ui_g_events_term_id <- function(id,
                                term_var,
                                arm_var,
                                fontsize) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      tags$div(
        tags$label("Term variable"),
        teal.picks::picks_ui(id = ns("term_var"), picks = term_var)
      ),
      tags$div(
        tags$label("Arm variable"),
        teal.picks::picks_ui(id = ns("arm_var"), picks = arm_var)
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = NULL
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = NULL
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
        checkboxInput(
          ns("reverse"),
          "Reverse Order",
          value = FALSE
        )
      ),
      ui_g_decorate(
        ns(NULL),
        fontsize = fontsize,
        titles = "Common AE Table",
        footnotes = ""
      )
    )
  )
}

#' @keywords internal
srv_g_events_term_id <- function(id,
                                 data,
                                 term_var,
                                 arm_var,
                                 plot_height,
                                 plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    anl_selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(term_var = term_var, arm_var = arm_var),
      data = data
    )

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj
    })

    merged_anl <- teal.picks::merge_srv(
      "merge_anl",
      data = data_with_card,
      selectors = anl_selectors,
      output_name = "ANL",
      join_fun = "dplyr::inner_join"
    )

    anl_q <- merged_anl$data
    merge_vars <- merged_anl$variables

    observeEvent(anl_selectors$arm_var(),
      {
        arm_selector <- anl_selectors$arm_var()
        req(arm_selector)
        arm_var_name <- arm_selector$variables$selected
        arm_dataset <- arm_selector$datasets$selected
        req(arm_var_name, arm_dataset)

        arm_data <- data()[[arm_dataset]]
        choices <- levels(arm_data[[arm_var_name]])

        trt_index <- if (length(choices) == 1L) 1L else 2L

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

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    output_q <- reactive({
      merged_vars <- merge_vars()
      validate(
        need(
          length(merged_vars[["term_var"]]) > 0L,
          "Please select a term variable"
        ),
        need(
          length(merged_vars[["arm_var"]]) > 0L,
          "Please select an arm variable"
        )
      )

      term_var_name <- merged_vars[["term_var"]][[1L]]
      arm_var_name <- merged_vars[["arm_var"]][[1L]]

      arm_selector <- anl_selectors$arm_var()
      arm_var_orig <- arm_selector$variables$selected
      arm_dataset <- arm_selector$datasets$selected

      qenv <- anl_q()
      ANL <- qenv[["ANL"]]

      validate(
        need(
          is.factor(ANL[[arm_var_name]]),
          "Arm Variable must be a factor variable."
        ),
        need(
          input$arm_trt %in% ANL[[arm_var_name]] && input$arm_ref %in% ANL[[arm_var_name]],
          "Cannot generate plot. The dataset does not contain subjects from both the control and treatment arms."
        ),
        need(
          !isTRUE(input$arm_trt == input$arm_ref),
          "Control and Treatment must be different."
        )
      )

      teal::validate_has_data(
        ANL,
        min_nrow = 10,
        msg = "Analysis data set must have at least 10 data points"
      )

      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Plot")

      teal.code::eval_code(
        qenv,
        code = bquote(
          plot <- osprey::g_events_term_id(
            term = ANL[[.(term_var_name)]],
            id = ANL$USUBJID,
            arm = ANL[[.(arm_var_name)]],
            arm_N = table(.(as.name(arm_dataset))[[.(arm_var_orig)]]),
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
