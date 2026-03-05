#' @rdname tm_g_events_term_id
#'
#' @examples
#' # Using the picks method
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
#' @export
tm_g_events_term_id.picks <- function(label = "Common AE", # nolint: object_name_linter.
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
  message("Initializing tm_g_events_term_id.picks")

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_class(term_var, "picks")
  if (isTRUE(attr(term_var$variables, "multiple"))) {
    warning(
      "`term_var` accepts only a single variable selection. ",
      "Forcing `teal.picks::variables(multiple)` to FALSE."
    )
    attr(term_var$variables, "multiple") <- FALSE
  }

  checkmate::assert_class(arm_var, "picks")
  if (isTRUE(attr(arm_var$variables, "multiple"))) {
    warning(
      "`arm_var` accepts only a single variable selection. ",
      "Forcing `teal.picks::variables(multiple)` to FALSE."
    )
    attr(arm_var$variables, "multiple") <- FALSE
  }

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
  # End of assertions

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_events_term_id.picks,
    server = srv_g_events_term_id.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_events_term_id.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_events_term_id.picks))],
    transformators = transformators,
    datanames = .picks_datanames(list(term_var, arm_var))
  )
}

# UI function for the events_term_id.picks module
ui_g_events_term_id.picks <- function(id, # nolint: object_name_linter.
                                      term_var,
                                      arm_var,
                                      fontsize) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      tags$div(
        tags$strong("Term variable"),
        teal.picks::picks_ui(id = ns("term_var"), picks = term_var)
      ),
      tags$div(
        tags$strong("Arm variable"),
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

# Server function for the events_term_id.picks module
srv_g_events_term_id.picks <- function(id, # nolint: object_name_linter.
                                       data,
                                       term_var,
                                       arm_var,
                                       plot_height,
                                       plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = list(
        term_var = term_var,
        arm_var = arm_var
      ),
      data = data
    )

    # Merge datasets based on picks selections
    merged <- teal.picks::merge_srv(
      "merge",
      data = data,
      selectors = selectors,
      output_name = "ANL"
    )

    # Update arm_ref/arm_trt based on arm_var selection
    observeEvent(selectors$arm_var(), {
      arm_var_name <- selectors$arm_var()$variables$selected
      arm_dataset <- selectors$arm_var()$datasets$selected
      req(arm_var_name, arm_dataset)

      arm_data <- data()[[arm_dataset]]
      choices <- levels(arm_data[[arm_var_name]])

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
    }, ignoreNULL = TRUE)

    # Update title based on sort selection
    observeEvent(input$sort, {
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
    }, ignoreNULL = FALSE)

    # Update footnotes based on CI settings
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

    # Main output reactive
    output_q <- reactive({
      qenv <- merged$data()

      # Variable names in the merged ANL dataset (potentially renamed to avoid conflicts)
      term_var_name <- merged$variables()$term_var
      arm_var_name <- merged$variables()$arm_var

      # Original variable name and dataset for arm_N calculation on the source dataset
      arm_var_orig <- selectors$arm_var()$variables$selected
      arm_dataset <- selectors$arm_var()$datasets$selected

      shiny::validate(
        shiny::need(
          length(term_var_name) > 0,
          "A Term Variable needs to be selected."
        ),
        shiny::need(
          length(arm_var_name) > 0,
          "An Arm Variable needs to be selected."
        )
      )

      ANL <- qenv[["ANL"]]

      shiny::validate(
        shiny::need(
          is.factor(ANL[[arm_var_name]]),
          "Arm Variable must be a factor variable."
        ),
        shiny::need(
          input$arm_trt %in% ANL[[arm_var_name]] && input$arm_ref %in% ANL[[arm_var_name]],
          "Cannot generate plot. The dataset does not contain subjects from both the control and treatment arms."
        )
      )

      shiny::validate(
        shiny::need(
          !isTRUE(input$arm_trt == input$arm_ref),
          "Control and Treatment must be different."
        )
      )

      teal::validate_has_data(
        ANL,
        min_nrow = 10,
        msg = "Analysis data set must have at least 10 data points"
      )

      teal.reporter::teal_card(qenv) <-
        c(
          teal.reporter::teal_card(qenv),
          teal.reporter::teal_card("## Module's output(s)")
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

#' Extract datanames from a list of picks objects
#'
#' @param x (`list`) list of picks objects (or NULLs)
#' @return `character` vector of unique datanames, or `"all"` if any picks
#'   object uses dynamic dataset choices.
#' @keywords internal
.picks_datanames <- function(x) {
  checkmate::assert_list(x, c("picks", "NULL"))
  datanames_list <- lapply(x, function(x) {
    if (is.character(x$datasets$choices)) {
      x$datasets$choices
    } else {
      NULL
    }
  })

  if (any(vapply(datanames_list, is.null, logical(1)))) {
    "all"
  } else {
    unique(unlist(datanames_list))
  }
}
