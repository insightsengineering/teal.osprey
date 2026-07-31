#' Teal module for the `AE` overview
#'
#' @description
#'
#' Display the `AE` overview plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param flag_var_anl Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected()`]) object.
#'   `choices_selected()` is being deprecated as an argument type and will be removed in the future.
#'   Object with variables used to count adverse event
#'   sub-groups (e.g. Serious events, Related events, etc.)
#' @inherit argument_convention return
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`grob`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_ae_oview(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...), # applied to the `plot` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.general")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examples
#' data <- within(teal_data(), {
#'   library(dplyr)
#'   ADSL <- rADSL
#'   ADAE <- rADAE
#'   .add_event_flags <- function(dat) {
#'     dat <- dat %>%
#'       mutate(
#'         TMPFL_SER = AESER == "Y",
#'         TMPFL_REL = AEREL == "Y",
#'         TMPFL_GR5 = AETOXGR == "5",
#'         AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
#'         AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
#'       )
#'     labels <- c(
#'       "Serious AE", "Related AE", "Grade 5 AE",
#'       "AE related to A: Drug X", "AE related to B: Placebo"
#'     )
#'     cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'     for (i in seq_along(labels)) {
#'       attr(dat[[cols[i]]], "label") <- labels[i]
#'     }
#'     dat
#'   }
#'   ADAE <- .add_event_flags(ADAE)
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' app <- suppressWarnings(init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ae_oview(
#'       label = "AE Overview",
#'       dataname = "ADAE",
#'       arm_var = variables(
#'         choices = dplyr::starts_with("ACTARM"),
#'         selected = "ACTARMCD"
#'       ),
#'       flag_var_anl = variables(
#'         choices = c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2"),
#'         selected = "AEREL1"
#'       )
#'     )
#'   )
#' ), classes = "picks_delayed")
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
tm_g_ae_oview <- function(
  label,
  dataname,
  arm_var,
  flag_var_anl,
  fontsize = c(5, 3, 7),
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_g_ae_oview")

  arm_var <- migrate_choices_selected_to_variables(arm_var)
  flag_var_anl <- migrate_choices_selected_to_variables(flag_var_anl)

  arm_var <- create_picks_helper(teal.picks::datasets(dataname), arm_var)
  flag_var_anl <- create_picks_helper(teal.picks::datasets(dataname), flag_var_anl)

  if (teal.picks::is_pick_multiple(arm_var$variables)) {
    warning(
      "`arm_var` accepts only a single variable selection. ",
      "Forcing `teal.picks::variables(multiple)` to FALSE."
    )
    attr(arm_var$variables, "multiple") <- FALSE
  }

  if (teal.picks::is_pick_multiple(flag_var_anl$variables)) {
    warning(
      "`flag_var_anl` accepts only a single variable selection. ",
      "Forcing `teal.picks::variables(multiple)` to FALSE."
    )
    attr(flag_var_anl$variables, "multiple") <- FALSE
  }

  checkmate::assert(
    checkmate::check_number(fontsize, finite = TRUE),
    checkmate::assert(
      combine = "and",
      .var.name = "fontsize",
      checkmate::check_numeric(
        fontsize,
        len = 3,
        any.missing = FALSE,
        finite = TRUE
      ),
      checkmate::check_numeric(
        fontsize[1],
        lower = fontsize[2],
        upper = fontsize[3]
      )
    )
  )
  checkmate::assert_numeric(
    plot_height,
    len = 3,
    any.missing = FALSE,
    finite = TRUE
  )
  checkmate::assert_numeric(
    plot_height[1],
    lower = plot_height[2],
    upper = plot_height[3],
    .var.name = "plot_height"
  )
  checkmate::assert_numeric(
    plot_width,
    len = 3,
    any.missing = FALSE,
    null.ok = TRUE,
    finite = TRUE
  )
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )
  assert_transformators(transformators)
  teal::assert_decorators(decorators, "plot")

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ae_oview,
    server_args = args[names(args) %in% names(formals(srv_g_ae_oview))],
    ui = ui_g_ae_oview,
    ui_args = args[names(args) %in% names(formals(ui_g_ae_oview))],
    transformators = transformators,
    datanames = c("ADSL", dataname)
  )
}

ui_g_ae_oview <- function(
  id,
  arm_var,
  flag_var_anl,
  fontsize,
  decorators
) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      tags$div(
        tags$strong("Arm variable"),
        teal.picks::picks_ui(id = ns("arm_var"), picks = arm_var)
      ),
      tags$div(
        tags$strong("Flag variables"),
        teal.picks::picks_ui(id = ns("flag_var_anl"), picks = flag_var_anl)
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
      teal.widgets::panel_item(
        "Confidence interval settings",
        teal.widgets::optionalSelectInput(
          ns("diff_ci_method"),
          "Method for Difference of Proportions CI",
          choices = ci_choices,
          selected = ci_choices[1],
          multiple = FALSE
        ),
        teal.widgets::optionalSliderInput(
          ns("conf_level"),
          "Confidence Level",
          min = 0.5,
          max = 1,
          value = 0.95
        )
      ),
      teal.widgets::optionalSelectInput(
        ns("axis"),
        "Axis Side",
        choices = c("Left" = "left", "Right" = "right"),
        selected = "left",
        multiple = FALSE
      ),
      teal::ui_transform_teal_data(
        ns("decorator"),
        transformators = select_decorators(decorators, "plot")
      ),
      ui_g_decorate(
        ns(NULL),
        fontsize = fontsize,
        titles = "AE Overview",
        footnotes = ""
      )
    )
  )
}

srv_g_ae_oview <- function(
  id,
  data,
  arm_var,
  flag_var_anl,
  plot_height,
  plot_width,
  decorators
) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = list(
        arm_var = arm_var,
        flag_var_anl = flag_var_anl
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

    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
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

    observeEvent(merged$variables()$arm_var, {
      arm_var_name <- merged$variables()$arm_var
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
    })

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        qenv <- merged$data()

        arm_var_name <- selectors$arm_var()$variables$selected
        arm_dataset <- selectors$arm_var()$datasets$selected

        teal.reporter::teal_card(qenv) <-
          c(
            teal.reporter::teal_card(qenv),
            teal.reporter::teal_card("## Module's output(s)")
          )
        qenv <- teal.code::eval_code(qenv, "library(dplyr)")

        ANL <- qenv[["ANL"]]

        arm_var_name <- merged$variables()$arm_var
        flag_var_name <- merged$variables()$flag_var_anl

        teal::validate_has_data(
          ANL,
          min_nrow = 10,
          msg = "Analysis data set must have at least 10 data points"
        )

        # Original variable name and dataset for arm_N calculation on the source dataset
        arm_var_orig <- selectors$arm_var()$variables$selected
        arm_dataset <- selectors$arm_var()$datasets$selected
        validate_input(
          "flag_var_anl",
          length(flag_var_name) > 0,
          "A Flag Variable needs to be selected."
        )

        validate_input(
          "arm_var",
          length(arm_var_name) > 0,
          "An Arm Variable needs to be selected."
        )

        validate_input(
          c("arm_trt", "arm_ref"),
          input$arm_trt %in%
            ANL[[arm_var_name]] &&
            input$arm_ref %in% ANL[[arm_var_name]],
          "Treatment or Control not found in Arm Variable. Perhaps they have been filtered out?"
        )

        validate_input(
          c("arm_trt", "arm_ref"),
          input$arm_trt != input$arm_ref,
          "Treatment and Control can't be the same."
        )

        q1 <- qenv %>%
          teal.code::eval_code(
            code = as.expression(c(
              bquote(anl_labels <- formatters::var_labels(ANL, fill = FALSE)),
              bquote(
                flags <- ANL %>%
                  select(all_of(.(flag_var_name))) %>%
                  rename_at(.(flag_var_name), function(x) {
                    paste0(x, ": ", anl_labels[x])
                  })
              )
            ))
          )

        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          "### Plot"
        )
        teal.code::eval_code(
          q1,
          code = as.expression(c(
            bquote(
              plot <- osprey::g_events_term_id(
                term = flags,
                id = ANL$USUBJID,
                arm = ANL[[.(arm_var_name)]],
                arm_N = table(ANL[[.(arm_var_name)]]),
                ref = .(input$arm_ref),
                trt = .(input$arm_trt),
                diff_ci_method = .(input$diff_ci_method),
                conf_level = .(input$conf_level),
                axis_side = .(input$axis),
                fontsize = .(font_size()),
                draw = TRUE
              )
            )
          ))
        )
      })
    )

    decorated_output_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = output_q,
      transformators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )
    plot_r <- reactive(decorated_output_q()[["plot"]])

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    )
    pws <- decorate_output$pws
    set_chunk_dims(pws, decorated_output_q)
  })
}
