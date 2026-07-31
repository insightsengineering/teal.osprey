#' teal module for the `AE` by subgroups
#'
#' @description
#'
#' Display the `AE` by subgroups plot as a teal module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param group_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected()`]) object.
#'   `choices_selected()` is being deprecated as an argument type and will be removed in the future.
#'   Object with subgroup variables.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
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
#' tm_g_ae_sub(
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
#' @export
#'
#' @examples
#' # Example using stream (ADaM) dataset
#' data <- within(teal_data(), {
#'   ADSL <- rADSL
#'   ADAE <- rADAE
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ae_sub(
#'       label = "AE by Subgroup",
#'       dataname = "ADAE",
#'       arm_var = variables(
#'         choices = c("ACTARM", "ACTARMCD"),
#'         selected = "ACTARMCD"
#'       ),
#'       group_var = variables(
#'         choices = c("SEX", "REGION1", "RACE"),
#'         selected = c("SEX", "REGION1", "RACE"),
#'         multiple = TRUE
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_ae_sub <- function(label,
                        dataname,
                        arm_var,
                        group_var,
                        plot_height = c(600L, 200L, 2000L),
                        plot_width = NULL,
                        fontsize = c(5, 3, 7),
                        transformators = list(),
                        decorators = list()) {
  message("Initializing tm_g_ae_sub")

  arm_var <- migrate_choices_selected_to_variables(arm_var)
  group_var <- migrate_choices_selected_to_variables(group_var)
  checkmate::assert_string(dataname)
  arm_var <- create_picks_helper(teal.picks::datasets(dataname), arm_var)
  group_var <- create_picks_helper(teal.picks::datasets(dataname), group_var)

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
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  assert_transformators(transformators)
  teal::assert_decorators(decorators, "plot")
  checkmate::assert_class(arm_var, "picks")
  checkmate::assert_class(group_var, "picks")

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ae_sub,
    server_args = args[names(args) %in% names(formals(srv_g_ae_sub))],
    ui = ui_g_ae_sub,
    ui_args = args[names(args) %in% names(formals(ui_g_ae_sub))],
    transformators = transformators,
    datanames = c("ADSL", dataname)
  )
}

ui_g_ae_sub <- function(id, arm_var, group_var, fontsize, arm_n = FALSE, decorators = NULL) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code("ADAE")),
      tags$div(
        tags$strong("Arm variable"),
        teal.picks::picks_ui(id = ns("arm_var"), picks = arm_var)
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = NULL
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = NULL
      ),
      checkboxInput(
        ns("arm_n"),
        "Show N in each arm",
        value = arm_n
      ),
      tags$div(
        tags$strong("Group variable"),
        teal.picks::picks_ui(id = ns("groups"), picks = group_var)
      ),
      teal.widgets::panel_item(
        "Change group labels",
        uiOutput(ns("grouplabel_output"))
      ),
      teal.widgets::panel_item(
        "Additional plot settings",
        teal.widgets::optionalSelectInput(
          ns("ci"),
          "CI method",
          choices = ci_choices,
          selected = ci_choices[1]
        ),
        teal.widgets::optionalSliderInput(
          ns("conf_level"),
          "Significant Level",
          min = 0.5,
          max = 1,
          value = 0.95
        ),
        teal::ui_transform_teal_data(
          ns("decorator"),
          transformators = select_decorators(args$decorators, "plot")
        ),
        ui_g_decorate(
          ns(NULL),
          fontsize = fontsize,
          titles = "AE Table with Subgroups",
          footnotes = ""
        )
      )
    )
  )
}

srv_g_ae_sub <- function(id,
                         data,
                         dataname,
                         label,
                         arm_var,
                         group_var,
                         plot_height,
                         plot_width,
                         fontsize,
                         decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Build picks list (exclude NULL optional picks)
    picks_list <- list(
      arm_var = arm_var,
      group_var = group_var
    )

    font_size <- reactive(input$fontsize)
    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal::validate_input(
        inputId   = "arm_var",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message   = "Please select an arm variable."
      )
      teal::validate_input(
        inputId   = "group_var",
        condition = !is.null(selectors$group_var()$variables$selected),
        message   = "Please select an group variable."
      )
      obj
    })

    # Merge datasets based on picks selections
    merged <- teal.picks::merge_srv(
      "merge",
      data = validated_q,
      selectors = selectors,
      output_name = "ANL"
    )

    # Dynamic options for Treatment vs Reference
    observeEvent(merged$variables(), ignoreNULL = TRUE, {
      ANL <- merged$data()[[dataname]]
      anl_val <- ANL[[merged$variables()$arm_var]]
      choices <- levels(anl_val)

      if (length(choices) == 1) {
        ref_index <- 1
      } else {
        ref_index <- 2
      }

      updateSelectInput(
        session,
        "arm_trt",
        selected = choices[1],
        choices = choices
      )
      updateSelectInput(
        session,
        "arm_ref",
        selected = choices[ref_index],
        choices = choices
      )
    })

    observeEvent(list(input$ci, input$conf_level, input$arm_trt, input$arm_ref), {
      diff_ci_method <- input$ci
      conf_level <- input$conf_level
      trt <- input$arm_trt
      ref <- input$arm_ref

      teal::validate_input(
        c("arm_trt", "arm_ref"),
        trt != ref,
        "Treatment and reference should be different."
      )
      teal::validate_input(
        "ci",
        !is.null(diff_ci_method),
        "There should be a CI method set."
      )
      updateTextAreaInput(
        session,
        "foot",
        value = sprintf(
          "Note: %d%% CI is calculated using %s\nTRT: %s; CONT: %s",
          round(conf_level * 100),
          name_ci(diff_ci_method),
          trt,
          ref
        )
      )
    })

    plot_r <- reactive(output_q()[["plot"]])
    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        qenv <- merged$data()
        teal.reporter::teal_card(qenv) <-
          c(
            teal.reporter::teal_card(qenv),
            teal.reporter::teal_card("## Module's output(s)")
          )
        qenv <- teal.code::eval_code(qenv, "library(dplyr)")

        ANL <- qenv[["ANL"]]
        ADSL <- qenv[["ADSL"]]
        arm_var_name <- selectors$arm_var()$variables$selected
        group_var_name <- selectors$group_var()$variables$selected

        teal::validate_has_data(ANL, min_nrow = 10, msg = sprintf("%s has not enough data", dataname))

        validate_input("group_var", length(group_var_name) > 0L, "Group variable is required.")
        validate_input("arm_var", length(arm_var_name) == 1L, "Arm Variable is required.")
        validate_input(
          "arm_var",
          is.factor(ANL[[arm_var_name]]),
          "Arm Variable must be a factor variable, contact app developer."
        )
        validate_input(
          c("arm_trt", "arm_ref"),
          input$arm_trt != input$arm_ref,
          "Treatment and reference should be different."
        )
        sapply(group_var_name, function(x) {
          teal::validate_input(
            inputId = "group_var",
            condition = is.factor(ANL[[x]]),
            message = sprintf("Group variable '%s' must be a factor variable.", x)
          )
        })
        validate_input(
          c("arm_trt", "arm_ref"),
          input$arm_trt %in% ANL[[arm_var_name]] && input$arm_ref %in% ANL[[arm_var_name]],
          "Treatment or Control not found in Arm Variable. Perhaps they have been filtered out?"
        )

        teal::validate_input(
          "arm_var",
          length(ANL[[arm_var_name]]) == length(ANL$USUBJID),
          "length of id and arm are identical"
        )

        q1 <- within(
          qenv,
          {
            var_names <- group_var_name
            subgroups_levels <- lapply(var_names, function(x) {
              lvl <- levels(ANL[[x]])

              l <- append(as.list(lvl), x, 0L)
              names(l) <- c("Total", lvl)
              l
            })
            names(subgroups_levels) <- var_names
          },
          group_var_name = group_var_name
        )


        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

        arm_n <- NULL # to avoid R CMD NOTE on no visible binding

        q2 <- within(
          q1,
          {
            plot <- osprey::g_ae_sub(
              id = ANL$USUBJID,
              arm = ANL[[arm_var_name]],
              arm_sl = as.character(ANL[[arm_var_name]]),
              trt = trt,
              ref = ref,
              subgroups = ANL[group_var_name],
              subgroups_sl = ANL[group_var_name],
              subgroups_levels = subgroups_levels,
              conf_level = conf_level,
              diff_ci_method = diff_ci_method,
              fontsize = fontsize,
              arm_n = arm_n,
              draw = TRUE
            )
          },
          dataname = as.name("ANL$USUBJID"),
          trt = input$arm_trt,
          ref = input$arm_ref,
          conf_level = input$conf_level,
          diff_ci_method = input$ci,
          group_var_name = group_var_name,
          arm_var_name = arm_var_name,
          arm_n = input$arm_n,
          fontsize = font_size()
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
