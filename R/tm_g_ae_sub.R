#' teal module for the `AE` by subgroups
#'
#' @description
#'
#' Display the `AE` by subgroups plot as a teal module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param arm_var ([`teal.picks::variables()`]) object with variable of the arm.
#' @param group_var ([`teal.picks::variables()`]) object with  subgroups variables.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @examples
#' # Example using stream (ADaM) dataset
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
#'       ),
#'       plot_height = c(600, 200, 2000)
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
                        transformators = list()) {
  message("Initializing tm_g_ae_sub")

  arm_var <- migrate_choices_selected_to_variables(arm_var)
  group_var <- migrate_choices_selected_to_variables(group_var)
  checkmate::assert_string(dataname)
  arm_var <- suppressWarnings(create_picks_helper(teal.picks::datasets(dataname), arm_var), classes = "picks")
  group_var <- suppressWarnings(create_picks_helper(teal.picks::datasets(dataname), group_var), classes = "picks")

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

ui_g_ae_sub <- function(id, arm_var, group_var, fontsize, arm_n = FALSE) {
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
                         plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Build picks list (exclude NULL optional picks)
    picks_list <- list(
      arm_var = arm_var,
      group_var = group_var
    )

    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal::validate_input(
        inputId   = "arm_var-variables-selected",
        condition = !is.null(selectors$arm_var()$variables$selected),
        message   = "Please select an arm variable."
      )
      teal::validate_input(
        inputId   = "group_var-variables-selected",
        condition = !is.null(selectors$group_var()$variables$selected),
        message   = "Please select an group variable."
      )
      # teal::validate_input(
      #   inputId = "group_var-variables-selected",
      #   condition = length(selectors$group_var()$variables$selected) == 1L,
      #   message = "Group variable must be of length 1.")
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

    observeEvent(input$groups, {
      ANL <- data()[[dataname]]
      output$grouplabel_output <- renderUI({
        grps <- input$groups
        lo <- lapply(seq_along(grps), function(index) {
          grp <- grps[index]
          choices <- levels(ANL[[grp]])
          sel <- teal.widgets::optionalSelectInput(
            session$ns(sprintf("groups__%s", index)),
            grp,
            choices,
            multiple = TRUE,
            selected = choices
          )
          textname <- sprintf("text_%s_out", index)
          txt <- uiOutput(session$ns(textname))
          observeEvent(
            eventExpr = input[[sprintf("groups__%s", index)]],
            handlerExpr = {
              output[[textname]] <- renderUI({
                if (!is.null(input[[sprintf("groups__%s", index)]])) {
                  l <- input[[sprintf("groups__%s", index)]]
                  l2 <- lapply(seq_along(l), function(i) {
                    nm <- sprintf("groups__%s__level__%s", index, i)
                    label <- sprintf("Label for %s, Level %s", grp, l[i])
                    textInput(session$ns(nm), label, l[i])
                  })
                  tagList(textInput(
                    session$ns(
                      sprintf("groups__%s__level__%s", index, "all")
                    ),
                    sprintf("Label for %s", grp), grp
                  ), l2)
                }
              })
            }
          )
          tagList(sel, txt)
        })
        ret <- tagList(lo)
        ret
      })
    })

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        obj <- data()
        teal.reporter::teal_card(obj) <-
          c(
            teal.reporter::teal_card(obj),
            teal.reporter::teal_card("## Module's output(s)")
          )

        ANL <- obj[[dataname]]
        ADSL <- obj[["ADSL"]]

        teal::validate_has_data(ANL, min_nrow = 10, msg = sprintf("%s has not enough data", dataname))

        teal::validate_inputs(iv())

        validate(need(
          input$arm_trt %in% ANL[[input$arm_var]] && input$arm_ref %in% ANL[[input$arm_var]],
          "Treatment or Control not found in Arm Variable. Perhaps they have been filtered out?"
        ))

        group_labels <- lapply(seq_along(input$groups), function(x) {
          items <- input[[sprintf("groups__%s", x)]]
          if (length(items) > 0) {
            l <- lapply(seq_along(items), function(y) {
              input[[sprintf("groups__%s__level__%s", x, y)]]
            })
            names(l) <- items
            l[["Total"]] <- input[[sprintf("groups__%s__level__%s", x, "all")]]
            l
          }
        })

        group_labels_call <- if (length(unlist(group_labels)) == 0) {
          quote(group_labels <- NULL)
        } else {
          bquote(group_labels <- setNames(.(group_labels), .(input$groups)))
        }

        q1 <- teal.code::eval_code(obj, code = group_labels_call) %>%
          teal.code::eval_code(code = "")

        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

        teal.code::eval_code(
          q1,
          code = as.expression(c(
            bquote(
              plot <- osprey::g_ae_sub(
                id = .(as.name(dataname))$USUBJID,
                arm = as.factor(.(as.name(dataname))[[.(input$arm_var)]]),
                arm_sl = as.character(ADSL[[.(input$arm_var)]]),
                trt = .(input$arm_trt),
                ref = .(input$arm_ref),
                subgroups = .(as.name(dataname))[.(input$groups)],
                subgroups_sl = ADSL[.(input$groups)],
                subgroups_levels = group_labels,
                conf_level = .(input$conf_level),
                diff_ci_method = .(input$ci),
                fontsize = .(font_size()),
                arm_n = .(input$arm_n),
                draw = TRUE
              )
            )
          ))
        )
      })
    )

    plot_r <- reactive(output_q()[["plot"]])
    set_chunk_dims(pws, output_q)
  })
}
