#' teal module for the `AE` by subgroups
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Display the `AE` by subgroups plot as a teal module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param group_var (`choices_selected`) subgroups variables. See [teal.transform::choices_selected()] for details.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @examples
#' # Example using stream (ADaM) dataset
#' data <- cdisc_data() |>
#'   within({
#'     ADSL <- rADSL
#'     ADAE <- rADAE
#'   })
#'
#' datanames(data) <- c("ADSL", "ADAE")
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ae_sub(
#'       label = "AE by Subgroup",
#'       dataname = "ADAE",
#'       arm_var = teal.transform::choices_selected(
#'         selected = "ACTARMCD",
#'         choices = c("ACTARM", "ACTARMCD")
#'       ),
#'       group_var = teal.transform::choices_selected(
#'         selected = c("SEX", "REGION1", "RACE"),
#'         choices = c("SEX", "REGION1", "RACE")
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
                        fontsize = c(5, 3, 7)) {
  logger::log_info("Initializing tm_g_ae_sub")
  checkmate::assert_class(arm_var, classes = "choices_selected")
  checkmate::assert_class(group_var, classes = "choices_selected")
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

  module(
    label = label,
    server = srv_g_ae_sub,
    server_args = list(
      label = label,
      dataname = dataname,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_ae_sub,
    ui_args = list(
      arm_var = arm_var,
      group_var = group_var,
      fontsize = fontsize
    ),
    datanames = c("ADSL", dataname)
  )
}

ui_g_ae_sub <- function(id, ...) {
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
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code("ADAE")),
      teal.widgets::optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      checkboxInput(
        ns("arm_n"),
        "Show N in each arm",
        value = args$arm_n
      ),
      teal.widgets::optionalSelectInput(
        ns("groups"),
        "Group Variable",
        choices = args$group_var$choices,
        selected = args$group_var$selected,
        multiple = TRUE
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
          fontsize = args$fontsize,
          titles = "AE Table with Subgroups",
          footnotes = ""
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    )
  )
}

srv_g_ae_sub <- function(id,
                         data,
                         filter_panel_api,
                         reporter,
                         dataname,
                         label,
                         plot_height,
                         plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      ANL <- data()[[dataname]] # nolint
      ADSL <- data()[["ADSL"]] # nolint

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("arm_var", shinyvalidate::sv_required(
        message = "Arm Variable is required"
      ))
      iv$add_rule("arm_var", ~ if (!is.factor(ANL[[.]])) {
        "Arm Var must be a factor variable, contact developer"
      })
      rule_diff <- function(value, other) {
        if (isTRUE(value == other)) "Control and Treatment must be different"
      }
      iv$add_rule("arm_trt", rule_diff, other = input$arm_ref)
      iv$add_rule("arm_ref", rule_diff, other = input$arm_trt)
      iv$add_rule("groups", shinyvalidate::sv_in_set(
        names(ANL),
        message_fmt = sprintf("Groups must be a variable in %s", dataname)
      ))
      iv$add_rule("groups", shinyvalidate::sv_in_set(
        names(ADSL),
        message_fmt = "Groups must be a variable in ADSL"
      ))
      iv$enable()
      iv
    })

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    observeEvent(input$arm_var, ignoreNULL = TRUE, {
      arm_var <- input$arm_var
      ANL <- data()[[dataname]] # nolint

      anl_val <- ANL[[arm_var]]
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
      ANL <- data()[[dataname]] # nolint
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
        ANL <- data()[[dataname]] # nolint
        ADSL <- data()[["ADSL"]] # nolint

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

        teal.code::eval_code(data(), code = group_labels_call) %>%
          teal.code::eval_code(code = "") %>%
          teal.code::eval_code(
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
              ),
              quote(plot)
            ))
          )
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
      verbatim_content = reactive(teal.code::get_code(output_q())),
      title = paste("R code for", label),
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "AE Subgroups",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(teal.code::get_code(output_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
