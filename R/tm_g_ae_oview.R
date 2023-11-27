#' Teal module for the `AE` overview
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Display the `AE` overview plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param flag_var_anl ([`teal.transform::choices_selected`])
#'   `choices_selected` object with variables used to count adverse event
#'   sub-groups (e.g. Serious events, Related events, etc.)
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @examples
#' data <- cdisc_data() |>
#'   within({
#'     library(nestcolor)
#'     ADSL <- rADSL
#'     ADAE <- rADAE
#'     add_event_flags <- function(dat) {
#'       dat <- dat |>
#'         mutate(
#'           TMPFL_SER = AESER == "Y",
#'           TMPFL_REL = AEREL == "Y",
#'           TMPFL_GR5 = AETOXGR == "5",
#'           AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
#'           AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
#'         )
#'       labels <- c(
#'         "Serious AE", "Related AE", "Grade 5 AE",
#'         "AE related to A: Drug X", "AE related to B: Placebo"
#'       )
#'       cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'       for (i in seq_along(labels)) {
#'         attr(dat[[cols[i]]], "label") <- labels[i]
#'       }
#'       dat
#'     }
#'     ADAE <- add_event_flags(ADAE)
#'   })
#'
#' datanames(data) <- c("ADSL", "ADAE")
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' ADAE <- data[["ADAE"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_ae_oview(
#'       label = "AE Overview",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         selected = "ACTARM",
#'         choices = c("ACTARM", "ACTARMCD")
#'       ),
#'       flag_var_anl = choices_selected(
#'         selected = "AEREL1",
#'         choices = variable_choices(
#'           ADAE,
#'           c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'         ),
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_ae_oview <- function(label,
                          dataname,
                          arm_var,
                          flag_var_anl,
                          fontsize = c(5, 3, 7),
                          plot_height = c(600L, 200L, 2000L),
                          plot_width = NULL) {
  logger::log_info("Initializing tm_g_ae_oview")
  checkmate::assert_class(arm_var, classes = "choices_selected")
  checkmate::assert_class(flag_var_anl, classes = "choices_selected")
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
  checkmate::assert_numeric(plot_height[1],
    lower = plot_height[2], upper = plot_height[3],
    .var.name = "plot_height"
  )
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ae_oview,
    server_args = list(
      label = label,
      dataname = dataname,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_ae_oview,
    ui_args = args,
    datanames = c("ADSL", dataname)
  )
}

ui_g_ae_oview <- function(id, ...) {
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
        ns("arm_var"),
        "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected,
        multiple = FALSE
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
      selectInput(
        ns("flag_var_anl"),
        "Flags",
        choices = args$flag_var_anl$choices,
        selected = args$flag_var_anl$selected,
        multiple = TRUE
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
      ui_g_decorate(
        ns(NULL),
        fontsize = args$fontsize,
        titles = "AE Overview",
        footnotes = ""
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    )
  )
}

srv_g_ae_oview <- function(id,
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
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      ANL <- data()[[dataname]] # nolint

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("arm_var", shinyvalidate::sv_required(
        message = "Arm Variable is required"
      ))
      iv$add_rule("arm_var", ~ if (!is.factor(ANL[[.]])) {
        "Arm Var must be a factor variable"
      })
      iv$add_rule("arm_var", ~ if (nlevels(ANL[[.]]) < 2L) {
        "Selected Arm Var must have at least two levels"
      })
      iv$add_rule("flag_var_anl", shinyvalidate::sv_required(
        message = "At least one Flag is required"
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
      id = NULL, plt = plot_r,
      plot_height = plot_height, plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    observeEvent(list(input$diff_ci_method, input$conf_level), {
      req(!is.null(input$diff_ci_method) && !is.null(input$conf_level))
      diff_ci_method <- input$diff_ci_method
      conf_level <- input$conf_level
      updateTextAreaInput(session,
        "foot",
        value = sprintf(
          "Note: %d%% CI is calculated using %s",
          round(conf_level * 100),
          name_ci(diff_ci_method)
        )
      )
    })

    observeEvent(input$arm_var, ignoreNULL = TRUE, {
      ANL <- data()[[dataname]] # nolint
      arm_var <- input$arm_var
      arm_val <- ANL[[arm_var]]
      choices <- levels(arm_val)

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
        ANL <- data()[[dataname]] # nolint

        teal::validate_has_data(ANL, min_nrow = 10, msg = sprintf("%s has not enough data", dataname))

        teal::validate_inputs(iv())

        validate(need(
          input$arm_trt %in% ANL[[input$arm_var]] && input$arm_ref %in% ANL[[input$arm_var]],
          "Treatment or Control not found in Arm Variable. Perhaps they have been filtered out?"
        ))

        q1 <- teal.code::eval_code(
          data(),
          code = as.expression(c(
            bquote(anl_labels <- formatters::var_labels(.(as.name(dataname)), fill = FALSE)),
            bquote(
              flags <- .(as.name(dataname)) %>%
                select(all_of(.(input$flag_var_anl))) %>%
                rename_at(vars(.(input$flag_var_anl)), function(x) paste0(x, ": ", anl_labels[x]))
            )
          ))
        )

        teal.code::eval_code(
          q1,
          code = as.expression(c(
            bquote(
              plot <- osprey::g_events_term_id(
                term = flags,
                id = .(as.name(dataname))[["USUBJID"]],
                arm = .(as.name(dataname))[[.(input$arm_var)]],
                arm_N = table(ADSL[[.(input$arm_var)]]),
                ref = .(input$arm_ref),
                trt = .(input$arm_trt),
                diff_ci_method = .(input$diff_ci_method),
                conf_level = .(input$conf_level),
                axis_side = .(input$axis),
                fontsize = .(font_size()),
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
      title = paste("R code for", label)
    )
    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "AE Overview",
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
