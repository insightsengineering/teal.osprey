#' Events by Term Plot Teal Module
#'
#' Display Events by Term plot as a shiny module
#'
#' @inheritParams shared_params
#' @param dataname (\code{character}) analysis data used in teal module, needs to be
#' available in the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param term_var \code{\link[teal]{choices_selected}} object with all available choices
#' and pre-selected option names that can be used to specify the term for events
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices
#' and pre-selected option for variable names that can be used as \code{arm_var}
#' @param fontsize (\code{numeric}) vector of choices for font size
#' @param plot_height optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the height of the main plot. Default is \code{c(600, 200, 2000)}.
#'
#' @return an \code{\link[teal]{module}} object
#' @importFrom rtables var_labels "var_labels<-"
#'
#' @export
#'
#' @author Liming Li (lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(random.cdisc.data)
#' library(teal.osprey)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- radae(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_events_term_id(
#'       label = "Common AE",
#'       dataname = "ADAE",
#'       term_var = choices_selected(selected = "AEDECOD",
#'                                   choices = c("AEDECOD", "AETERM",
#'                                               "AEHLT", "AELLT","AEBODSYS")),
#'       arm_var = choices_selected(selected = "ACTARMCD",
#'                                  choices = c("ACTARM", "ACTARMCD")),
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
                                plot_width = NULL
                                ) {
  stopifnot(is_character_single(label))
  stopifnot(is.choices_selected(term_var))
  stopifnot(is.choices_selected(arm_var))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)
  stopifnot(is_numeric_vector(fontsize))

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
  standard_layout(
    output = white_small_well(
      plot_decorate_output(id = ns(NULL), plot_height = args$plot_height, plot_width = args$plot_width)
      ),
    encoding = div(
      optionalSelectInput(
        ns("term"),
        "Term Variable",
        choices = args$term_var$choices,
        selected = args$term_var$selected
      ),
      optionalSelectInput(
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
      optionalSelectInput(
        ns("sort"),
        "Sort By",
        choices = c(
          "Term" = "term",
          "Risk Difference" = "riskdiff",
          "Mean Risk" = "meanrisk"
        ),
        selected = "term"
      ),
      panel_item(
        "Confidence interval settings",
        optionalSelectInput(
          ns("diff_ci_method"),
          "Method for Difference of Proportions CI",
          choices = ci_choices,
          selected = ci_choices[1]
        ),
        optionalSliderInput(
          ns("conf_level"),
          "Confidence Level",
          min = 0.5,
          max = 1,
          value = 0.95
        )
      ),
      panel_item(
        "Additional plot settings",
        optionalSelectInput(
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
                      value = FALSE)
      ),
      ui_g_decorate(
        ns(NULL),
        fontsize = args$fontsize,
        titles = "Common AEs",
        footnotes = ""
      )
    ),
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_g_events_term_id <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 label,
                                 plot_height,
                                 plot_width) {

  font_size <- callModule(srv_g_decorate, id = NULL, plt = plt, plot_height = plot_height, plot_width = plot_width) # nolint

  init_chunks()

  observe({
    req(!is.null(input$diff_ci_method) && !is.null(input$conf_level))
    diff_ci_method <- input$diff_ci_method
    conf_level <- input$conf_level
    updateTextAreaInput(session,
                        "foot",
                        value = sprintf(
                          "Note: %d%% CI is calculated using %s",
                          round(conf_level * 100),
                          name_ci(diff_ci_method)
                        ))
  })
  observe({
    req(input$sort)
    updateTextInput(session,
                    "title",
                    value = sprintf(
                      "Common AE Table Sorted by %s",
                      c(
                        "term" = "Term",
                        "riskdiff" = "Risk Difference",
                        "meanrisk" = "Mean Risk"
                      )[input$sort]
                    ))
  })

  observe({
    req(!is.null(input$arm_var))
    arm_var <- input$arm_var
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
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
      choices = choices)
    updateSelectInput(
      session,
      "arm_trt",
      selected = choices[trt_index],
      choices = choices)
  })

  plt <- reactive({

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
    rtables::var_labels(ANL_FILTERED) <- rtables::var_labels(datasets$get_data(dataname, filtered = FALSE)) # nolint

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    validate(need(all(c(input$arm_trt, input$arm_ref) %in% unique(ANL_FILTERED[[input$arm_var]])), "Plot loading"))

    chunks_reset(envir = environment())

    adsl_vars <- unique(c("USUBJID", "STUDYID", input$arm_var)) # nolint
    anl_vars <- c("USUBJID", "STUDYID", input$term) # nolint

    chunks_push(bquote({
      ANL <- merge( # nolint
        x = ADSL_FILTERED[, .(adsl_vars), drop = FALSE],
        y = .(as.name(anl_name))[, .(anl_vars), drop = FALSE],
        all.x = FALSE,
        all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )
    }))

    chunks_safe_eval()
    validate(need(nrow(chunks_get_var("ANL")) > 10, "need at least 10 data points"))

    chunks_push(bquote({
      term <- ANL[[.(input$term)]]
      id <- ANL$USUBJID
      arm <- ANL[[.(input$arm_var)]]
      arm_N <- table(ADSL_FILTERED[[.(input$arm_var)]]) # nolint
      ref <- .(input$arm_ref)
      trt <- .(input$arm_trt)

      args <- list(
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

    chunks_push_new_line()

    chunks_safe_eval()

    validate(need(
      input$arm_trt != input$arm_ref,
      paste("Treatment arm and control arm cannot be the same.",
            "Please select a different treatment arm or control arm",
            sep = "\n")
    ))

    args <- chunks_get_var("args")
    args$draw <- FALSE

    chunks_push(bquote({
      do.call(g_events_term_id, args = args)
    }))
    do.call(g_events_term_id, args = args) # nolint
  })

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = paste0("R code for ", label)
  )
}
