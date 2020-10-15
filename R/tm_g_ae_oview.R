#' Teal module for ae overview
#'
#' Display AE overview plot as a shiny module
#'
#' @inheritParams shared_params
#' @param dataname (\code{character}) analysis data used in teal module, needs to be
#' available in the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices
#' and pre-selected option for variable names that can be used as \code{arm_var}
#' @param add_flag a string or a vector of characters including variable name(s) for
#' additional flags, default is \code{NULL} (i.e. no additional flags will be added)
#' @param fontsize a numeric vector with 3 values, selected font size and font size range,
#' default is \code{c(5, 3, 7)}
#' @param plot_height optional, (\code{numeric}) a vector of length three with \code{c(value, min, max)}. Specifies
#'   the height of the main plot. Default is \code{c(600, 200, 2000)}.
#'
#' @return a \code{\link[teal]{module}} object
#' @importFrom rtables var_labels
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(teal.osprey)
#' library(rtables)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' # Add additional dummy causality flags.
#' ADAE <- ADAE %>%
#'   mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) %>%
#'   mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")) %>%
#'   rtables::var_relabel(
#'     AEREL1 = "AE related to A: Drug X",
#'     AEREL2 = "AE related to B: Placebo"
#'   )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAE", ADAE),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADAE <- radae(cached = TRUE)
#'             # Add additional dummy causality flags.
#'             ADAE <- ADAE %>%
#'               mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) %>%
#'               mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")) %>%
#'               rtables::var_relabel(
#'                 AEREL1 = "AE related to A: Drug X",
#'                 AEREL2 = "AE related to B: Placebo"
#'               )'),
#'   modules = root_modules(
#'     tm_g_ae_oview(
#'       label = "AE Overview",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(selected = "ACTARM",
#'                                  choices = c("ACTARM", "ACTARMCD")),
#'       add_flag = choices_selected(choices = variable_choices(ADAE,  c("AEREL1", "AEREL2")),
#'                                   selected = NULL),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_ae_oview <- function(label,
                          dataname,
                          arm_var,
                          add_flag = NULL,
                          fontsize = c(5, 3, 7),
                          plot_height = c(600L, 200L, 2000L),
                          plot_width = NULL
                          ) {
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(add_flag))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_ae_oview,
    server_args = list(
      label = label,
      dataname = dataname,
      add_flag = add_flag,
      plot_height = plot_height,
      plot_width = plot_width),
    ui = ui_g_ae_oview,
    ui_args = args,
    filters = dataname
  )
}

ui_g_ae_oview <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  standard_layout(
    output = white_small_well(
      plot_decorate_output(id = ns(NULL), plot_height = args$plot_height, plot_width = args$plot_width)
      ),
    encoding = div(
      optionalSelectInput(
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
        ns("flags_select"),
        "Flags",
        choices = NULL,
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("add_flags"),
        "Additional Flags",
        choices = args$add_flag$choices,
        multiple = TRUE
      ),
      panel_item(
        "Confidence interval settings",
        optionalSelectInput(
          ns("diff_ci_method"),
          "Method for Difference of Proportions CI",
          choices = ci_choices,
          selected = ci_choices[1],
          multiple = FALSE
        ),
        optionalSliderInput(
          ns("conf_level"),
          "Confidence Level",
          min = 0.5,
          max = 1,
          value = 0.95
        )
      ),
      optionalSelectInput(
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
    forms = tags$div(actionButton(ns("show_rcode"),
                                  "Show R Code",
                                  width = "100%"))
  )
}

srv_g_ae_oview <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           label,
                           add_flag,
                           plot_height,
                           plot_width) {
  init_chunks()
  font_size <- callModule(srv_g_decorate, id = NULL, plt = plt, plot_height = plot_height, plot_width = plot_width) # nolint

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
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    req(!is.null(input$arm_var))
    arm_var <- input$arm_var

    choices <- unique(ANL_FILTERED[[arm_var]])

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

    flags <- osprey::create_flag_vars(ANL_FILTERED)

    updateSelectInput(
      session,
      "flags_select",
      selected = names(flags),
      choices = names(flags))
  })

  plt <- reactive({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    # assign labels back to the data
    anl_labels <- rtables::var_labels(datasets$get_data(dataname, filtered = FALSE))
    if (!is.null(input$add_flags)) {
      add_flag_labels <- anl_labels[names(anl_labels) == input$add_flags] # nolint
    }

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    chunks_reset(envir = environment())

    validate(need(nlevels(ANL_FILTERED[[input$arm_var]]) > 1, "Arm needs to have at least 2 levels"))
    validate_has_data(ANL_FILTERED, min_nrow = 10)
    validate(need(all(c(input$arm_trt, input$arm_ref) %in% unique(ANL_FILTERED[[input$arm_var]])), "Plot loading"))

    chunks_push(bquote({
      id <- .(as.name(anl_name))[["USUBJID"]]
      arm <- .(as.name(anl_name))[[.(input$arm_var)]]
      arm_N <- table(ADSL_FILTERED[[.(input$arm_var)]]) # nolint
      trt <- .(input$arm_trt)
      ref <- .(input$arm_ref)
      flags <- .(as.name(anl_name)) %>%
        osprey::create_flag_vars() %>%
        select(.(input$flags_select))
    }))

    if (!is.null(input$add_flags)) {
      chunks_push(bquote({
        add_flag_df <- data.frame(.(as.name(anl_name))[, .(input$add_flags)])
        names(add_flag_df) <- .(add_flag_labels)
        flags <- do.call(bind_cols, c(flags, add_flag_df))
      }))
    }

    chunks_push_new_line()

    chunks_eval()

    validate(need((
      length(input$flags_select) + length(input$add_flags)) > 0,
      "Please select at least one flag."))
    validate(need(
      input$arm_trt != input$arm_ref,
      paste(
        "Treatment arm and control arm cannot be the same.",
        "Please select a different treatment arm or control arm",
        sep = "\n"
      )
    ))
    chunks_push(bquote({
      args <- list(
        term = flags,
        id = id,
        arm = arm,
        arm_N = arm_N,
        ref = .(input$arm_ref),
        trt = .(input$arm_trt),
        diff_ci_method = .(input$diff_ci_method),
        conf_level = .(input$conf_level),
        axis_side = .(input$axis),
        fontsize = .(font_size()),
        draw = TRUE
      )
    }))

    chunks_push_new_line()

    chunks_eval()

    validate(need(chunks_is_ok(), "Data could not be constructed."))

    args <- chunks_get_var("args")
    args$draw <- FALSE

    chunks_push(bquote({
      do.call(g_events_term_id, args = args)
    }))
    do.call(g_events_term_id, args = args) # nolint
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(title = label,
                     rcode = get_rcode(
                       datasets = datasets,
                       title = sprintf("R Code for %s", label)
                     ))
  })
}
