#' Teal module for the AE overview
#'
#' Display the AE overview plot as a shiny module
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
#' library(scda)
#' library(nestcolor)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' # Add additional dummy causality flags.
#' add_event_flags <- function(dat) {
#'   dat <- dat %>%
#'     dplyr::mutate(
#'       TMPFL_SER = AESER == "Y",
#'       TMPFL_REL = AEREL == "Y",
#'       TMPFL_GR5 = AETOXGR == "5",
#'       AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
#'       AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
#'     )
#'   labels <- c(
#'     "Serious AE", "Related AE", "Grade 5 AE",
#'     "AE related to A: Drug X", "AE related to B: Placebo"
#'   )
#'   cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
#'   for (i in seq_along(labels)) {
#'     attr(dat[[cols[i]]], "label") <- labels[i]
#'   }
#'   dat
#' }
#' ADAE <- ADAE %>% add_event_flags()
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADAE", ADAE,
#'       code =
#'         "ADAE <- synthetic_cdisc_data('latest')$adae
#'            add_event_flags <- function(dat) {
#'              dat <- dat %>%
#'                dplyr::mutate(
#'                  TMPFL_SER = AESER == 'Y',
#'                  TMPFL_REL = AEREL == 'Y',
#'                  TMPFL_GR5 = AETOXGR == '5',
#'                  AEREL1 = (AEREL == 'Y' & ACTARM == 'A: Drug X'),
#'                  AEREL2 = (AEREL == 'Y' & ACTARM == 'B: Placebo')
#'                )
#'              labels <- c(
#'                'Serious AE',
#'                'Related AE',
#'                'Grade 5 AE',
#'                'AE related to A: Drug X',
#'                'AE related to B: Placebo'
#'              )
#'              cols <- c('TMPFL_SER', 'TMPFL_REL', 'TMPFL_GR5', 'AEREL1', 'AEREL2')
#'              for (i in seq_along(labels)) {
#'               attr(dat[[cols[i]]], 'label') <- labels[i]
#'              }
#'              dat
#'            }
#'            # Generating user-defined event flags.
#'            ADAE <- ADAE %>% add_event_flags()"
#'     ),
#'     check = TRUE
#'   ),
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
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
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
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
    filters = dataname
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
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_g_ae_oview <- function(id,
                           datasets,
                           reporter,
                           dataname,
                           label,
                           plot_height,
                           plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("arm_var", shinyvalidate::sv_required(
      message = "Please select an arm variable."
    ))
    iv$add_rule("flag_var_anl", shinyvalidate::sv_required(
      message = "Please select at least one flag"
    ))
    iv$enable()

    teal.code::init_chunks()
    decorate_output <- srv_g_decorate(id = NULL, plt = plt, plot_height = plot_height, plot_width = plot_width)
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

    observeEvent(input$arm_var, {
      ANL <- datasets$get_data(dataname, filtered = FALSE) # nolint
      req(!is.null(input$arm_var))
      arm_var <- input$arm_var
      choices <- unique(ANL[[arm_var]])
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
        choices = choices
      )
      updateSelectInput(
        session,
        "arm_trt",
        selected = choices[trt_index],
        choices = choices
      )
    })

    plt <- reactive({
      validate(need(iv$is_valid(), "Misspecification error: please observe red flags in the interface."))

      ANL_UNFILTERED <- datasets$get_data(dataname, filtered = FALSE) # nolint
      ADSL <- datasets$get_data("ADSL", filtered = TRUE) # nolint
      ANL <- datasets$get_data(dataname, filtered = TRUE) # nolint

      anl_name <- dataname
      assign(anl_name, ANL)

      teal.code::chunks_reset(envir = environment())

      validate_has_data(ANL, min_nrow = 10)

      iv_comp <- shinyvalidate::InputValidator$new()
      iv_comp$add_rule("arm_trt", comp_arm, comparison = input$arm_ref)
      iv_comp$add_rule("arm_ref", comp_arm, comparison = input$arm_trt)
      iv_comp$enable()
      validate(need(iv_comp$is_valid(), "Misspecification error: please observe red flags in the interface."))

      validate(need(nlevels(ANL[[input$arm_var]]) > 1, "Arm needs to have at least 2 levels"))

      if (all(c(input$arm_trt, input$arm_ref) %in% ANL_UNFILTERED[[input$arm_var]])) {
        iv_an <- shinyvalidate::InputValidator$new()
        iv_an$add_rule("arm_ref", isin_dataset, anl = ANL, armv = input$arm_var)
        iv_an$add_rule("arm_trt", isin_dataset, anl = ANL, armv = input$arm_var)
        iv_an$enable()
        validate(need(iv_an$is_valid(), "Misspecification error: please observe red flags in the interface."))
      }
      validate(need(all(c(input$arm_trt, input$arm_ref) %in% unique(ANL[[input$arm_var]])), "Plot loading"))

      teal.code::chunks_push(
        id = "variables call",
        expression = bquote({
          id <- .(as.name(anl_name))[["USUBJID"]]
          arm <- .(as.name(anl_name))[[.(input$arm_var)]]
          arm_N <- table(ADSL[[.(input$arm_var)]]) # nolint
          trt <- .(input$arm_trt)
          ref <- .(input$arm_ref)
          anl_labels <- formatters::var_labels(.(as.name(anl_name)), fill = FALSE)
          flags <- .(as.name(anl_name)) %>%
            select(all_of(.(input$flag_var_anl))) %>%
            rename_at(vars(.(input$flag_var_anl)), function(x) paste0(x, ": ", anl_labels[x]))
        })
      )

      teal.code::chunks_push_new_line()

      teal.code::chunks_safe_eval()

      teal.code::chunks_push(
        id = "g_events_term_id call",
        expression = bquote({
          osprey::g_events_term_id(
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
        })
      )

      teal.code::chunks_safe_eval()
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = unique(c(
        dataname,
        vapply(X = dataname, FUN.VALUE = character(1), function(x) {
          if (inherits(datasets, "CDISCFilteredData")) datasets$get_parentname(x)
        })
      ))
    )
    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("AE Overview")
        card$append_text("AE Overview", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Plot", "header3")
        card$append_plot(plt(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 2L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
