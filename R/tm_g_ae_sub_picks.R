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
tm_g_ae_sub.picks <- function(label,
                              dataname = NULL,
                              arm_var = eal.picks::picks(
                                teal.picks::datasets(),
                                teal.picks::variables(
                                  choices = teal.picks::is_categorical(min.len = 2),
                                  selected = 1L
                                )
                              ),
                              group_var = teal.picks::picks(
                                teal.picks::datasets(),
                                teal.picks::variables(
                                  choices = teal.picks::is_categorical(min.len = 2),
                                  selected = 1L
                                )
                              ),
                              plot_height = c(600L, 200L, 2000L),
                              plot_width = NULL,
                              fontsize = c(5, 3, 7),
                              transformators = list()) {
  message("Initializing tm_g_ae_sub.picks")
  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_class(group_var, "picks")
  if (isTRUE(attr(group_var$variables, "multiple"))) {
    warning(
      "`arm_var` accepts only a single variable selection. ",
      "Forcing `teal.picks::variables(multiple)` to FALSE."
    )
    attr(arm_var$variables, "multiple") <- FALSE
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
    ui = ui_g_ae_sub.picks,
    server = srv_g_ae_sub.icks,
    ui_args = args[names(args) %in% names(formals(ui_g_ae_sub.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_ae_sub.picks))],
    transformators = transformators,
    datanames = .picks_datanames(list(arm_var, group_var))
  )
}


ui_g_ae_sub.picks <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code("ADAE")),
      tags$div(
        tags$strong("Term variable"),
        teal.picks::picks_ui(id = ns("term_var"), picks = args$term_var)
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = get_choices(args$arm_var$choices),
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = get_choices(args$arm_var$choices),
        selected = args$arm_var$selected
      ),
      checkboxInput(
        ns("arm_n"),
        "Show N in each arm",
        value = args$arm_n
      ),
      tags$div(
        tags$strong("Term variable"),
        teal.picks::picks_ui(id = ns("group_var"), picks = args$group_var)
      ),,
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
    )
  )
}

srv_g_ae_sub <- function(id,
                         data,
                         dataname,
                         label,
                         plot_height,
                         plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
}