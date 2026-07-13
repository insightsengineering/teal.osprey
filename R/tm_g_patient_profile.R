#' Patient Profile plot teal module
#'
#' @description
#'
#' Display patient profile plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param patient_id Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   describing the unique subject ID selection.
#' @param sl_dataname (`character`) subject level dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()]
#' @param ex_dataname,ae_dataname,rs_dataname,cm_dataname,lb_dataname
#'        (`character(1)`) names of exposure, adverse events, response,
#'        concomitant medications, and labs datasets, respectively;
#'        must be available in the list passed to the `data`
#'        argument of [teal::init()]\cr
#'        set to NA (default) to omit from analysis
#' @param sl_start_date Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the study start date variable, usually set to treatment start date or
#'   randomization date.
#' @param ex_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the exposure variable to plot as each line. Leave unspecified or set to
#'   `NULL` if exposure data is not available. q
#' @param ae_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the adverse event variable to plot as each line. Leave unspecified or
#'   set to `NULL` if adverse events data is not available.
#' @param ae_line_col_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for coloring `AE` lines. Leave unspecified or set to `NULL` if adverse
#'   events data is not available.
#' @param ae_line_col_opt aesthetic values to map color values
#'                        (named vector to map color values to each name).
#'                        If not `NULL`, please make sure this contains all possible
#'                        values for `ae_line_col_var` values. \cr
#'                        leave unspecified or set to `NULL` if adverse events data is not available
#' @param rs_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the response variable to plot as each line. Leave unspecified or set to
#'   `NULL` if response data is not available.
#' @param cm_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the concomitant medication variable to plot as each line. Leave
#'   unspecified or set to `NULL` if concomitant medications data is not available.
#' @param lb_var Either a [teal.transform::choices_selected()] `choices_selected`,
#'   a [teal.picks::variables()] object, or a full [teal.picks::picks()] object
#'   for the lab variable to plot as each line. Leave unspecified or set to
#'   `NULL` if labs data is not available.
#' @param x_limit a single `character` string with two numbers
#'                separated by a comma indicating the x-axis limit,
#'                default is "-28, 365"
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Tina Cho (chot) \email{tina.cho@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @details
#' As the patient profile module plots different domains in one plot, the study day (x-axis)
#' is derived for consistency based the start date of user's choice in the app (for example,
#' `ADSL.RANDDT` or `ADSL.TRTSDT`):
#' - In `ADAE`, `ADEX`, and `ADCM`, it would be study day based on `ASTDT` and/or
#'     `AENDT` in reference to the start date
#' - In `ADRS` and `ADLB`, it would be study day based on `ADT` in reference to
#'     the start date
#'
#' @export
#'
#' @examples
#' data <- teal_data() %>%
#'   within({
#'     library(nestcolor)
#'     library(dplyr)
#'     ADSL <- rADSL
#'     ADAE <- rADAE %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
#'     ADCM <- rADCM %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
#'     # The step below is to pre-process ADCM to legacy standard
#'     ADCM <- ADCM %>%
#'       select(-starts_with("ATC")) %>%
#'       unique()
#'     ADRS <- rADRS %>% mutate(ADT = as.Date(ADTM))
#'     ADEX <- rADEX %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
#'     ADLB <- rADLB %>% mutate(ADT = as.Date(ADTM), LBSTRESN = as.numeric(LBSTRESC))
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_patient_profile(
#'       label = "Patient Profile Plot",
#'       patient_id = variables(
#'         choices = unique(ADSL$USUBJID),
#'         selected = unique(ADSL$USUBJID)[1]
#'       ),
#'       sl_dataname = "ADSL",
#'       ex_dataname = "ADEX",
#'       ae_dataname = "ADAE",
#'       rs_dataname = "ADRS",
#'       cm_dataname = "ADCM",
#'       lb_dataname = "ADLB",
#'       sl_start_date = variables(
#'         selected = "TRTSDTM",
#'         choices = c("TRTSDTM", "RANDDT")
#'       ),
#'       ex_var = variables(
#'         selected = "PARCAT2",
#'         choices = "PARCAT2"
#'       ),
#'       ae_var = variables(
#'         selected = "AEDECOD",
#'         choices = c("AEDECOD", "AESOC")
#'       ),
#'       ae_line_col_var = variables(
#'         selected = "AESER",
#'         choices = c("AESER", "AEREL")
#'       ),
#'       ae_line_col_opt = c("Y" = "red", "N" = "blue"),
#'       rs_var = variables(
#'         selected = "PARAMCD",
#'         choices = "PARAMCD"
#'       ),
#'       cm_var = variables(
#'         selected = "CMDECOD",
#'         choices = c("CMDECOD", "CMCAT")
#'       ),
#'       lb_var = variables(
#'         selected = "LBTESTCD",
#'         choices = c("LBTESTCD", "LBCAT")
#'       ),
#'       x_limit = "-28, 750",
#'       plot_height = c(1200, 400, 5000)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_patient_profile <- function(label = "Patient Profile Plot",
                                 patient_id,
                                 sl_dataname,
                                 ex_dataname = NA,
                                 ae_dataname = NA,
                                 rs_dataname = NA,
                                 cm_dataname = NA,
                                 lb_dataname = NA,
                                 sl_start_date,
                                 ex_var = NULL,
                                 ae_var = NULL,
                                 ae_line_col_var = NULL,
                                 ae_line_col_opt = NULL,
                                 rs_var = NULL,
                                 cm_var = NULL,
                                 lb_var = NULL,
                                 x_limit = "-28, 365",
                                 plot_height = c(1200L, 400L, 5000L),
                                 plot_width = NULL,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 transformators = list()) {
  message("Initializing tm_g_patient_profile")
  checkmate::assert_string(label)
  checkmate::assert_string(sl_dataname)
  checkmate::assert_string(ex_dataname, na.ok = TRUE)
  checkmate::assert_string(ae_dataname, na.ok = TRUE)
  checkmate::assert_string(rs_dataname, na.ok = TRUE)
  checkmate::assert_string(cm_dataname, na.ok = TRUE)
  checkmate::assert_string(lb_dataname, na.ok = TRUE)
  checkmate::assert_character(
    c(sl_dataname, ex_dataname, rs_dataname, cm_dataname, lb_dataname),
    any.missing = TRUE, all.missing = FALSE
  )

  patient_id <- migrate_choices_selected_to_variables(patient_id, arg_name = "patient_id", multiple = FALSE)
  sl_start_date <- migrate_choices_selected_to_variables(sl_start_date, arg_name = "sl_start_date", multiple = FALSE)
  ex_var <- migrate_choices_selected_to_variables(ex_var, arg_name = "ex_var", multiple = FALSE, null.ok = TRUE)
  ae_var <- migrate_choices_selected_to_variables(ae_var, arg_name = "ae_var", multiple = FALSE, null.ok = TRUE)
  ae_line_col_var <- migrate_choices_selected_to_variables(
    ae_line_col_var,
    arg_name = "ae_line_col_var",
    multiple = FALSE,
    null.ok = TRUE
  )
  rs_var <- migrate_choices_selected_to_variables(rs_var, arg_name = "rs_var", multiple = FALSE, null.ok = TRUE)
  cm_var <- migrate_choices_selected_to_variables(cm_var, arg_name = "cm_var", multiple = FALSE, null.ok = TRUE)
  lb_var <- migrate_choices_selected_to_variables(lb_var, arg_name = "lb_var", multiple = FALSE, null.ok = TRUE)

  patient_id <- teal.picks::picks(
    teal.picks::datasets(sl_dataname, sl_dataname),
    patient_id,
    teal.picks::values(
      choices = function(x) !is.na(x),
      multiple = FALSE
    )
  )
  sl_start_date <- create_picks_helper(teal.picks::datasets(sl_dataname, sl_dataname), sl_start_date)
  if (!is.null(ex_var) && !is.na(ex_dataname)) {
    ex_var <- create_picks_helper(teal.picks::datasets(ex_dataname, ex_dataname), ex_var)
  }
  if (!is.null(ae_var) && !is.na(ae_dataname)) {
    ae_var <- create_picks_helper(teal.picks::datasets(ae_dataname, ae_dataname), ae_var)
  }
  if (!is.null(ae_line_col_var) && !is.na(ae_dataname)) {
    ae_line_col_var <- create_picks_helper(teal.picks::datasets(ae_dataname), ae_line_col_var)
  }
  if (!is.null(rs_var) && !is.na(rs_dataname)) {
    rs_var <- create_picks_helper(teal.picks::datasets(rs_dataname), rs_var)
  }
  if (!is.null(cm_var) && !is.na(cm_dataname)) {
    cm_var <- create_picks_helper(teal.picks::datasets(cm_dataname), cm_var)
  }
  if (!is.null(lb_var) && !is.na(lb_dataname)) {
    lb_var <- create_picks_helper(teal.picks::datasets(lb_dataname), lb_var)
  }

  checkmate::assert_string(x_limit)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_patient_profile,
    ui_args = args[names(args) %in% names(formals(ui_g_patient_profile))],
    server = srv_g_patient_profile,
    server_args = args[names(args) %in% names(formals(srv_g_patient_profile))],
    transformators = transformators,
    datanames = "all"
  )
}

ui_g_patient_profile <- function(id,
                                 patient_id,
                                 ex_dataname,
                                 ae_dataname,
                                 rs_dataname,
                                 cm_dataname,
                                 lb_dataname,
                                 sl_start_date,
                                 ex_var,
                                 ae_var,
                                 ae_line_col_var,
                                 rs_var,
                                 cm_var,
                                 lb_var,
                                 x_limit,
                                 pre_output,
                                 post_output) {
  ns <- NS(id)
  checkboxes <- c(ex_dataname, ae_dataname, rs_dataname, lb_dataname, cm_dataname)

  shiny::tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("patientprofileplot"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        tags$div(
          tags$strong("Patient ID"),
          teal.picks::picks_ui(id = ns("patient_id"), picks = patient_id)
        ),
        tags$div(
          tagList(
            helpText("Select", tags$code("ADaM"), "Domains"),
            checkboxGroupInput(
              inputId = ns("select_ADaM"),
              label = NULL,
              choices = checkboxes[!is.na(checkboxes)],
              selected = checkboxes[!is.na(checkboxes)]
            )
          )
        ),
        tags$div(
          tags$strong("Start date variable"),
          helpText("from ", tags$code("ADSL")),
          teal.picks::picks_ui(id = ns("sl_start_date"), picks = sl_start_date)
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", ex_dataname),
          ns = ns,
          if (!is.null(ex_var)) {
            tags$div(
              tags$strong("Exposure variable"),
              teal.picks::picks_ui(id = ns("ex_var"), picks = ex_var)
            )
          }
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", ae_dataname),
          ns = ns,
          if (!is.null(ae_var)) {
            tags$div(
              tags$strong("Adverse Event variable"),
              teal.picks::picks_ui(id = ns("ae_var"), picks = ae_var)
            )
          },
          if (!is.null(ae_line_col_var)) {
            tags$div(
              tags$strong("Adverse Event line color variable"),
              teal.picks::picks_ui(id = ns("ae_line_col_var"), picks = ae_line_col_var)
            )
          }
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", rs_dataname),
          ns = ns,
          if (!is.null(rs_var)) {
            tags$div(
              tags$strong("Tumor response variable"),
              teal.picks::picks_ui(id = ns("rs_var"), picks = rs_var)
            )
          }
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", cm_dataname),
          ns = ns,
          if (!is.null(cm_var)) {
            tags$div(
              tags$strong("Concomitant medicine variable"),
              teal.picks::picks_ui(id = ns("cm_var"), picks = cm_var)
            )
          }
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", lb_dataname),
          ns = ns,
          if (!is.null(lb_var)) {
            tags$div(
              tags$strong("Lab variable"),
              teal.picks::picks_ui(id = ns("lb_var"), picks = lb_var)
            )
          },
          selectInput(
            ns("lb_var_show"),
            "Lab values",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          )
        ),
        textInput(
          ns("x_limit"),
          label = tags$div(
            "Study Days Range",
            tags$br(),
            helpText("Enter TWO numeric values of study days range, separated by comma (eg. -28, 750)")
          ),
          value = x_limit
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

#' @keywords internal
.patient_profile_picks_selected_var <- function(selector_state) {
  if (is.null(selector_state) || is.null(selector_state$variables)) {
    return(character())
  }
  as.character(selector_state$variables$selected)
}

#' @keywords internal
.patient_profile_picks_selected_values <- function(selector_state) {
  if (is.null(selector_state) || is.null(selector_state$values)) {
    return(character())
  }
  as.character(selector_state$values$selected)
}

srv_g_patient_profile <- function(id,
                                  data,
                                  patient_id,
                                  sl_start_date,
                                  ex_var,
                                  ae_var,
                                  ae_line_col_var,
                                  rs_var,
                                  cm_var,
                                  lb_var,
                                  sl_dataname,
                                  ex_dataname,
                                  ae_dataname,
                                  rs_dataname,
                                  lb_dataname,
                                  cm_dataname,
                                  label,
                                  ae_line_col_opt,
                                  plot_height,
                                  plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  if (!is.na(ex_dataname)) checkmate::assert_names(ex_dataname, subset.of = names(data))
  if (!is.na(ae_dataname)) checkmate::assert_names(ae_dataname, subset.of = names(data))
  if (!is.na(rs_dataname)) checkmate::assert_names(rs_dataname, subset.of = names(data))
  if (!is.na(lb_dataname)) checkmate::assert_names(lb_dataname, subset.of = names(data))
  if (!is.na(cm_dataname)) checkmate::assert_names(cm_dataname, subset.of = names(data))
  checkboxes <- c(ex_dataname, ae_dataname, rs_dataname, lb_dataname, cm_dataname)
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = Filter(
        Negate(is.null),
        list(
          patient_id = patient_id,
          sl_start_date = sl_start_date,
          ex_var = ex_var,
          ae_var = ae_var,
          ae_line_col_var = ae_line_col_var,
          rs_var = rs_var,
          cm_var = cm_var,
          lb_var = lb_var
        )
      ),
      data = data
    )

    select_plot <- reactive(
      vapply(checkboxes, function(x) x %in% input$select_ADaM, logical(1L))
    )

    if (!is.na(lb_dataname) && !is.null(lb_var)) {
      observeEvent(.patient_profile_picks_selected_var(selectors$lb_var()), ignoreNULL = TRUE, {
        ADLB <- data()[[lb_dataname]]
        lb_var_selected <- .patient_profile_picks_selected_var(selectors$lb_var())
        req(length(lb_var_selected) > 0)
        choices <- unique(ADLB[[lb_var_selected]])
        choices_selected <- if (length(choices) > 5) choices[1:5] else choices

        updateSelectInput(
          session,
          "lb_var_show",
          selected = choices_selected,
          choices = choices
        )
      })
    }

    # render plot
    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        obj <- data()
        teal.reporter::teal_card(obj) <-
          c(
            teal.reporter::teal_card(obj),
            teal.reporter::teal_card("## Module's output(s)")
          )
        obj <- teal.code::eval_code(obj, "library(dplyr)")

        pick_selected <- function(name) {
          if (name %in% names(selectors)) {
            return(.patient_profile_picks_selected_var(selectors[[name]]()))
          }
          character()
        }

        # get inputs ---
        patient_id <- .patient_profile_picks_selected_values(selectors$patient_id())
        sl_start_date <- pick_selected("sl_start_date")
        ae_var <- pick_selected("ae_var")
        ae_line_col_var <- pick_selected("ae_line_col_var")
        rs_var <- pick_selected("rs_var")
        cm_var <- pick_selected("cm_var")
        ex_var <- pick_selected("ex_var")
        lb_var <- pick_selected("lb_var")
        x_limit <- input$x_limit
        lb_var_show <- input$lb_var_show

        validated_q <- obj

        teal::validate_input(
          "select_ADaM",
          condition = function(x) length(x) > 0L,
          message = "At least one ADaM data set is required"
        )
        if (isTRUE(select_plot()[lb_dataname])) {
          teal::validate_input(
            "lb_var_show",
            condition = function(x) length(x) > 0L,
            message = "At least one Lab value is required"
          )
        }
        teal::validate_input(
          "x_limit",
          condition = function(x) length(x) == 1L && nzchar(trimws(x)),
          message = "Study Days Range is required"
        )
        teal::validate_input(
          "x_limit",
          condition = function(x) {
            vals <- suppressWarnings(as_numeric_from_comma_sep_str(x))
            !anyNA(vals)
          },
          message = "Study Days Range is invalid"
        )
        teal::validate_input(
          "x_limit",
          condition = function(x) length(suppressWarnings(as_numeric_from_comma_sep_str(x))) == 2L,
          message = "Study Days Range must be two values"
        )
        teal::validate_input(
          "x_limit",
          condition = function(x) {
            identical(order(suppressWarnings(as_numeric_from_comma_sep_str(x))), 1:2)
          },
          message = "Study Days Range mut be: first lower, then upper limit"
        )

        teal::validate_input(
          "patient_id-values-selected",
          condition = function(x) length(x) > 0L,
          message = "Patient ID is required."
        )
        teal::validate_input(
          "sl_start_date-variables-selected",
          condition = function(x) length(x) > 0L,
          message = "Date variable is required."
        )
        if (isTRUE(select_plot()[ex_dataname])) {
          teal::validate_input(
            "ex_var-variables-selected",
            condition = function(x) length(x) > 0L,
            message = "Exposure variable is required."
          )
        }
        if (isTRUE(select_plot()[ae_dataname])) {
          teal::validate_input(
            "ae_var-variables-selected",
            condition = function(x) length(x) > 0L,
            message = "Adverse Event variable is required."
          )
        }
        if (isTRUE(select_plot()[rs_dataname])) {
          teal::validate_input(
            "rs_var-variables-selected",
            condition = function(x) length(x) > 0L,
            message = "Tumor response variable is required."
          )
        }
        if (isTRUE(select_plot()[cm_dataname])) {
          teal::validate_input(
            "cm_var-variables-selected",
            condition = function(x) length(x) > 0L,
            message = "Concomitant medicine variable is required."
          )
        }
        if (isTRUE(select_plot()[lb_dataname])) {
          teal::validate_input(
            "lb_var-variables-selected",
            condition = function(x) length(x) > 0L,
            message = "Lab variable is required."
          )
          teal::validate_input(
            "lb_var_show",
            condition = function(x) length(x) > 0L,
            message = "At least one Lab value is required."
          )
          teal::validate_input(
            c("lb_var-variables-selected", "lb_var_show"),
            condition = function(lb_var, lb_var_show) !isTRUE(any(lb_var == lb_var_show)),
            message = "Lab variable and Lab value must be different"
          )
        }
        if (isTRUE(select_plot()[ae_dataname]) && length(ae_line_col_var) > 0L && !is.null(ae_line_col_opt)) {
          teal::validate_input(
            "ae_line_col_var-variables-selected",
            condition = function(x) {
              length(levels(validated_q[[ae_dataname]][[x]])) <= length(ae_line_col_opt)
            },
            message = "Not enough colors provided for Adverse Event line color, unselect"
          )
        }

        adrs_vars <- unique(c(
          "USUBJID", "STUDYID", "PARAMCD",
          "PARAM", "AVALC", "AVAL", "ADY",
          "ADT", rs_var
        ))
        adae_vars <- unique(c(
          "USUBJID", "STUDYID", "ASTDT",
          "AENDT", "AESOC", "AEDECOD",
          "AESER", "AETOXGR", "AEREL",
          "ASTDY", "AENDY",
          ae_var, ae_line_col_var
        ))
        adcm_vars <- unique(c(
          "USUBJID", "STUDYID", "ASTDT",
          "AENDT", "ASTDT", "CMDECOD",
          "ASTDY", "AENDY", "CMCAT",
          cm_var
        ))
        adex_vars <- unique(c(
          "USUBJID", "STUDYID", "ASTDT",
          "AENDT", "PARCAT2", "AVAL",
          "AVALU", "PARAMCD", "PARCAT1",
          "PARCAT2", ex_var
        ))
        adlb_vars <- unique(c(
          "USUBJID", "STUDYID", "ANRIND", "LBSEQ",
          "PARAMCD", "BASETYPE", "ADT", "AVISITN",
          "LBSTRESN", "LBCAT", "LBTESTCD",
          lb_var
        ))

        # get ADSL dataset ---
        ADSL <- validated_q[[sl_dataname]]

        ADEX <- NULL
        if (isTRUE(select_plot()[ex_dataname])) {
          ADEX <- validated_q[[ex_dataname]]
          teal::validate_has_variable(ADEX, adex_vars)
        }
        ADAE <- NULL
        if (isTRUE(select_plot()[ae_dataname])) {
          ADAE <- validated_q[[ae_dataname]]
          teal::validate_has_variable(ADAE, adae_vars)
        }
        ADRS <- NULL
        if (isTRUE(select_plot()[rs_dataname])) {
          ADRS <- validated_q[[rs_dataname]]
          teal::validate_has_variable(ADRS, adrs_vars)
        }
        ADCM <- NULL
        if (isTRUE(select_plot()[cm_dataname])) {
          ADCM <- validated_q[[cm_dataname]]
          teal::validate_has_variable(ADCM, adcm_vars)
        }
        ADLB <- NULL
        if (isTRUE(select_plot()[lb_dataname])) {
          ADLB <- validated_q[[lb_dataname]]
          teal::validate_has_variable(ADLB, adlb_vars)
        }

        empty_rs <- FALSE
        empty_ae <- FALSE
        empty_cm <- FALSE
        empty_ex <- FALSE
        empty_lb <- FALSE

        q1 <- teal.code::eval_code(
          validated_q,
          code = substitute(
            expr = {
              ADSL <- ADSL %>%
                filter(USUBJID == patient_id) %>%
                group_by(USUBJID) %>%
                mutate(
                  max_date = pmax(as.Date(LSTALVDT), as.Date(DTHDT), na.rm = TRUE),
                  max_day = as.numeric(difftime(as.Date(max_date), as.Date(sl_start_date), units = "days")) +
                    (as.Date(max_date) >= as.Date(sl_start_date))
                )
            },
            env = list(
              ADSL = as.name(sl_dataname),
              sl_start_date = as.name(sl_start_date),
              patient_id = patient_id
            )
          )
        )

        # ADSL with single subject
        teal::validate_input(
          "patient_id-values-selected",
          condition = function(x) nrow(q1[["ADSL"]]) >= 1,
          message = paste(
            "Subject",
            patient_id,
            "not found in the dataset. Perhaps they have been filtered out by the filter panel?"
          )
        )

        # name for ae_line_col
        q1 <- if (!is.null(ae_line_col_var) && is.data.frame(ADAE)) {
          teal.code::eval_code(
            q1,
            code = substitute(
              expr = ae_line_col_name <- formatters::var_labels(ADAE, fill = FALSE)[ae_line_col_var],
              env = list(ADAE = as.name(ae_dataname), ae_line_col_var = ae_line_col_var)
            )
          )
        } else {
          teal.code::eval_code(q1, code = quote(ae_line_col_name <- NULL))
        }

        q1 <- if (isTRUE(select_plot()[ae_dataname])) {
          if (all(ADAE$USUBJID %in% ADSL$USUBJID)) {
            qq <- teal.code::eval_code(
              q1,
              code = substitute(
                expr = {
                  # ADAE
                  ADAE <- ADAE[, adae_vars]

                  ADAE <- ADSL %>%
                    left_join(ADAE, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    filter(!is.na(ASTDT), !is.na(AENDT)) %>%
                    mutate(
                      ASTDY = as.numeric(difftime(ASTDT, as.Date(sl_start_date), units = "days")) +
                        (ASTDT >= as.Date(sl_start_date)),
                      AENDY = as.numeric(difftime(AENDT, as.Date(sl_start_date), units = "days")) +
                        (AENDT >= as.Date(sl_start_date))
                    ) %>%
                    select(c(adae_vars, ASTDY, AENDY))
                  formatters::var_labels(ADAE)[ae_line_col_var] <-
                    formatters::var_labels(ADAE, fill = FALSE)[ae_line_col_var]
                },
                env = list(
                  ADSL = as.name(sl_dataname),
                  ADAE = as.name(ae_dataname),
                  sl_start_date = as.name(sl_start_date),
                  ae_line_col_var = ae_line_col_var,
                  adae_vars = adae_vars
                )
              )
            ) %>%
              teal.code::eval_code(
                code = substitute(
                  expr = ae <- list(
                    data = data.frame(ADAE),
                    var = as.vector(ADAE[, ae_var]),
                    line_col = line_col,
                    line_col_legend = line_col_legend,
                    line_col_opt = line_col_opt
                  ),
                  env = list(
                    ADAE = as.name(ae_dataname),
                    ae_var = ae_var,
                    line_col = if (!is.null(ae_line_col_var)) bquote(as.vector(ADAE[, .(ae_line_col_var)])) else NULL,
                    line_col_legend = ae_line_col_var,
                    line_col_opt = ae_line_col_opt
                  )
                )
              )
            ADAE <- qq[[ae_dataname]]
            if (is.null(ADAE) | nrow(ADAE) == 0) {
              empty_ae <- TRUE
            }
            qq
          } else {
            empty_ae <- TRUE
            teal.code::eval_code(q1, code = quote(ae <- NULL))
          }
        } else {
          teal.code::eval_code(q1, code = quote(ae <- NULL))
        }

        q1 <- if (isTRUE(select_plot()[rs_dataname])) {
          if (all(ADRS$USUBJID %in% ADSL$USUBJID)) {
            qq <- teal.code::eval_code(
              q1,
              code = substitute(
                expr = {
                  ADRS <- ADRS[, adrs_vars]
                  ADRS <- ADSL %>%
                    left_join(ADRS, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    mutate(
                      ADY = as.numeric(difftime(ADT, as.Date(sl_start_date), units = "days")) +
                        (ADT >= as.Date(sl_start_date))
                    ) %>%
                    select(USUBJID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADT) %>%
                    filter(is.na(ADY) == FALSE)
                  rs <- list(data = data.frame(ADRS), var = as.vector(ADRS[, rs_var]))
                },
                env = list(
                  ADRS = as.name(rs_dataname),
                  adrs_vars = adrs_vars,
                  sl_start_date = as.name(sl_start_date),
                  rs_var = rs_var
                )
              )
            )
            ADRS <- qq[[rs_dataname]]
            if (is.null(ADRS) || nrow(ADRS) == 0) {
              empty_rs <- TRUE
            }
            qq
          } else {
            empty_rs <- TRUE
            teal.code::eval_code(q1, expression = quote(rs <- NULL))
          }
        } else {
          teal.code::eval_code(q1, code = quote(rs <- NULL))
        }

        q1 <- if (isTRUE(select_plot()[cm_dataname])) {
          if (all(ADCM$USUBJID %in% ADSL$USUBJID)) {
            qq <- teal.code::eval_code(
              q1,
              code = substitute(
                expr = {
                  # ADCM
                  ADCM <- ADCM[, adcm_vars]
                  ADCM <- ADSL %>%
                    left_join(ADCM, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    filter(!is.na(ASTDT), !is.na(AENDT)) %>%
                    mutate(
                      ASTDY = as.numeric(difftime(ASTDT, as.Date(sl_start_date), units = "days")) +
                        (ASTDT >= as.Date(sl_start_date)),
                      AENDY = as.numeric(difftime(AENDT, as.Date(sl_start_date), units = "days")) +
                        (AENDT >= as.Date(sl_start_date))
                    ) %>%
                    select(USUBJID, ASTDT, AENDT, ASTDY, AENDY, !!quo(cm_var))
                  if (length(unique(ADCM$USUBJID)) > 0) {
                    ADCM <- ADCM[which(ADCM$AENDY >= -28 | is.na(ADCM$AENDY) == TRUE & is.na(ADCM$ASTDY) == FALSE), ]
                  }
                  cm <- list(data = data.frame(ADCM), var = as.vector(ADCM[, cm_var]))
                },
                env = list(
                  ADSL = as.name(sl_dataname),
                  ADCM = as.name(cm_dataname),
                  sl_start_date = as.name(sl_start_date),
                  adcm_vars = adcm_vars,
                  cm_var = cm_var
                )
              )
            )

            ADCM <- qq[[cm_dataname]]
            if (is.null(ADCM) | nrow(ADCM) == 0) {
              empty_cm <- TRUE
            }
            qq
          } else {
            empty_cm <- TRUE
            teal.code::eval_code(q1, code = quote(cm <- NULL))
          }
        } else {
          teal.code::eval_code(q1, code = quote(cm <- NULL))
        }

        q1 <- if (isTRUE(select_plot()[ex_dataname])) {
          if (all(ADEX$USUBJID %in% ADSL$USUBJID)) {
            qq <- teal.code::eval_code(
              q1,
              code = substitute(
                expr = {
                  # ADEX
                  ADEX <- ADEX[, adex_vars]
                  ADEX <- ADSL %>%
                    left_join(ADEX, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    filter(PARCAT1 == "INDIVIDUAL" & PARAMCD == "DOSE" & !is.na(AVAL) & !is.na(ASTDT)) %>%
                    select(USUBJID, ASTDT, PARCAT2, AVAL, AVALU, PARAMCD, sl_start_date)

                  ADEX <- split(ADEX, ADEX$USUBJID) %>%
                    lapply(function(pinfo) {
                      pinfo %>%
                        arrange(PARCAT2, PARAMCD, ASTDT) %>%
                        ungroup() %>%
                        mutate(
                          diff = c(0, diff(AVAL, lag = 1)),
                          Modification = case_when(
                            diff < 0 ~ "Decrease",
                            diff > 0 ~ "Increase",
                            diff == 0 ~ "None"
                          ),
                          ASTDT_dur = as.numeric(difftime(as.Date(ASTDT), as.Date(sl_start_date), units = "days")) +
                            (as.Date(ASTDT) >= as.Date(sl_start_date))
                        )
                    }) %>%
                    Reduce(rbind, .) %>%
                    as.data.frame() %>%
                    select(-diff)
                  ex <- list(data = data.frame(ADEX), var = as.vector(ADEX[, ex_var]))
                },
                env = list(
                  ADSL = as.name(sl_dataname),
                  ADEX = as.name(ex_dataname),
                  adex_vars = adex_vars,
                  sl_start_date = as.name(sl_start_date),
                  ex_var = ex_var
                )
              )
            )
            ADEX <- qq[[ex_dataname]]
            if (is.null(ADEX) | nrow(ADEX) == 0) {
              empty_ex <- TRUE
            }
            qq
          } else {
            empty_ex <- TRUE
            teal.code::eval_code(q1, code = quote(ex <- NULL))
          }
        } else {
          teal.code::eval_code(q1, code = quote(ex <- NULL))
        }

        q1 <- if (isTRUE(select_plot()[lb_dataname])) {
          if (all(ADLB$USUBJID %in% ADSL$USUBJID)) {
            qq <- teal.code::eval_code(
              q1,
              code = substitute(
                expr = {
                  ADLB <- ADLB[, adlb_vars]
                  ADLB <- ADSL %>%
                    left_join(ADLB, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    mutate(
                      ANRIND = factor(ANRIND, levels = c("HIGH", "LOW", "NORMAL"))
                    ) %>%
                    filter(!is.na(LBSTRESN) & !is.na(ANRIND) & .data[[lb_var]] %in% lb_var_show) %>%
                    as.data.frame() %>%
                    select(
                      USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADT, AVISITN, sl_start_date, LBTESTCD, ANRIND, lb_var
                    ) %>%
                    mutate(
                      ADY = as.numeric(difftime(ADT, as.Date(sl_start_date), units = "days")) +
                        (ADT >= as.Date(sl_start_date))
                    )
                  lb <- list(data = data.frame(ADLB), var = as.vector(ADLB[, lb_var]))
                },
                env = list(
                  ADLB = as.name(lb_dataname),
                  ADSL = as.name(sl_dataname),
                  adlb_vars = adlb_vars,
                  sl_start_date = as.name(sl_start_date),
                  lb_var = lb_var,
                  lb_var_show = lb_var_show
                )
              )
            )

            ADLB <- qq[[lb_dataname]]
            if (is.null(ADLB) | nrow(ADLB) == 0) {
              empty_lb <- TRUE
            }
            qq
          } else {
            empty_lb <- TRUE
            teal.code::eval_code(q1, code = quote(lb <- NULL))
          }
        } else {
          teal.code::eval_code(q1, code = quote(lb <- NULL))
        }

        # Check the subject has information in at least one selected domain
        empty_data_check <- structure(
          c(empty_ex, empty_ae, empty_rs, empty_lb, empty_cm),
          names = checkboxes
        )

        teal::validate_input(
          "select_ADaM",
          condition = function(x) any(!empty_data_check & select_plot()),
          message = "The subject does not have information in any selected domain."
        )

        # Check the subject has information in all the selected domains
        if (any(empty_data_check & select_plot())) {
          showNotification(
            paste0(
              "This subject does not have information in the ",
              paste(checkboxes[empty_data_check & select_plot()], collapse = ", "),
              " domain."
            ),
            duration = 8,
            type = "warning"
          )
        }

        # Convert x_limit to numeric vector
        if (!is.null(x_limit) || x_limit != "") {
          q1 <- teal.code::eval_code(
            q1,
            code = bquote(x_limit <- as.numeric(unlist(strsplit(.(x_limit), ","))))
          )
          x_limit <- q1[["x_limit"]]
        }

        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

        q1 <- teal.code::eval_code(
          q1,
          code = substitute(
            expr = {
              plot <- osprey::g_patient_profile(
                ex = ex,
                ae = ae,
                rs = rs,
                cm = cm,
                lb = lb,
                arrow_end_day = ADSL[["max_day"]],
                xlim = x_limit,
                xlab = "Study Day",
                title = paste("Patient Profile: ", patient_id)
              )
              plot
            },
            env = list(
              patient_id = patient_id,
              ADSL = as.name(sl_dataname)
            )
          )
        )
      })
    )

    plot_r <- reactive(output_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "patientprofileplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, output_q)
  })
}
