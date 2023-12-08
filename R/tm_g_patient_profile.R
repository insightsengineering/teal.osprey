#' Patient Profile plot teal module
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Display patient profile plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param patient_id (\code{choices_seleced}) unique subject ID variable
#' @param sl_dataname (\code{character}) subject level dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}}
#' @param ex_dataname,ae_dataname,rs_dataname,cm_dataname,lb_dataname
#'        (\code{character(1)}) names of exposure, adverse events, response,
#'        concomitant medications, and labs datasets, respectively;
#'        must be available in the list passed to the \code{data}
#'        argument of \code{\link[teal]{init}}\cr
#'        set to NA (default) to omit from analysis
#' @param sl_start_date (\code{choices_selected}) study start date variable, usually set to
#'                      treatment start date or randomization date
#' @param ex_var (\code{choices_selected}) exposure variable to plot as each line \cr
#'               leave unspecified or set to \code{NULL} if exposure data is not available
#' @param ae_var (\code{choices_selected}) adverse event variable to plot as each line \cr
#'               leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param ae_line_col_var (\code{choices_selected}) variable for coloring `AE` lines \cr
#'                        leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param ae_line_col_opt aesthetic values to map color values
#'                        (named vector to map color values to each name).
#'                        If not \code{NULL}, please make sure this contains all possible
#'                        values for \code{ae_line_col_var} values. \cr
#'                        leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param rs_var (\code{choices_selected}) response variable to plot as each line \cr
#'               leave unspecified or set to \code{NULL} if response data is not available
#' @param cm_var (\code{choices_selected}) concomitant medication variable
#'               to plot as each line \cr
#'               leave unspecified or set to \code{NULL} if concomitant medications data is not available
#' @param lb_var (\code{choices_selected}) lab variable to plot as each line \cr
#'               leave unspecified or set to \code{NULL} if labs data is not available
#' @param x_limit a single \code{character} string with two numbers
#'                separated by a comma indicating the x-axis limit,
#'                default is \code{"-28, 365"}
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Tina Cho (chot) \email{tina.cho@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @inherit argument_convention return
#'
#' @details
#' As the patient profile module plots different domains in one plot, the study day (x-axis)
#' is derived for consistency based the start date of user's choice in the app (for example,
#' \code{ADSL.RANDDT} or \code{ADSL.TRTSDT}):
#' \itemize{
#' \item In \code{ADAE}, \code{ADEX}, and \code{ADCM}, it would be study day based on \code{ASTDT} and/or
#'     \code{AENDT} in reference to the start date
#' \item In \code{ADRS} and \code{ADLB}, it would be study day based on \code{ADT} in reference to
#'     the start date
#' }
#'
#' @export
#'
#' @examples
#' data <- cdisc_data() |>
#'   within({
#'     library(nestcolor)
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
#' datanames(data) <- c("ADSL", "ADAE", "ADCM", "ADRS", "ADEX", "ADLB")
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' ADSL <- data[["ADSL"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_patient_profile(
#'       label = "Patient Profile Plot",
#'       patient_id = choices_selected(
#'         choices = unique(ADSL$USUBJID),
#'         selected = unique(ADSL$USUBJID)[1]
#'       ),
#'       sl_dataname = "ADSL",
#'       ex_dataname = "ADEX",
#'       ae_dataname = "ADAE",
#'       rs_dataname = "ADRS",
#'       cm_dataname = "ADCM",
#'       lb_dataname = "ADLB",
#'       sl_start_date = choices_selected(
#'         selected = "TRTSDTM",
#'         choices = c("TRTSDTM", "RANDDT")
#'       ),
#'       ex_var = choices_selected(
#'         selected = "PARCAT2",
#'         choices = "PARCAT2"
#'       ),
#'       ae_var = choices_selected(
#'         selected = "AEDECOD",
#'         choices = c("AEDECOD", "AESOC")
#'       ),
#'       ae_line_col_var = choices_selected(
#'         selected = "AESER",
#'         choices = c("AESER", "AEREL")
#'       ),
#'       ae_line_col_opt = c("Y" = "red", "N" = "blue"),
#'       rs_var = choices_selected(
#'         selected = "PARAMCD",
#'         choices = "PARAMCD"
#'       ),
#'       cm_var = choices_selected(
#'         selected = "CMDECOD",
#'         choices = c("CMDECOD", "CMCAT")
#'       ),
#'       lb_var = choices_selected(
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
                                 post_output = NULL) {
  args <- as.list(environment())
  checkmate::assert_string(label)
  checkmate::assert_string(sl_dataname)
  checkmate::assert_string(ex_dataname, na.ok = TRUE)
  checkmate::assert_string(ae_dataname, na.ok = TRUE)
  checkmate::assert_string(rs_dataname, na.ok = TRUE)
  checkmate::assert_string(cm_dataname, na.ok = TRUE)
  checkmate::assert_string(lb_dataname, na.ok = TRUE)
  checkmate::assert_character(c(sl_dataname, ex_dataname, rs_dataname, cm_dataname, lb_dataname),
    any.missing = TRUE, all.missing = FALSE
  )
  checkmate::assert_class(sl_start_date, classes = "choices_selected")
  checkmate::assert_class(ex_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(ae_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(ae_line_col_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(rs_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(cm_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(lb_var, classes = "choices_selected", null.ok = TRUE)
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

  module(
    label = label,
    ui = ui_g_patient_profile,
    ui_args = args,
    server = srv_g_patient_profile,
    server_args = list(
      sl_dataname = sl_dataname,
      ex_dataname = ex_dataname,
      ae_dataname = ae_dataname,
      rs_dataname = rs_dataname,
      cm_dataname = cm_dataname,
      lb_dataname = lb_dataname,
      ae_line_col_opt = ae_line_col_opt,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    datanames = "all"
  )
}

ui_g_patient_profile <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)
  checkboxes <- c(a$ex_dataname, a$ae_dataname, a$rs_dataname, a$lb_dataname, a$cm_dataname)

  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("patientprofileplot"))
      ),
      encoding = div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        tags$label("Encodings", class = "text-primary"),
        selectizeInput(
          ns("patient_id"),
          "Patient ID",
          choices = a$patient_id$choices,
          selected = a$patient_id$selected
        ),
        div(
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
        teal.widgets::optionalSelectInput(
          ns("sl_start_date"),
          "Start date variable",
          choices = a$sl_start_date$choices,
          selected = a$sl_start_date$selected,
          multiple = FALSE,
          label_help = helpText(
            "from ", tags$code("ADSL")
          )
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", a$ex_dataname),
          ns = ns,
          selectInput(
            ns("ex_var"),
            "Exposure variable",
            choices = a$ex_var$choices,
            selected = a$ex_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", a$ae_dataname),
          ns = ns,
          teal.widgets::optionalSelectInput(
            ns("ae_var"),
            "Adverse Event variable",
            choices = a$ae_var$choices,
            selected = a$ae_var$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("ae_line_var"),
            "Adverse Event line color variable",
            choices = a$ae_line_col_var$choices,
            selected = a$ae_line_col_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", a$rs_dataname),
          ns = ns,
          teal.widgets::optionalSelectInput(
            ns("rs_var"),
            "Tumor response variable",
            choices = a$rs_var$choices,
            selected = a$rs_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", a$cm_dataname),
          ns = ns,
          teal.widgets::optionalSelectInput(
            ns("cm_var"),
            "Concomitant medicine variable",
            choices = a$cm_var$choices,
            selected = a$cm_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = sprintf("input['select_ADaM'].includes('%s')", a$lb_dataname),
          ns = ns,
          teal.widgets::optionalSelectInput(
            ns("lb_var"),
            "Lab variable",
            choices = a$lb_var$choices,
            selected = a$lb_var$selected,
            multiple = FALSE
          ),
          selectInput(
            ns("lb_var_show"),
            "Lab values",
            choices = a$lb_var$choices,
            selected = a$lb_var$selected,
            multiple = TRUE
          )
        ),
        textInput(
          ns("x_limit"),
          label = div(
            "Study Days Range",
            tags$br(),
            helpText("Enter TWO numeric values of study days range, separated by comma (eg. -28, 750)")
          ),
          value = a$x_limit
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      ),
      pre_output = a$pre_output,
      post_output = a$post_output
    )
  )
}

srv_g_patient_profile <- function(id,
                                  data,
                                  filter_panel_api,
                                  reporter,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelApi")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  if (!is.na(ex_dataname)) checkmate::assert_names(ex_dataname, subset.of = names(data))
  if (!is.na(ae_dataname)) checkmate::assert_names(ae_dataname, subset.of = names(data))
  if (!is.na(rs_dataname)) checkmate::assert_names(rs_dataname, subset.of = names(data))
  if (!is.na(lb_dataname)) checkmate::assert_names(lb_dataname, subset.of = names(data))
  if (!is.na(cm_dataname)) checkmate::assert_names(cm_dataname, subset.of = names(data))
  checkboxes <- c(ex_dataname, ae_dataname, rs_dataname, lb_dataname, cm_dataname)
  moduleServer(id, function(input, output, session) {
    select_plot <- reactive(
      vapply(checkboxes, function(x) x %in% input$select_ADaM, logical(1L))
    )

    if (!is.na(lb_dataname)) {
      observeEvent(input$lb_var, ignoreNULL = TRUE, {
        ADLB <- data()[[lb_dataname]] # nolint
        choices <- unique(ADLB[[input$lb_var]])
        choices_selected <- if (length(choices) > 5) choices[1:5] else choices

        updateSelectInput(
          session,
          "lb_var_show",
          selected = choices_selected,
          choices = choices
        )
      })
    }

    iv <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("select_ADaM", shinyvalidate::sv_required(
        message = "At least one ADaM data set is required"
      ))
      iv$add_rule("sl_start_date", shinyvalidate::sv_required(
        message = "Date variable is required"
      ))
      if (isTRUE(select_plot()[ex_dataname])) {
        iv$add_rule("ex_var", shinyvalidate::sv_required(
          message = "Exposure variable is required"
        ))
      }
      if (isTRUE(select_plot()[ae_dataname])) {
        iv$add_rule("ae_var", shinyvalidate::sv_required(
          message = "Adverse Event variable is required"
        ))
        iv$add_rule("ae_line_var", shinyvalidate::sv_optional())
        iv$add_rule("ae_line_var", ~ if (length(levels(data()[[ae_dataname]][[.]])) > length(ae_line_col_opt)) {
          "Not enough colors provided for Adverse Event line color, unselect"
        })
      }
      if (isTRUE(select_plot()[rs_dataname])) {
        iv$add_rule("rs_var", shinyvalidate::sv_required(
          message = "Tumor response variable is required"
        ))
      }
      if (isTRUE(select_plot()[cm_dataname])) {
        iv$add_rule("cm_var", shinyvalidate::sv_required(
          message = "Concomitant medicine variable is required"
        ))
      }
      if (isTRUE(select_plot()[lb_dataname])) {
        iv$add_rule("lb_var", shinyvalidate::sv_required(
          message = "Lab variable is required"
        ))
        iv$add_rule("lb_var_show", shinyvalidate::sv_required(
          message = "At least one Lab value is required"
        ))
        rule_diff <- function(value, other) {
          if (isTRUE(any(value == other))) {
            "Lab variable and Lab value must be different"
          }
        }
        iv$add_rule("lb_var", rule_diff, other = input$lb_var_show)
        iv$add_rule("lb_var_show", rule_diff, other = input$lb_var)
      }
      iv$add_rule("x_limit", shinyvalidate::sv_required(
        message = "Study Days Range is required"
      ))
      iv$add_rule("x_limit", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Study Days Range is invalid"
      })
      iv$add_rule("x_limit", ~ if (length(suppressWarnings(as_numeric_from_comma_sep_str(.))) != 2L) {
        "Study Days Range must be two values"
      })
      iv$add_rule("x_limit", ~ if (!identical(order(suppressWarnings(as_numeric_from_comma_sep_str(.))), 1:2)) {
        "Study Days Range mut be: first lower, then upper limit"
      })
      iv$enable()
      iv
    })

    # render plot
    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        teal::validate_inputs(iv())

        # get inputs ---
        patient_id <- input$patient_id # nolint
        sl_start_date <- input$sl_start_date # nolint
        ae_var <- input$ae_var
        ae_line_col_var <- input$ae_line_var
        rs_var <- input$rs_var
        cm_var <- input$cm_var
        ex_var <- input$ex_var
        lb_var <- input$lb_var
        x_limit <- input$x_limit
        lb_var_show <- input$lb_var_show

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
        ADSL <- data()[[sl_dataname]] # nolint

        ADEX <- NULL # nolint
        if (isTRUE(select_plot()[ex_dataname])) {
          ADEX <- data()[[ex_dataname]] # nolint
          teal::validate_has_variable(ADEX, adex_vars)
        }
        ADAE <- NULL # nolint
        if (isTRUE(select_plot()[ae_dataname])) {
          ADAE <- data()[[ae_dataname]] # nolint
          teal::validate_has_variable(ADAE, adae_vars)
        }
        ADRS <- NULL # nolint
        if (isTRUE(select_plot()[rs_dataname])) {
          ADRS <- data()[[rs_dataname]] # nolint
          teal::validate_has_variable(ADRS, adrs_vars)
        }
        ADCM <- NULL # nolint
        if (isTRUE(select_plot()[cm_dataname])) {
          ADCM <- data()[[cm_dataname]] # nolint
          teal::validate_has_variable(ADCM, adcm_vars)
        }
        ADLB <- NULL # nolint
        if (isTRUE(select_plot()[lb_dataname])) {
          ADLB <- data()[[lb_dataname]] # nolint
          teal::validate_has_variable(ADLB, adlb_vars)
        }

        empty_rs <- FALSE
        empty_ae <- FALSE
        empty_cm <- FALSE
        empty_ex <- FALSE
        empty_lb <- FALSE

        q1 <- teal.code::eval_code(
          data(),
          code = substitute(
            expr = {
              ADSL <- ADSL %>% # nolint
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
        validate(
          need(
            nrow(q1[["ADSL"]]) >= 1,
            paste(
              "Subject",
              patient_id,
              "not found in the dataset. Perhaps they have been filtered out by the filter panel?"
            )
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
                  ADAE <- ADAE[, adae_vars] # nolint

                  ADAE <- ADSL %>% # nolint
                    left_join(ADAE, by = c("STUDYID", "USUBJID")) %>% # nolint
                    as.data.frame() %>%
                    filter(!is.na(ASTDT), !is.na(AENDT)) %>%
                    mutate(
                      ASTDY = as.numeric(difftime(ASTDT, as.Date(sl_start_date), units = "days")) +
                        (ASTDT >= as.Date(sl_start_date)),
                      AENDY = as.numeric(difftime(AENDT, as.Date(sl_start_date), units = "days")) +
                        (AENDT >= as.Date(sl_start_date))
                    ) %>%
                    select(c(adae_vars, ASTDY, AENDY))
                  formatters::var_labels(ADAE)[ae_line_col_var] <- # nolint
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
            ADAE <- qq[[ae_dataname]] # nolint
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
                  ADRS <- ADRS[, adrs_vars] # nolint
                  ADRS <- ADSL %>% # nolint
                    left_join(ADRS, by = c("STUDYID", "USUBJID")) %>% # nolint
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
            ADRS <- qq[[rs_dataname]] # nolint
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
                  ADCM <- ADCM[, adcm_vars] # nolint
                  ADCM <- ADSL %>% # nolint
                    left_join(ADCM, by = c("STUDYID", "USUBJID")) %>% # nolint
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
                    ADCM <- ADCM[which(ADCM$AENDY >= -28 | is.na(ADCM$AENDY) == TRUE # nolint
                    & is.na(ADCM$ASTDY) == FALSE), ] # nolint
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

            ADCM <- qq[[cm_dataname]] # nolint
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
                  ADEX <- ADEX[, adex_vars] # nolint
                  ADEX <- ADSL %>% # nolint
                    left_join(ADEX, by = c("STUDYID", "USUBJID")) %>% # nolint
                    as.data.frame() %>%
                    filter(PARCAT1 == "INDIVIDUAL" & PARAMCD == "DOSE" & !is.na(AVAL) & !is.na(ASTDT)) %>%
                    select(USUBJID, ASTDT, PARCAT2, AVAL, AVALU, PARAMCD, sl_start_date)

                  ADEX <- split(ADEX, ADEX$USUBJID) %>% # nolint
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
            ADEX <- qq[[ex_dataname]] # nolint
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
                  ADLB <- ADLB[, adlb_vars] # nolint
                  ADLB <- ADSL %>% # nolint
                    left_join(ADLB, by = c("STUDYID", "USUBJID")) %>%
                    as.data.frame() %>%
                    mutate(
                      ANRIND = factor(ANRIND, levels = c("HIGH", "LOW", "NORMAL"))
                    ) %>%
                    filter(!is.na(LBSTRESN) & !is.na(ANRIND) & .data[[lb_var]] %in% lb_var_show) %>%
                    as.data.frame() %>%
                    select(
                      USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADT, AVISITN, sl_start_date, LBTESTCD, ANRIND, lb_var
                    ) %>% # nolint
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

            ADLB <- qq[[lb_dataname]] # nolint
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

        validate(need(
          any(!empty_data_check & select_plot()),
          "The subject does not have information in any selected domain."
        ))

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

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      title = paste("R code for", label),
      verbatim_content = reactive(teal.code::get_code(output_q()))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Patient Profile",
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
