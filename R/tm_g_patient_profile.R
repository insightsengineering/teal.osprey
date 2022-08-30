#' Patient Profile plot teal module
#'
#' Display patient profile plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param patient_id (\code{choices_seleced}) unique subject ID variable
#' @param sl_dataname (\code{character}) subject level dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}}
#' @param ex_dataname (\code{character}) exposures dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no exposure data is available
#' @param ae_dataname (\code{character}) adverse events dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no adverse events data is available
#' @param rs_dataname (\code{character}) response dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no response data is available
#' @param cm_dataname (\code{character}) concomitant medications dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no concomitant medications data is available
#' @param lb_dataname (\code{character}) labs dataset name,
#' needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}} \cr
#' specify to \code{NA} if no labs data is available
#' @param show_ex_plot boolean value of whether exposures plot is shown,
#' default is \code{TRUE}
#' @param show_ae_plot boolean value of whether adverse events plot is shown,
#' default is \code{TRUE}
#' @param show_rs_plot boolean value of whether response plot is shown,
#' default is \code{TRUE}
#' @param show_cm_plot boolean value of whether concomitant medications
#' plot is shown, default is \code{TRUE}
#' @param show_lb_plot boolean value of whether labs plot is shown,
#' default is \code{TRUE}
#' @param sl_start_date (\code{choices_selected}) study start date variable, usually set to treatment
#' start date or randomization date
#' @param ex_var (\code{choices_selected}) exposure variable to plot as each line \cr
#' leave unspecified or set to \code{NULL} if exposure data is not available
#' @param ae_var (\code{choices_selected}) adverse event variable to plot as each line \cr
#' leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param ae_line_col_var (\code{choices_selected}) variable for coloring AE lines \cr
#' leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param ae_line_col_opt aesthetic values to map color values (named vector to map color values to each name).
#'      If not \code{NULL}, please make sure this contains all possible values for \code{ae_line_col_var} values. \cr
#' leave unspecified or set to \code{NULL} if adverse events data is not available
#' @param rs_var (\code{choices_selected}) response variable to plot as each line \cr
#' leave unspecified or set to \code{NULL} if response data is not available
#' @param cm_var (\code{choices_selected}) concomitant medication variable
#' to plot as each line \cr
#' leave unspecified or set to \code{NULL} if concomitant medications data is not available
#' @param lb_var (\code{choices_selected}) lab variable to plot as each line \cr
#' leave unspecified or set to \code{NULL} if labs data is not available
#' @param x_limit a single \code{character} string with two numbers
#' separated by a comma indicating the x-axis limit,
#' default is \code{"-28, 365"}
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
#' library(scda)
#' library(nestcolor)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae %>%
#'   mutate(
#'     ASTDT = as.Date(ASTDTM),
#'     AENDT = as.Date(AENDTM)
#'   )
#' ADCM <- synthetic_cdisc_data("latest")$adcm %>%
#'   mutate(
#'     ASTDT = as.Date(ASTDTM),
#'     AENDT = as.Date(AENDTM)
#'   )
#'
#' # The step below is to pre-process ADCM to legacy standard
#' ADCM <- ADCM %>%
#'   select(-starts_with("ATC")) %>%
#'   unique()
#'
#' ADRS <- synthetic_cdisc_data("latest")$adrs %>%
#'   mutate(ADT = as.Date(ADTM))
#' ADEX <- synthetic_cdisc_data("latest")$adex %>%
#'   mutate(
#'     ASTDT = as.Date(ASTDTM),
#'     AENDT = as.Date(AENDTM)
#'   )
#' ADLB <- synthetic_cdisc_data("latest")$adlb %>%
#'   mutate(
#'     ADT = as.Date(ADTM),
#'     LBSTRESN = as.numeric(LBSTRESC)
#'   )
#'
#' x <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#'     ),
#'     cdisc_dataset("ADRS", ADRS,
#'       code = "ADRS <- synthetic_cdisc_data(\"latest\")$adrs %>%
#'               mutate(ADT = as.Date(ADTM))"
#'     ),
#'     cdisc_dataset("ADAE", ADAE,
#'       code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))"
#'     ),
#'     cdisc_dataset("ADCM", ADCM,
#'       code = "ADCM <- synthetic_cdisc_data(\"latest\")$adcm %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))
#'               ADCM <- ADCM %>% select(-starts_with(\"ATC\")) %>% unique()",
#'       keys = c("STUDYID", "USUBJID", "ASTDTM", "CMSEQ", "CMDECOD")
#'     ),
#'     cdisc_dataset("ADLB", ADLB,
#'       code = "ADLB <- synthetic_cdisc_data(\"latest\")$adlb %>%
#'               mutate(ADT = as.Date(ADTM),
#'                      LBSTRESN = as.numeric(LBSTRESC))"
#'     ),
#'     cdisc_dataset("ADEX", ADEX,
#'       code = "ADEX <- synthetic_cdisc_data(\"latest\")$adex %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))"
#'     ),
#'     check = TRUE
#'   ),
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
#'       show_ex_plot = TRUE,
#'       show_ae_plot = TRUE,
#'       show_rs_plot = TRUE,
#'       show_cm_plot = FALSE,
#'       show_lb_plot = TRUE,
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
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
#'
tm_g_patient_profile <- function(label = "Patient Profile Plot",
                                 patient_id,
                                 sl_dataname,
                                 ex_dataname,
                                 ae_dataname,
                                 rs_dataname,
                                 cm_dataname,
                                 lb_dataname,
                                 show_ex_plot = TRUE,
                                 show_ae_plot = TRUE,
                                 show_rs_plot = TRUE,
                                 show_cm_plot = TRUE,
                                 show_lb_plot = TRUE,
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
    filters = "all"
  )
}

ui_g_patient_profile <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)


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
        helpText("Select", tags$code("ADaM"), "Domains"),
        div(
          class = "pretty-left-border",
          uiOutput(ns("select_ae_output")),
          uiOutput(ns("select_ex_output")),
          uiOutput(ns("select_rs_output")),
          uiOutput(ns("select_cm_output")),
          uiOutput(ns("select_lb_output"))
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
          paste0("input['", ns("select_ex"), "']"),
          selectInput(
            ns("ex_var"),
            "Exposure variable",
            choices = a$ex_var$choices,
            selected = a$ex_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          paste0("input['", ns("select_ae"), "']"),
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
          paste0("input['", ns("select_rs"), "']"),
          teal.widgets::optionalSelectInput(
            ns("rs_var"),
            "Tumor response variable",
            choices = a$rs_var$choices,
            selected = a$rs_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          paste0("input['", ns("select_cm"), "']"),
          teal.widgets::optionalSelectInput(
            ns("cm_var"),
            "Concomitant medicine variable",
            choices = a$cm_var$choices,
            selected = a$cm_var$selected,
            multiple = FALSE
          )
        ),
        conditionalPanel(
          paste0("input['", ns("select_lb"), "']"),
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
      forms = get_rcode_ui(ns("rcode")),
      pre_output = a$pre_output,
      post_output = a$post_output
    )
  )
}

srv_g_patient_profile <- function(id,
                                  datasets,
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

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("sl_start_date", shinyvalidate::sv_required(message = "Please select a start date variable."))
    iv$add_rule("lb_var_show", shinyvalidate::sv_required(message = "Please select `Lab values`."))
    iv$enable()

    # initialize chunks
    teal.code::init_chunks()

    # only show the check box when domain data is available
    observeEvent(ae_dataname, {
      if (!is.na(ae_dataname)) {
        output$select_ae_output <- renderUI({
          checkboxInput(
            session$ns("select_ae"),
            "ADAE",
            value = !is.na(ae_dataname)
          )
        })
      }
    })

    observeEvent(ex_dataname, {
      if (!is.na(ex_dataname)) {
        output$select_ex_output <- renderUI({
          checkboxInput(
            session$ns("select_ex"),
            "ADEX",
            value = !is.na(ex_dataname)
          )
        })
      }
    })

    observeEvent(rs_dataname, {
      if (!is.na(rs_dataname)) {
        output$select_rs_output <- renderUI({
          checkboxInput(
            session$ns("select_rs"),
            "ADRS",
            value = !is.na(rs_dataname)
          )
        })
      }
    })

    observeEvent(cm_dataname, {
      if (!is.na(cm_dataname)) {
        output$select_cm_output <- renderUI({
          checkboxInput(
            session$ns("select_cm"),
            "ADCM",
            value = !is.na(cm_dataname)
          )
        })
      }
    })

    observeEvent(lb_dataname, {
      if (!is.na(lb_dataname)) {
        output$select_lb_output <- renderUI({
          checkboxInput(
            session$ns("select_lb"),
            "ADLB",
            value = !is.na(lb_dataname)
          )
        })
      }
    })

    observeEvent(input$select_lb, {
      req(input$select_lb == TRUE && !is.null(input$lb_var))
      ADLB <- datasets$get_data(lb_dataname, filtered = TRUE) # nolint
      choices <- unique(ADLB[[input$lb_var]])
      choices_selected <- if (length(choices) > 5) choices[1:5] else choices

      updateSelectInput(
        session,
        "lb_var_show",
        selected = choices_selected,
        choices = choices
      )
    })

    # render plot
    plot_r <- reactive({
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

      validate(
        need(ae_line_col_var, "Please select an adverse event line color.")
      )

      validate(need(iv$is_valid(), "Misspecification error: please observe red flags in the interface."))

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
      ADSL <- datasets$get_data(sl_dataname, filtered = TRUE) # nolint

      if (!is.null(input$select_ex)) {
        if (input$select_ex == FALSE | is.na(ex_dataname)) {
          ADEX <- NULL # nolint
        } else {
          ADEX <- datasets$get_data(ex_dataname, filtered = TRUE) # nolint
          validate_has_variable(ADEX, adex_vars)
        }
      } else {
        ADEX <- NULL # nolint
      }

      if (!is.null(input$select_ae)) {
        if (input$select_ae == FALSE | is.na(ae_dataname)) {
        # nolint start
          ADAE <- NULL #
        } else {
          ADAE <- datasets$get_data(ae_dataname, filtered = TRUE)
          formatters::var_labels(ADAE) <- formatters::var_labels(
            datasets$get_data(ae_dataname, filtered = FALSE),
            fill = FALSE
          )
          validate_has_variable(ADAE, adae_vars)

        }
      } else {
        ADAE <- NULL
        # nolint end
      }

      if (!is.null(input$select_rs)) {
        if (input$select_rs == FALSE | is.na(rs_dataname)) {
          ADRS <- NULL # nolint
        } else {
          ADRS <- datasets$get_data(rs_dataname, filtered = TRUE) # nolint
          validate_has_variable(ADRS, adrs_vars)
        }
      } else {
        ADRS <- NULL # nolint
      }

      if (!is.null(input$select_cm)) {
        if (input$select_cm == FALSE | is.na(cm_dataname)) {
          ADCMD <- NULL # nolint
        } else {
          ADCM <- datasets$get_data(cm_dataname, filtered = TRUE) # nolint
          validate_has_variable(ADCM, adcm_vars)
        }
      } else {
        ADCM <- NULL # nolint
      }

      if (!is.null(input$select_lb)) {
        if (input$select_lb == FALSE | is.na(lb_dataname)) {
          ADLB <- NULL # nolint
        } else {
          ADLB <- datasets$get_data(lb_dataname, filtered = TRUE) # nolint
          validate_has_variable(ADLB, adlb_vars)
        }
      } else {
        ADLB <- NULL # nolint
      }

      # check color assignment
      if (!is.null(ae_line_col_opt)) {
        validate(need(
          is.null(ae_line_col_var) || length(levels(ADAE[[ae_line_col_var]])) <= length(ae_line_col_opt),
          paste(
            "Please check ae_line_col_opt contains all possible values for ae_line_col_var values.",
            "Or specify ae_line_col_opt as NULL.",
            sep = "\n"
          )
        ))
      }

      possible_plot <- c("ex", "ae", "rs", "cm", "lb")
      datanames <- c(
        ex_dataname,
        ae_dataname,
        rs_dataname,
        cm_dataname,
        lb_dataname
      )
      input_select <- purrr::map_lgl(datanames, is.na)
      select_plot <- purrr::map2_lgl(
        input_select, possible_plot,
        ~ if (!.x) {
          input[[paste("select", .y, sep = "_")]]
        } else {
          FALSE
        }
      )

      names(select_plot) <- possible_plot

      empty_rs <- FALSE
      empty_ae <- FALSE
      empty_cm <- FALSE
      empty_ex <- FALSE
      empty_lb <- FALSE

      # restart chunks & include current environment ---
      teal.code::chunks_reset(envir = environment())

      teal.code::chunks_push(
        id = "ADSL call",
        expression = bquote({
          # nolint start
          ADSL <- ADSL %>%
            group_by(.data$USUBJID)
          ADSL$max_date <- pmax(
            as.Date(ADSL$LSTALVDT),
            as.Date(ADSL$DTHDT),
            na.rm = TRUE
          )
          ADSL <- ADSL %>%
          # nolint end
            mutate(
              max_day = as.numeric(
                as.Date(.data$max_date) - as.Date(
                  eval(parse(text = .(sl_start_date), keep.source = FALSE))
                )
              )
              + (as.Date(.data$max_date) >= as.Date(eval(parse(text = .(sl_start_date)))))
            ) %>%
            filter(USUBJID == .(patient_id))
        })
      )

      teal.code::chunks_push_new_line()
      teal.code::chunks_safe_eval()

      # ADSL with single subject
      ADSL <- teal.code::chunks_get_var("ADSL") # nolint

      validate(
        need(
          nrow(ADSL) >= 1,
          paste(
            "Subject",
            patient_id,
            "not found in the dataset. Have they been filtered out by filtering in the filter panel?"
          )
        )
      )

      # name for ae_line_col
      if (!is.null(ae_line_col_var) && is.data.frame(ADAE)) {
        teal.code::chunks_push(
          id = "ae_line_col_name call",
          expression =
            bquote(ae_line_col_name <- formatters::var_labels(ADAE, fill = FALSE)[.(ae_line_col_var)])
        )
      } else {
        teal.code::chunks_push(id = "ae_line_col_name call", expression = quote(ae_line_col_name <- NULL))
      }

      if (select_plot["ae"]) {
        validate(
          need(!is.null(input$ae_var), "Please select an adverse event variable.")
        )
        if (ADSL$USUBJID %in% ADAE$USUBJID) {
          teal.code::chunks_push(
            id = "ADAE call",
            expression = bquote({
              # ADAE
              ADAE <- ADAE[, .(adae_vars)] # nolint

              ADAE <- ADSL %>% # nolint
                left_join(ADAE, by = c("STUDYID", "USUBJID")) %>% # nolint
                as.data.frame() %>%
                filter(!is.na(ASTDT)) %>%
                mutate(ASTDY = as.numeric(
                  difftime(
                    ASTDT,
                    as.Date(substr(
                      as.character(eval(parse(
                        text = .(sl_start_date)
                      ))), 1, 10
                    )),
                    units = "days"
                  )
                )
                + (ASTDT >= as.Date(substr(
                    as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                  )))) %>%
                filter(!is.na(AENDT)) %>%
                mutate(AENDY = as.numeric(
                  difftime(
                    AENDT,
                    as.Date(substr(
                      as.character(eval(parse(
                        text = .(sl_start_date)
                      ))), 1, 10
                    )),
                    units = "days"
                  )
                )
                + (AENDT >= as.Date(substr( # nolint
                    as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                  )))) %>%
                select(c(.(adae_vars), ASTDY, AENDY))
              formatters::var_labels(ADAE)[.(ae_line_col_var)] <- # nolint
                formatters::var_labels(ADAE, fill = FALSE)[.(ae_line_col_var)]
            })
          )
          teal.code::chunks_safe_eval()

          teal.code::chunks_push(
            id = "ae call",
            expression = call(
              "<-",
              as.name("ae"),
              call(
                "list",
                data = bquote(data.frame(ADAE)),
                var = bquote(as.vector(ADAE[, .(ae_var)])),
                line_col = if (!is.null(ae_line_col_var)) {
                  bquote(as.vector(ADAE[, .(ae_line_col_var)]))
                } else {
                  NULL
                },
                line_col_legend = if (!is.null(ae_line_col_var)) {
                  quote(ae_line_col_name)
                } else {
                  NULL
                },
                line_col_opt = if (is.null(ae_line_col_var)) {
                  NULL
                } else {
                  bquote(.(ae_line_col_opt))
                }
              )
            )
          )
          ADAE <- teal.code::chunks_get_var("ADAE") # nolint
          if (is.null(ADAE) | nrow(ADAE) == 0) {
            empty_ae <- TRUE
          }
        } else {
          empty_ae <- TRUE
          teal.code::chunks_push(id = "ae call", expression = bquote(ae <- NULL))
        }
      } else {
        teal.code::chunks_push(id = "ae call", expression = bquote(ae <- NULL))
      }

      teal.code::chunks_push_new_line()
      teal.code::chunks_safe_eval()

      if (select_plot["rs"]) {
        validate(
          need(!is.null(rs_var), "Please select a tumor response variable.")
        )
        if (ADSL$USUBJID %in% ADRS$USUBJID) {
          teal.code::chunks_push(
            id = "ADRS and rs call",
            expression = bquote({
              ADRS <- ADRS[, .(adrs_vars)] # nolint
              ADRS <- ADSL %>% # nolint
                left_join(ADRS, by = c("STUDYID", "USUBJID")) %>% # nolint
                as.data.frame() %>%
                mutate(
                  ADY = as.numeric(difftime(
                    ADT,
                    as.Date(substr(
                      as.character(eval(parse(
                        text = .(sl_start_date),
                        keep.source = FALSE
                      ))), 1, 10
                    )),
                    units = "days"
                  ))
                  + (ADT >= as.Date(substr(
                      as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                    )))
                ) %>%
                select(USUBJID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADT) %>%
                filter(is.na(ADY) == FALSE)
              rs <- list(data = data.frame(ADRS), var = as.vector(ADRS[, .(rs_var)]))
            })
          )
          teal.code::chunks_safe_eval()
          ADRS <- teal.code::chunks_get_var("ADRS") # nolint
          if (is.null(ADRS) || nrow(ADRS) == 0) {
            empty_rs <- TRUE
          }
        } else {
          empty_rs <- TRUE
          teal.code::chunks_push(id = "rs call", expression = bquote(rs <- NULL))
        }
      } else {
        teal.code::chunks_push(id = "rs call", expression = bquote(rs <- NULL))
      }

      teal.code::chunks_push_new_line()

      if (select_plot["cm"]) {
        validate(
          need(!is.null(cm_var), "Please select a concomitant medication variable.")
        )
        if (ADSL$USUBJID %in% ADCM$USUBJID) {
          teal.code::chunks_push(
            id = "ADCM and cm call",
            expression = bquote({
              # ADCM
              ADCM <- ADCM[, .(adcm_vars)] # nolint
              ADCM <- ADSL %>% # nolint
                left_join(ADCM, by = c("STUDYID", "USUBJID")) %>% # nolint
                as.data.frame() %>%
                filter(!is.na(ASTDT)) %>%
                mutate(ASTDY = as.numeric(difftime(
                  ASTDT,
                  as.Date(substr(as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10)),
                  units = "days"
                ))
                + (ASTDT >= as.Date(substr(
                    as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                  )))) %>%
                filter(!is.na(AENDT)) %>%
                mutate(AENDY = as.numeric(difftime(
                  AENDT,
                  as.Date(substr(as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10)),
                  units = "days"
                ))
                + (AENDT >= as.Date(substr(
                    as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                  )))) %>%
                select(USUBJID, ASTDT, AENDT, ASTDY, AENDY, !!quo(.(cm_var)))
              if (length(unique(ADCM$USUBJID)) > 0) {
                ADCM <- ADCM[which(ADCM$AENDY >= -28 | is.na(ADCM$AENDY) == TRUE # nolint
                & is.na(ADCM$ASTDY) == FALSE), ]
              }
              cm <- list(data = data.frame(ADCM), var = as.vector(ADCM[, .(cm_var)]))
            })
          )
          teal.code::chunks_safe_eval()
          ADCM <- teal.code::chunks_get_var("ADCM") # nolint
          if (is.null(ADCM) | nrow(ADCM) == 0) {
            empty_cm <- TRUE
          }
        } else {
          empty_cm <- TRUE
          teal.code::chunks_push(id = "cm call", expression = bquote(cm <- NULL))
        }
      } else {
        teal.code::chunks_push(id = "cm call", expression = bquote(cm <- NULL))
      }

      teal.code::chunks_push_new_line()

      if (select_plot["ex"]) {
        validate(
          need(!is.null(ex_var), "Please select an exposure variable.")
        )
        if (ADSL$USUBJID %in% ADEX$USUBJID) {
          teal.code::chunks_push(
            id = "ADEX and ex call",
            expression = bquote({
              # ADEX
              ADEX <- ADEX[, .(adex_vars)] # nolint
              ADEX <- ADSL %>% # nolint
                left_join(ADEX, by = c("STUDYID", "USUBJID")) %>% # nolint
                as.data.frame() %>%
                filter(PARCAT1 == "INDIVIDUAL" & PARAMCD == "DOSE" & !is.na(AVAL)) %>%
                filter(!is.na(ASTDT)) %>%
                select(
                  USUBJID, ASTDT, PARCAT2,
                  AVAL, AVALU, PARAMCD, !!quo(.(sl_start_date))
                )
              ADEX <- split(ADEX, ADEX$USUBJID) %>% # nolint
                lapply(function(pinfo) {
                  pinfo %>%
                    arrange(PARCAT2, PARAMCD, ASTDT) %>%
                    ungroup() %>%
                    mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
                    mutate(
                      Modification = case_when(
                        diff < 0 ~ "Decrease",
                        diff > 0 ~ "Increase",
                        diff == 0 ~ "None"
                      )
                    ) %>%
                    mutate(ASTDT_dur = as.numeric(
                      as.Date(substr(as.character(ASTDT), 1, 10)) -
                        as.Date(substr(as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10))
                    )
                    + (as.Date(substr(as.character(ASTDT), 1, 10)) >=
                        as.Date(substr(as.character(eval(parse(text = .(sl_start_date)))), 1, 10))))
                }) %>%
                Reduce(rbind, .) %>%
                as.data.frame() %>%
                select(-diff)
              ex <- list(data = data.frame(ADEX), var = as.vector(ADEX[, .(ex_var)]))
            })
          )
          teal.code::chunks_safe_eval()
          ADEX <- teal.code::chunks_get_var("ADEX") # nolint
          if (is.null(ADEX) | nrow(ADEX) == 0) {
            empty_ex <- TRUE
          }
        } else {
          empty_ex <- TRUE
          teal.code::chunks_push(id = "ex call", expression = bquote(ex <- NULL))
        }
      } else {
        teal.code::chunks_push(id = "ex call", expression = bquote(ex <- NULL))
      }

      teal.code::chunks_push_new_line()

      if (select_plot["lb"]) {
        validate(
          need(!is.null(lb_var), "Please select a lab variable.")
        )
        if (ADSL$USUBJID %in% ADLB$USUBJID) {
          req(lb_var_show != lb_var)
          teal.code::chunks_push(
            id = "ADLB and lb call",
            expression = bquote({
              ADLB <- ADLB[, .(adlb_vars)] # nolint
              ADLB <- ADSL %>% # nolint
                left_join(ADLB, by = c("STUDYID", "USUBJID")) %>%
                as.data.frame() %>%
                group_by(USUBJID) %>%
                mutate(ANRIND = factor(
                  .data$ANRIND,
                  levels = c("HIGH", "LOW", "NORMAL")
                )) %>%
                filter(
                  !is.na(.data$LBSTRESN) & !is.na(.data$ANRIND)
                ) %>%
                as.data.frame() %>%
                select(
                  USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADT, AVISITN, !!quo(.(sl_start_date)),
                  LBTESTCD, ANRIND, !!quo(.(lb_var))
                )

              ADLB <- ADLB %>% # nolint
                mutate(ADY = as.numeric(difftime(
                  .data$ADT,
                  as.Date(substr(as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10)),
                  units = "days"
                ))
                + (ADT >= as.Date(substr(
                    as.character(eval(parse(text = .(sl_start_date), keep.source = FALSE))), 1, 10
                  )))) %>%
                filter(.data[[.(lb_var)]] %in% .(lb_var_show))
              lb <- list(data = data.frame(ADLB), var = as.vector(ADLB[, .(lb_var)]))
            })
          )
          teal.code::chunks_safe_eval()
          ADLB <- teal.code::chunks_get_var("ADLB") # nolint
          if (is.null(ADLB) | nrow(ADLB) == 0) {
            empty_lb <- TRUE
          }
        } else {
          empty_lb <- TRUE
          teal.code::chunks_push(id = "lb call", expression = bquote(lb <- NULL))
        }
      } else {
        teal.code::chunks_push(id = "lb call", expression = bquote(lb <- NULL))
      }


      teal.code::chunks_push_new_line()

      # Check that at least 1 dataset is selected

      validate(
        need(any(select_plot), "Please select an ADaM dataset.")
      )

      # Check the subject has information in at least one selected domain
      empty_data_check <- c(empty_ex, empty_ae, empty_rs, empty_cm, empty_lb)

      validate(need(
        any(!empty_data_check & select_plot),
        "The subject does not have information in any selected domain."
      ))

      # Check the subject has information in all the selected domains
      if (any(empty_data_check & select_plot)) {
        showNotification(
          paste0(
            "This subject does not have information in the ",
            paste(c(possible_plot[(empty_data_check & select_plot)]), collapse = ", "),
            " domain."
          ),
          duration = 8,
          type = "warning"
        )
      }

      # Convert x_limit to numeric vector
      if (!is.null(x_limit) || x_limit != "") {
        teal.code::chunks_push(
          id = "x_limit call",
          expression = bquote(x_limit <- as.numeric(unlist(strsplit(.(x_limit), ","))))
        )
        teal.code::chunks_safe_eval()
        x_limit <- teal.code::chunks_get_var("x_limit")
      }

      validate(need(
        all(!is.na(x_limit)) & all(!is.infinite(x_limit)),
        "Not all values entered for study days range were numeric."
      ))
      validate(need(
        x_limit[1] < x_limit[2],
        "The lower limit for study days range should come first."
      ))

      teal.code::chunks_push_new_line()

      teal.code::chunks_push(
        id = "g_patient_profile call",
        expression = bquote({
          osprey::g_patient_profile(
            ex = ex,
            ae = ae,
            rs = rs,
            cm = cm,
            lb = lb,
            arrow_end_day = ADSL$max_day,
            xlim = x_limit,
            xlab = "Study Day",
            title = paste("Patient Profile: ", .(patient_id))
          )
        })
      )
      teal.code::chunks_safe_eval()
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "patientprofileplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = datasets$datanames()
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Patient Profile")
        card$append_text("Patient Profile", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
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
