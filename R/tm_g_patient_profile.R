#' Patient Profile plot teal module
#'
#' Display patient profile plot as a shiny module
#'
#' @param label module label in the teal app, please be noted that this module is developed based on
#' ADaM data structure and ADaM variables.
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
#' @param plot_height plot height, default is \code{c(1200, 400, 5000)}
#' @param pre_output default is \code{NULL}
#' @param post_output default is \code{NULL}
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Tina Cho (chot) \email{tina.cho@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @return plot object
#' @import dplyr
#' @importFrom rtables var_labels "var_labels<-"
#' @importFrom purrr map_lgl map2_lgl
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
#' # Example using random.cdisc.data dataset
#' library(random.cdisc.data)
#' library(teal.osprey)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE) %>%
#'   mutate(ASTDT = as.Date(ASTDTM),
#'          AENDT = as.Date(AENDTM))
#' ADCM <- radcm(cached = TRUE) %>%
#'   mutate(ASTDT = as.Date(ASTDTM),
#'          AENDT = as.Date(AENDTM))
#' ADRS <- radrs(cached = TRUE) %>%
#'   mutate(ADT = as.Date(ADTM))
#' ADEX <- radex(cached = TRUE) %>%
#'   mutate(ASTDT = as.Date(ASTDTM),
#'          AENDT = as.Date(AENDTM))
#' ADLB <- radlb(cached = TRUE) %>%
#'   mutate(ADT = as.Date(ADTM),
#'          LBSTRESN = as.numeric(LBSTRESC))
#'
#' x <- init(
#'   data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADAE", ADAE),
#'     cdisc_dataset("ADCM", ADCM),
#'     cdisc_dataset("ADLB", ADLB),
#'     cdisc_dataset("ADEX", ADEX),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADAE <- radae(cached = TRUE) %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))
#'             ADCM <- radcm(cached = TRUE) %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))
#'             ADRS <- radrs(cached = TRUE) %>%
#'               mutate(ADT = as.Date(ADTM))
#'             ADEX <- radex(cached = TRUE) %>%
#'               mutate(ASTDT = as.Date(ASTDTM),
#'                      AENDT = as.Date(AENDTM))
#'             ADLB <- radlb(cached = TRUE) %>%
#'               mutate(ADT = as.Date(ADTM),
#'                      LBSTRESN = as.numeric(LBSTRESC))'),
#'   modules = root_modules(
#'     tm_g_patient_profile(
#'       label = "Patient Profile Plot",
#'      patient_id = choices_selected(
#'        choices = unique(ADSL$USUBJID),
#'        selected = unique(ADSL$USUBJID)[1]
#'        ),
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
#'         choices = c("TRTSDTM", "RANDDT")),
#'       ex_var = choices_selected(
#'         selected = "PARCAT2",
#'         choices = "PARCAT2"),
#'       ae_var = choices_selected(
#'         selected = "AEDECOD",
#'         choices = c("AEDECOD", "AESOC")),
#'       ae_line_col_var = choices_selected(
#'         selected = "AESER",
#'         choices = c("AESER", "AEREL")),
#'       ae_line_col_opt = c("Y" = "red","N" = "blue"),
#'       rs_var = choices_selected(
#'         selected = "PARAMCD",
#'         choices = "PARAMCD"),
#'       cm_var = choices_selected(
#'         selected = "CMDECOD",
#'         choices = c("CMDECOD", "CMCAT")),
#'       lb_var = choices_selected(
#'         selected = "LBTESTCD",
#'         choices = c("LBTESTCD", "LBCAT")),
#'       x_limit = "-28, 750",
#'       plot_height = c(1200, 400, 5000)
#'     )
#'   )
#' )
#'
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
                                 plot_height =c(1200, 400, 5000),
                                 pre_output = NULL,
                                 post_output = NULL) {

  args <- as.list(environment())
  stopifnot(is_character_single(label))
  stopifnot(is_character_single(sl_dataname))
  stopifnot(is.na(ex_dataname) | is_character_single(ex_dataname))
  stopifnot(is.na(ae_dataname) | is_character_single(ae_dataname))
  stopifnot(is.na(rs_dataname) | is_character_single(rs_dataname))
  stopifnot(is.na(cm_dataname) | is_character_single(cm_dataname))
  stopifnot(is.na(lb_dataname) | is_character_single(lb_dataname))
  stopifnot(is.choices_selected(sl_start_date))
  stopifnot(is.null(ex_var) | is.choices_selected(ex_var))
  stopifnot(is.null(ae_var) | is.choices_selected(ae_var))
  stopifnot(is.null(ae_line_col_var) | is.choices_selected(ae_line_col_var))
  stopifnot(is.null(rs_var) | is.choices_selected(rs_var))
  stopifnot(is.null(cm_var) | is.choices_selected(cm_var))
  stopifnot(is.null(lb_var) | is.choices_selected(lb_var))
  stopifnot(is_character_single(x_limit))
  stopifnot(is_numeric_vector(plot_height))


  module(
    label = label,
    ui = ui_g_patient_profile,
    ui_args = args,
    server = srv_g_patient_profile,
    server_args = list(sl_dataname = sl_dataname,
                       ex_dataname = ex_dataname,
                       ae_dataname = ae_dataname,
                       rs_dataname = rs_dataname,
                       cm_dataname = cm_dataname,
                       lb_dataname = lb_dataname,
                       ae_line_col_opt = ae_line_col_opt,
                       label = label),
    filters = "all"
  )
}

ui_g_patient_profile <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      plot_height_output(
        id = ns("patientprofileplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      selectizeInput(
        ns("patient_id"),
        "Patient ID",
        choices = a$patient_id$choices,
        selected = a$patient_id$selected
        ),
      helpText("Select", tags$code("ADaM"), "Domains"),
      div(
      style = "border-left: 3px solid #e3e3e3;
               padding-left: 0.6em;
               border-radius: 5px;
               margin-left: -0.6m;",
      uiOutput(ns("select_ae_output")),
      uiOutput(ns("select_ex_output")),
      uiOutput(ns("select_rs_output")),
      uiOutput(ns("select_cm_output")),
      uiOutput(ns("select_lb_output"))
      ),
      optionalSelectInput(
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
          multiple = FALSE)),
      conditionalPanel(
        paste0("input['", ns("select_ae"), "']"),
        optionalSelectInput(
          ns("ae_var"),
          "Adverse Event variable",
          choices = a$ae_var$choices,
          selected = a$ae_var$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("ae_line_var"),
          "Adverse Event line color variable",
          choices = a$ae_line_col_var$choices,
          selected = a$ae_line_col_var$selected,
          multiple = FALSE)),
      conditionalPanel(
        paste0("input['", ns("select_rs"), "']"),
        optionalSelectInput(
          ns("rs_var"),
          "Tumor response variable",
          choices = a$rs_var$choices,
          selected = a$rs_var$selected,
          multiple = FALSE)),
      conditionalPanel(
        paste0("input['", ns("select_cm"), "']"),
        optionalSelectInput(
          ns("cm_var"),
          "Concomittant medicine variable",
          choices = a$cm_var$choices,
          selected = a$cm_var$selected,
          multiple = FALSE)),
      conditionalPanel(
        paste0("input['", ns("select_lb"), "']"),
        optionalSelectInput(
          ns("lb_var"),
          "Lab variable",
          choices = a$lb_var$choices,
          selected = a$lb_var$selected,
          multiple = FALSE),
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
          helpText("Enter TWO numeric values of study days range,
                   separated by comma (eg. -28, 750)")),
        value = a$x_limit
        ),
      plot_height_input(id = ns("patientprofileplot"), value = a$plot_height)
    ),
    forms = tags$div(actionButton(
      ns("show_rcode"),
      "Show R Code",
      width = "100%")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_patient_profile <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  sl_dataname,
                                  ex_dataname,
                                  ae_dataname,
                                  rs_dataname,
                                  lb_dataname,
                                  cm_dataname,
                                  label,
                                  ae_line_col_opt
) {

  callModule(
    plot_with_height,
    id = "patientprofileplot",
    plot_height = reactive(input$patientprofileplot),
    plot_id = session$ns("plot")
  )

  # initialize chunks
  init_chunks()

  #only show the check box when domain data is available
  observe({
    if (!is.na(ae_dataname)) {
      output$select_ae_output <- renderUI({
        checkboxInput(
          session$ns("select_ae"),
          "ADAE",
          value = !is.na(ae_dataname))
      })
    }

    if (!is.na(ex_dataname)) {
      output$select_ex_output <- renderUI({
        checkboxInput(
          session$ns("select_ex"),
          "ADEX",
          value = !is.na(ex_dataname))
      })
    }

    if (!is.na(rs_dataname)) {
      output$select_rs_output <- renderUI({
        checkboxInput(
          session$ns("select_rs"),
          "ADRS",
          value = !is.na(rs_dataname))
      })
    }

    if (!is.na(cm_dataname)) {
      output$select_cm_output <- renderUI({
        checkboxInput(
          session$ns("select_cm"),
          "ADCM",
          value = !is.na(cm_dataname))
      })
    }

    if (!is.na(lb_dataname)) {
      output$select_lb_output <- renderUI({
        checkboxInput(
          session$ns("select_lb"),
          "ADLB",
          value = !is.na(lb_dataname))
      })
    }

  })

  observe({
    req(input$select_lb == TRUE && !is.null(input$lb_var))
    ADLB_FILTERED <- datasets$get_data(lb_dataname, filtered = TRUE) #nolint
    choices <- unique(ADLB_FILTERED[[input$lb_var]])
    choices_selected <- if (length(choices) > 5) choices[1:5] else choices

    updateSelectInput(
      session,
      "lb_var_show",
      selected = choices_selected,
      choices = choices)
  })

  # render plot
  output$plot <- renderPlot({

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

    validate(
      need(!is.null(sl_start_date), "Please select a start date variable.")
    )

    adrs_vars <- unique(c(
      "USUBJID", "STUDYID", "PARAMCD",
      "PARAM", "AVALC", "AVAL", "ADY",
      "ADT", rs_var))
    adae_vars <- unique(c(
      "USUBJID", "STUDYID", "ASTDT",
      "AENDT", "AESOC", "AEDECOD",
      "AESER", "AETOXGR", "AEREL",
      "ASTDY", "AENDY",
      ae_var, ae_line_col_var))
    adcm_vars <- unique(c(
      "USUBJID", "STUDYID", "ASTDT",
      "AENDT", "ASTDT", "CMDECOD",
      "ASTDY", "AENDY", "CMCAT",
      cm_var))
    adex_vars <- unique(c(
      "USUBJID", "STUDYID", "ASTDT",
      "AENDT", "PARCAT2", "AVAL",
      "AVALU", "PARAMCD", "PARCAT1",
      "PARCAT2", ex_var))
    adlb_vars <- unique(c(
      "USUBJID", "STUDYID", "ANRIND", "LBSEQ",
      "PARAMCD", "BASETYPE", "ADT", "AVISITN",
      "LBSTRESN", "LBCAT", "LBTESTCD",
      lb_var))

    # get ADSL dataset ---
    ADSL_FILTERED <- datasets$get_data(sl_dataname, filtered = TRUE) # nolint

    if (!is.null(input$select_ex)){
      if (input$select_ex == FALSE | is.na(ex_dataname)) {
        ADEX_FILTERED <- NULL # nolint
      } else {
        ADEX_FILTERED <- datasets$get_data(ex_dataname, filtered = TRUE) # nolint
        validate_has_variable(ADEX_FILTERED, adex_vars)
      }
    } else {
      ADEX_FILTERED <- NULL # nolint
    }

    if (!is.null(input$select_ae)){
      if (input$select_ae == FALSE | is.na(ae_dataname)) {
        ADAE_FILTERED <- NULL # nolint
      } else {
        ADAE_FILTERED <- datasets$get_data(ae_dataname, filtered = TRUE) # nolint
        var_labels(ADAE_FILTERED) <- var_labels(
          datasets$get_data(ae_dataname, filtered = FALSE)
        )
        validate_has_variable(ADAE_FILTERED, adae_vars)
      }
    } else {
      ADAE_FILTERED <- NULL # nolint
    }


    if (!is.null(input$select_rs)) {
      if (input$select_rs == FALSE | is.na(rs_dataname)) {
        ADRS_FILTERED <- NULL # nolint
      } else {
        ADRS_FILTERED <- datasets$get_data(rs_dataname, filtered = TRUE) # nolint
        validate_has_variable(ADRS_FILTERED, adrs_vars)
      }
    } else {
      ADRS_FILTERED <- NULL # nolint
    }

    if (!is.null(input$select_cm)) {
      if (input$select_cm == FALSE | is.na(cm_dataname)) {
        ADCM_FILTERED <- NULL # nolint
      } else {
        ADCM_FILTERED <- datasets$get_data(cm_dataname, filtered = TRUE) # nolint
        validate_has_variable(ADCM_FILTERED, adcm_vars)
      }
    } else {
      ADCM_FILTERED <- NULL # nolint
    }

    if (!is.null(input$select_lb)) {
      if (input$select_lb == FALSE | is.na(lb_dataname)) {
        ADLB_FILTERED <- NULL # nolint
      } else {
        ADLB_FILTERED <- datasets$get_data(lb_dataname, filtered = TRUE) # nolint
        validate_has_variable(ADLB_FILTERED, adlb_vars)
      }
    } else {
      ADLB_FILTERED <- NULL # nolint
    }

    #check color assignment
    if (!is.null(ae_line_col_opt)) {
      validate(need(
        length(levels(ADAE_FILTERED[[input$ae_line_var]])) <= length(ae_line_col_opt),
        paste(
          "Please check ae_line_col_opt contains all possible values for ae_line_col_var values.",
          "Or specify ae_line_col_opt as NULL.",
          sep = "\n")
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
    input_select <- map_lgl(datanames, is.na)
    select_plot <- map2_lgl(
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
    chunks_reset(envir = environment())

    chunks_push(bquote({

      ADSL <- ADSL_FILTERED %>% # nolint
        group_by(.data$USUBJID)
      ADSL$max_date <- pmax(
        as.Date(ADSL$LSTALVDT, "%d%b%Y"),
        as.Date(ADSL$DTHDT, "%d%b%Y"), na.rm = TRUE)
      ADSL <- ADSL %>% # nolint
        mutate(
          max_day = as.numeric(
            as.Date(.data$max_date) - as.Date(
              eval(parse(text = .(sl_start_date))), "%d%b%Y")) + 1) %>%
        filter(USUBJID == .(patient_id))
    }))

    chunks_push_new_line()

    # check
    chunks_eval()

    #ADSL with single subject
    ADSL <- chunks_get_var("ADSL") # nolint

    # name for ae_line_col
    if (!is.null(ae_line_col_var) & is.data.frame(ADAE_FILTERED)) {
      chunks_push(bquote(ae_line_col_name <- rtables::var_labels(ADAE_FILTERED)[.(ae_line_col_var)]))
    } else {
      chunks_push(quote(ae_line_col_name <- NULL))
    }

    if (select_plot["ae"]) {
      validate(
        need(!is.null(input$ae_var), "Please select an adverse event variable.")
      )
      if (ADSL$USUBJID %in% ADAE_FILTERED$USUBJID){
        chunks_push(bquote({
          # ADAE
          ADAE <- ADAE_FILTERED[, .(adae_vars)] # nolint

          ADAE <- ADSL %>% # nolint
            left_join(ADAE, by = c("STUDYID", "USUBJID")) %>% # nolint
            as.data.frame() %>%
            filter(!is.na(ASTDT)) %>%
            mutate(ASTDY = as.numeric(
              difftime(ASTDT,
                       as.Date(substr(
                         as.character(eval(parse(
                           text = .(sl_start_date)))), 1, 10)), units = "days")) + 1) %>%
            filter(!is.na(AENDT)) %>%
            mutate(AENDY = as.numeric(
              difftime(AENDT,
                       as.Date(substr(
                         as.character(eval(parse(
                           text = .(sl_start_date)))), 1, 10)), units = "days")) + 1) %>%
            select(c(.(adae_vars), ASTDY, AENDY))
          var_labels(ADAE)[.(ae_line_col_var)] <- var_labels(ADAE_FILTERED)[.(ae_line_col_var)]
        })
        )
        chunks_eval()


        chunks_push(
          call(
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
        ADAE <- chunks_get_var("ADAE") # nolint
        if (is.null(ADAE) | nrow(ADAE) == 0) {
          empty_ae <- TRUE
        }
      } else {
        empty_ae <- TRUE
        chunks_push(bquote(ae <- NULL))
      }
    } else {
      chunks_push(bquote(ae <- NULL))
    }

    chunks_push_new_line()
    chunks_eval()

    if (select_plot["rs"]) {
      validate(
        need(!is.null(rs_var), "Please select a tumor response variable.")
      )
      if (ADSL$USUBJID %in% ADRS_FILTERED$USUBJID){
        chunks_push(bquote({
          ADRS <- ADRS_FILTERED[, .(adrs_vars)] # nolint
          ADRS <- ADSL %>% # nolint
            left_join(ADRS, by = c("STUDYID", "USUBJID")) %>% # nolint
            as.data.frame() %>%
            mutate(ADY = as.numeric(difftime(
              ADT,
              as.Date(substr(
                as.character(eval(parse(
                  text = .(sl_start_date)))), 1, 10)), units = "days")) + 1) %>%
            select(USUBJID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADT) %>%
            filter(is.na(ADY) == FALSE)
          rs <- list(data = data.frame(ADRS), var = as.vector(ADRS[, .(rs_var)]))
        })
        )
        chunks_eval()
        ADRS <- chunks_get_var("ADRS") # nolint
        if (is.null(ADRS) | nrow(ADRS) == 0) {
          empty_rs <- TRUE
        }
      } else {
        empty_rs <- TRUE
        chunks_push(bquote(rs <- NULL))
      }
    } else {
      chunks_push(bquote(rs <- NULL))
    }

    chunks_push_new_line()

    # check
    chunks_eval()
    if (select_plot["cm"]) {
      validate(
        need(!is.null(cm_var), "Please select a concomittant medicine variable.")
      )
      if (ADSL$USUBJID %in% ADCM_FILTERED$USUBJID) {
        chunks_push(bquote({
          # ADCM
          ADCM <- ADCM_FILTERED[, .(adcm_vars)] # nolint
          ADCM <- ADSL %>% # nolint
            left_join(ADCM, by = c("STUDYID", "USUBJID")) %>% # nolint
            as.data.frame() %>%
            filter(!is.na(ASTDT)) %>%
            mutate(ASTDY = as.numeric(difftime(
              ASTDT,
              as.Date(substr(as.character(eval(parse(text = .(sl_start_date)))), 1, 10)),
              units = "days")) + 1) %>%
            filter(!is.na(AENDT)) %>%
            mutate(AENDY = as.numeric(difftime(
              AENDT,
              as.Date(substr(as.character(eval(parse(text = .(sl_start_date)))), 1, 10)),
              units = "days")) + 1) %>%
            select(USUBJID, ASTDT, AENDT, ASTDY, AENDY, !!quo(.(cm_var)))
          if (length(unique(ADCM$USUBJID)) > 0) {
            ADCM <- ADCM[which(ADCM$AENDY >= -28 | is.na(ADCM$AENDY) == TRUE # nolint
                               & is.na(ADCM$ASTDY) == FALSE), ]
          }
          cm <- list(data = data.frame(ADCM), var = as.vector(ADCM[, .(cm_var)]))
        })
        )
        chunks_eval()
        ADCM <- chunks_get_var("ADCM") # nolint
        if (is.null(ADCM) | nrow(ADCM) == 0) {
          empty_cm <- TRUE
        }
      } else {
        empty_cm <- TRUE
        chunks_push(bquote(cm <- NULL)) #nolint
      }
    } else {
      chunks_push(bquote(cm <- NULL))
    }

    chunks_push_new_line()

    # check
    chunks_eval()
    if (select_plot["ex"]) {
      validate(
        need(!is.null(ex_var), "Please select an exposure variable.")
      )
      if (ADSL$USUBJID %in% ADEX_FILTERED$USUBJID) {
        chunks_push(bquote({
          #ADEX
          ADEX <- ADEX_FILTERED[, .(adex_vars)] # nolint
          ADEX <- ADSL %>% # nolint
            left_join(ADEX, by = c("STUDYID", "USUBJID")) %>% # nolint
            as.data.frame() %>%
            filter(PARCAT1 == "INDIVIDUAL" & PARAMCD == "DOSE" & !is.na(AVAL)) %>%
            filter(!is.na(ASTDT)) %>%
            select(
              USUBJID, ASTDT, PARCAT2,
              AVAL, AVALU, PARAMCD, !!quo(.(sl_start_date)))
          ADEX <- split(ADEX, ADEX$USUBJID) %>% # nolint
            lapply(function(pinfo) {
              pinfo %>%
                arrange(PARCAT2, PARAMCD, ASTDT) %>%
                ungroup %>%
                mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
                mutate(
                  Modification = case_when(diff < 0 ~ "Decrease",
                                           diff > 0 ~ "Increase",
                                           diff == 0 ~ "None")) %>%
                mutate(ASTDT_dur = as.numeric(
                  as.Date(substr(as.character(ASTDT), 1, 10)) -
                    as.Date(substr(as.character(eval(parse(text = .(sl_start_date)))), 1, 10))) + 1
                )
            }) %>%
            Reduce(rbind, .) %>%
            as.data.frame %>%
            select(-diff)
          ex <- list(data = data.frame(ADEX), var = as.vector(ADEX[, .(ex_var)]))
        })
        )
        chunks_eval()
        ADEX <- chunks_get_var("ADEX") # nolint
        if (is.null(ADEX) | nrow(ADEX) == 0) {
          empty_ex <- TRUE
        }
      } else {
        empty_ex <- TRUE
        chunks_push(bquote(ex <- NULL)) #nolint
      }
    } else {
      chunks_push(bquote(ex <- NULL))
    }

    chunks_push_new_line()

    # check
    chunks_eval()


    if (select_plot["lb"]) {
      validate(
        need(!is.null(lb_var), "Please select a lab variable.")
      )
      if (ADSL$USUBJID %in% ADLB_FILTERED$USUBJID) {
        req(input$lb_var_show != input$lb_var)
        chunks_push(bquote({
          ADLB <- ADLB_FILTERED[, .(adlb_vars)] # nolint
          ADLB <- ADSL %>% # nolint
            left_join(ADLB, by = c("STUDYID", "USUBJID")) %>% # nolint
            as.data.frame() %>%
            group_by(USUBJID) %>%
            mutate(ANRIND = factor(
              .data$ANRIND,
              levels = c("HIGH", "LOW", "NORMAL"))) %>%
            filter(
              !is.na(.data$LBSTRESN) & !is.na(.data$ANRIND)) %>%
            as.data.frame() %>%
            select(
              USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADT, AVISITN, !!quo(.(sl_start_date)),
              LBTESTCD, ANRIND, !!quo(.(lb_var)))

          ADLB <- ADLB %>%  # nolint
            mutate(ADY = as.numeric(difftime(
              .data$ADT,
              as.Date(substr(as.character(eval(parse(text = .(sl_start_date)))), 1, 10)),
              units = "days")) + 1) %>%
            filter(.data[[.(lb_var)]] %in% .(input$lb_var_show))
          lb <- list(data = data.frame(ADLB), var = as.vector(ADLB[, .(lb_var)]))
        })
        )
        chunks_eval()
        ADLB <- chunks_get_var("ADLB") # nolint
        if (is.null(ADLB) | nrow(ADLB) == 0) {
          empty_lb <- TRUE
        }
      } else {
        empty_lb <- TRUE
        chunks_push(bquote(lb <- NULL)) #nolint
      }
    } else {
      chunks_push(bquote(lb <- NULL))
    }


    chunks_push_new_line()
    # check
    chunks_eval()

    # Check that at least 1 dataset is selected

    validate(
      need(any(select_plot), "Please select an ADaM dataset.")
    )

    # Check the subject has information in at least one selected domain
    empty_data_check <- c(empty_ex, empty_ae, empty_rs, empty_cm, empty_lb)

    validate(
      need(
        any(!empty_data_check & select_plot),
        "The subject does not have information in any selected domain.")
    )

    # Check the subject has information in all the selected domains
    if (any(empty_data_check & select_plot)) {
      showNotification(
        paste(
          "This subject does not have information in the ",
          paste(c(possible_plot[(empty_data_check & select_plot)]), collapse = ", "),
          " domain.", sep = ""),
        duration = 8,
        type = "warning"
        )
    }

    # Convert x_limit to numeric vector
    if (!is.null(x_limit) || x_limit != "") {
      chunks_push(bquote(x_limit <-
                           as.numeric(unlist(strsplit(.(x_limit), ",")))))
      chunks_eval()
      x_limit <- chunks_get_var("x_limit")
    }

    validate(need(
      all(!is.na(x_limit)),
      "Not all values entered for study days range were numeric."))
    validate(need(
      x_limit[1] < x_limit[2],
      "The lower limit for study days range should come first.")
    )

    chunks_push_new_line()

    # check
    chunks_eval()
    validate(need(chunks_is_ok(), "Data could not be constructed."))

    chunks_push(call(
      "g_patient_profile",
      ex = bquote(ex),
      ae = bquote(ae),
      rs = bquote(rs),
      cm = bquote(cm),
      lb = bquote(lb),
      arrow_end_day = quote(ADSL$max_day),
      xlim = quote(x_limit),
      xlab = "Study Day",
      title = paste("Patient Profile: ", bquote(.(patient_id)))

    ))

    chunks_eval()

  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Patient profile Plot",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })

}
