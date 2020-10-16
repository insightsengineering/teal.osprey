
#' Disposition Table Teal Module
#'
#' Display DST01 Patient Disposition Table as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#'
#' @return an \code{\link[teal]{module}} object
#' @importFrom rtables as_html
#'
#' @export
#'
#' @template author_zhanc107
#'
#' @examples
#'
#' # Example using stream (ADaM) dataset
#' library(dplyr)
#'
#' ADSL <- rADSL
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- rADSL",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_ds(
#'        label = "Patient Disposition Table",
#'        dataname = "ADSL",
#'        arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
#'        class_var =  choices_selected(selected = "EOSSTT", choices = "EOSSTT"),
#'        term_var = choices_selected(selected = "DCSREAS", choices = c("DCSREAS", "DCSREASP")),
#'        total_col = TRUE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_ds <- function(label,
                    dataname,
                    arm_var,
                    class_var,
                    term_var,
                    total_col = TRUE,
                    pre_output = NULL,
                    post_output = NULL) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_ds,
    ui = ui_t_ds,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )

}

ui_t_ds <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var$choices, a$arm_var$selected, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variable", a$class_var$choices, a$class_var$selected,
        multiple = FALSE),
      optionalSelectInput(ns("term_var"), "Term Variable", a$term_var$choices, a$term_var$selected,
        multiple = FALSE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_t_ds <- function(input, output, session, datasets, dataname) {

  init_chunks()

  output$table <- renderUI({

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint

    chunks_reset(envir = environment())

    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients

    adsl_vars <- unique(c("STUDYID", "USUBJID", arm_var, class_var, term_var)) # nolint
    validate_has_variable(ADSL_FILTERED, adsl_vars)

    if (all_p == TRUE) {
      total <- "All Patients"
    } else {
      total <- NULL
    }
    chunks_push(bquote({
      ADSL_f <- as.data.frame(ADSL_FILTERED[, .(adsl_vars)]) # nolint
    }))

    chunks_safe_eval()

    # check dataset
    validate_has_data(chunks_get_var("ADSL_f"), min_nrow = 1)
    validate(need(chunks_get_var("ADSL_f")[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% chunks_get_var("ADSL_f")[[arm_var]]), "arm values can not contain empty strings ''"))

    chunks_push(call(
      "t_ds",
      class = bquote(ADSL_f[, .(class_var)]),
      term = bquote(ADSL_f[, .(term_var)]),
      id = bquote(ADSL_f$USUBJID),
      col_by = bquote(droplevels(as.factor(ADSL_f[[.(arm_var)]]))),
      total = total
    ))

    tbl <- chunks_safe_eval()

    rtables::as_html(tbl)
  })


  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "Disposition table"
  )

}
