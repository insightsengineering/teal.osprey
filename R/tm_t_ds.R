
#' Disposition Table Teal Module
#'
#' Display DST01 Patient Disposition Table as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#' @param arm_var_choices vector with variable names that can be used as
#'   \code{arm_var}
#' @param class_var_choices vector with \code{class_var} choices
#' @param term_var_choices vector with \code{term_var} choices
#'
#' @return an \code{\link[teal]{module}} object
#' @export
#'
#' @template author_zhanc107
#'
#' @examples
#'
#' \dontrun{
#' # Example using stream (ADaM) dataset
#' library(dplyr)
#'
#' ASL <- rADSL %>% mutate(USUBJID = SUBJID)
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- rADSL %>% mutate(USUBJID = SUBJID)",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_ds(
#'        label = "Patient Disposition Table",
#'        dataname = "ASL",
#'        arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
#'        class_var =  choices_selected(selected = "EOSSTT", choices = "EOSSTT"),
#'        term_var = choices_selected(selected = "DCSREAS", choices = c("DCSREAS", "DCSREASP")),
#'        total_col = TRUE
#'     )
#'   )
#' )
#'
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
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var$choices, a$arm_var$selected, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variable", a$class_var$choices, a$class_var$selected,
        multiple = FALSE),
      optionalSelectInput(ns("term_var"), "Term Variable", a$term_var$choices, a$term_var$selected,
        multiple = FALSE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_t_ds <- function(input, output, session, datasets, dataname) {

  init_chunks()

  output$table <- renderUI({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    chunks_reset(envir = environment())

    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients

    asl_vars <- unique(c("STUDYID","USUBJID", arm_var, class_var, term_var))

    if(all_p == TRUE){
      total <- "All Patients"
    } else {
      total <- NULL
    }
    chunks_push(bquote({
      ASL_f <- as.data.frame(ASL_FILTERED[, .(asl_vars)])
    }))

    chunks_eval()

    # check dataset
    validate_has_data(chunks_get_var("ASL_f"), min_nrow = 1)
    validate(need(chunks_get_var("ASL_f")[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% chunks_get_var("ASL_f")[[arm_var]]), "arm values can not contain empty strings ''"))

    chunks_push(call(
      "t_ds",
      class = bquote(ASL_f[, .(class_var)]),
      term = bquote(ASL_f[, .(term_var)]),
      id = bquote(ASL_f$USUBJID),
      col_by = bquote(droplevels(as.factor(ASL_f[[.(arm_var)]]))),
      total = total
    ))

    tbl <- chunks_eval()

    if (!chunks_is_ok()) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))

    as_html(tbl)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
        title = "Disposition table",
        rcode = get_rcode(
            datasets = datasets,
            title = "R Code for the Current AE Overview Table"
        )
    )
  })

}
