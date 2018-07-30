
#' Disposition Table Teal Module
#' 
#' Display DST01 Patient Disposition Table as a shiny module
#' 
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var single name of variable in analysis data that is used as
#'   \code{col_by} argument for the respective \code{tern} function.
#' @param arm_var_choices vector with variable names that can be used as
#'   \code{arm_var}
#' @param term_var term variables selected for display
#' @param term_var_choices vector with \code{term_var} choices
#' @param total_col argument for appearance of All Patients column,
#'  default here is TRUE
#' @inheritParams teal::standard_layout
#' 
#' @return an \code{\link[teal]{module}} object
#' @export
#' 
#' @template author_zhanc107
#' 
#' @examples 
#' #Example using random dataset 
#' 
#' library(dplyr)
#' 
#' data("rADSL")
#' ASL <- rADSL
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL), 
#'   modules = root_modules(
#'     tm_t_ds(
#'        label = "Patient Disposition Table",
#'        dataname = "ASL",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        class_var = "EOSSTT",
#'        class_var_choices = "EOSSTT",
#'        term_var = "DCSREAS",
#'        term_var_choices = c("DCSREAS", "DCSREASP"),
#'        total_col = TRUE
#'    )
#'   )
#' )
#'    
#' shinyApp(x$ui, x$server)  
#' 
#'   
#' 
tm_t_ds <- function(label, 
                    dataname, 
                    arm_var, 
                    arm_var_choices, 
                    class_var,
                    class_var_choices,
                    term_var, 
                    term_var_choices, 
                    total_col = TRUE, 
                    pre_output = NULL, 
                    post_output = NULL, 
                    code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_ds,
    ui = ui_t_ds,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
  
}

ui_t_ds <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variable", a$class_var_choices, a$class_var, multiple = FALSE),
      optionalSelectInput(ns("term_var"), "Term Variable", a$term_var_choices, a$term_var, multiple = FALSE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_ds <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    vars = "# Not Calculated",
    data = "# Not Calculated",
    analysis = "# Not Calculated"
  )
  
  
  output$table <- renderUI({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    
    # # validate your input values
    # validate_standard_inputs(
    #   ASL = ASL_FILTERED,
    #   aslvars = c("USUBJID", arm_var, class_var, term_var)
    # )
    
    asl_vars <- unique(c("USUBJID", arm_var, class_var, term_var))
    
    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      class_var <- .(class_var)
      term_var <- .(term_var)
      all_p <- .(all_p)
      asl_vars <- .(asl_vars)

    })
    
    chunks$data <<- bquote({
      ASL_f <- ASL_FILTERED
      
      if(all_p == TRUE){
        total = "All Patients"
      } else{
        total = NULL
      }
      
      ASL_f <- ASL_f[, .(asl_vars)] %>% 
        as.data.frame() 
      
    })
    
    eval(chunks$data)
    
    validate_has_data(ASL_f, min_nrow = 1)
    validate(need(ASL_f[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ASL_f[[arm_var]]), "arm values can not contain empty strings ''"))
    
    chunks$analysis <<- call(
      "t_ds",
      class = bquote(ASL_f[, class_var]),
      term = bquote(ASL_f[, term_var]), 
      id = bquote(ASL_f$USUBJID),
      col_by = bquote(as.factor(ASL_f[[.(arm_var)]])),
      total = total
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Patient Disposition Table",
      datanames = dataname,
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$vars, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$data, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Patient Disposition Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}