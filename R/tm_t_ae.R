#' Display AET02 Adverse Events Table Teal Module
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
#' @param class_var class variables selected for display
#' @param class_var_choices vector with \code{class_var} choices
#' @param term_var term variables selected for display
#' @param term_var_choices vector with \code{term_var} choices
#' @param total_col argument for appearance of All Patients column,
#'  default here is TRUE
#' @inheritParams teal::standard_layout
#' 
#' @return an \code{\link[teal]{module}} object
#' @export
#' 
#' @author Carolyn Zhang
#' 
#' 
#' @examples 
#' 
#' 
#' library(dplyr)
#' suppressPackageStartupMessages(library(tidyverse))
#' library(random.cdisc.data)
#' library(rtables)
#' 
#' data <- left_join(radam("AAE", N=10),radam("ADSL", N=10))
#'
#' adae <- data.frame(
#'   AEBODSYS = data$AEBODSYS,
#'   AEDECOD =  data$AEDECOD,
#'   USUBJID = data$USUBJID,
#'   ARM = data$ARM
#' )
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = adae),
#'   modules = root_modules(
#'     tm_t_ae(
#'        label = "Adverse Events Table",
#'        dataname = "ADAE",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        class_var = "All Classes",
#'        class_var_choices = c(unique(data$AEBODSYS), "All Classes"),
#'        term_var = "All Terms",
#'        term_var_choices = c(unique(data$AEDECOD), "All Terms"),
#'        total_col = TRUE
#'    )
#'   )
#' )
#'    
#' shinyApp(x$ui, x$server) 
#'   
#' 
tm_t_ae <- function(label, 
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
    server = srv_t_ae,
    ui = ui_t_ae,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
  
}

ui_t_ae <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variables", a$class_var_choices, a$class_var, multiple = TRUE),
      optionalSelectInput(ns("term_var"), "Term variables", a$term_var_choices, a$term_var, multiple = TRUE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_ae <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    analysis = "# Not Calculated"
  )
  
  
  output$table <- renderUI({
    #ADAE_f <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ADAE <- datasets$get_data("ASL", reactive = FALSE, filtered = FALSE)
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    
    chunks$analysis <<- "# Not Calculated"
    
    validate_has_data(ADAE, min_nrow = 15)    
    validate(need(ADAE[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ADAE[[arm_var]]), "arm values can not contain empty strings ''"))
    
    #data_name <- paste0(dataname, "_FILTERED")
    #assign(data_name, ADAE_f)
    
    all_p <- input$All_Patients
    
    if(all_p == TRUE){
      total = "ALL Patients"
    }
    else{
      total = "NONE"
    }
    
    ADAE_f <- ADAE
    if(class_var != "All Classes"){
      ADAE_f <- ADAE %>% 
        filter(AEBODSYS %in% class_var) %>% 
        droplevels()
    }
    if(term_var != "All Terms"){
      ADAE_f <- ADAE %>% 
        filter(AEDECOD %in% term_var) %>% 
        droplevels()
    }
    
    validate(need(nrow(ADAE_f) > 1, "need at least 1 data point"))
   
    chunks$analysis <<- call(
      "t_ae",
      class = bquote(ADAE_f$AEBODSYS), 
      term = bquote(ADAE_f$AEDECOD), 
      id = bquote(ADAE_f$USUBJID),
      col_by = bquote(as.factor(.(ADAE_f)[[.(arm_var)]])),
      total = total
    )

    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Adverse Events Table",
      datanames = "ASL",
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current AE Overview Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}