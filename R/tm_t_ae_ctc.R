#' Adverse Events Table by Highest NCI CTCAE Grade Teal Module
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
#' @param sort_by_var argument for order of class and term elements in table,
#'  defaulte here is "count"
#' @param sort_by_var_choices vector with \code{sort_by_var} choices
#' @inheritParams teal::standard_layout
#' 
#' @return an \code{\link[teal]{module}} object
#' @export
#' 
#' @author Carolyn Zhang
#' 
#' 
#' @examples 
#' #Example 
#' library(random.cdisc.data)
#' library(dplyr)
#' suppressPackageStartupMessages(library(tidyverse))
#' library(rtables)
#' 
#' ASL <- radam("ASL", N = 10)
#' AAE <- radam("AAE", ADSL = ASL)
#' 
#' x1 <- teal::init(
#'   data = list(ASL = ASL, AAE = AAE),
#'   modules = root_modules(
#'     tm_t_ae_ctc(
#'        label = "Adverse Events Table By Highest NCI CTCAE Grade",
#'        dataname = "AAE",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        class_var = "AEBODSYS",
#'        class_var_choices = c("AEBODSYS", "DEFAULT"),
#'        term_var = "AEDECOD",
#'        term_var_choices = c("AEDECOD", "DEFAULT"),
#'        total_col = TRUE,
#'        sort_by_var = "count",
#'        sort_by_var_choices = c("count", "alphabetical")
#'    )
#'   )
#' )
#'    
#' shinyApp(x1$ui, x1$server)  
#' 
#' 
tm_t_ae_ctc <- function(label, 
                    dataname, 
                    arm_var, 
                    arm_var_choices, 
                    class_var, 
                    class_var_choices, 
                    term_var, 
                    term_var_choices, 
                    total_col = TRUE, 
                    sort_by_var,
                    sort_by_var_choices,
                    pre_output = NULL, 
                    post_output = NULL, 
                    code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_ae_ctc,
    ui = ui_t_ae_ctc,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
  
}

ui_t_ae_ctc <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variables", a$class_var_choices, a$class_var, multiple = FALSE),
      optionalSelectInput(ns("term_var"), "Term Variables", a$term_var_choices, a$term_var, multiple = FALSE),
      radioButtons(ns("sort_by_var"), "Sort By Variable", a$sort_by_var_choices, a$sort_by_var),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_ae_ctc <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    vars = "# Not Calculated", 
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    sort_by_var <- input$sort_by_var
    
    ADAE  <- merge(ASL_FILTERED[,c("USUBJID", "STUDYID", arm_var)], AAE_FILTERED) %>% 
      as.data.frame()
    
    ADAE <- ADAE[!(ADAE[,class_var] == ""),]
    ADAE <- ADAE[!(ADAE[,term_var] == ""),]
    
    validate_has_data(ADAE, min_nrow = 1)    
    validate(need(ADAE[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ADAE[[arm_var]]), "arm values can not contain empty strings ''"))
    
    all_p <- input$All_Patients
    
    if(all_p == TRUE){
      total = "ALL Patients"
    } else{
      total = "NONE"
    }
    
    ADAE_f <- ADAE
    
    validate(need(nrow(ADAE_f) > 1, "need at least 1 data point"))
    
    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      ADAE_f <- .(ADAE_f) 
      sort_by_var <- .(sort_by_var)
    })
    
    chunks$analysis <<- call(
      "t_ae_ctc_v2",
      class = bquote(ADAE_f[[.(class_var)]]), 
      term = bquote(ADAE_f[[.(term_var)]]), 
      id = bquote(ADAE_f$USUBJID),
      grade = bquote(as.numeric(ADAE_f$AETOXGR)),
      col_by = bquote(as.factor(.(ADAE_f)[[.(arm_var)]])),
      total = total,
      sort_by = bquote(sort_by_var)
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Adverse Events Table",
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