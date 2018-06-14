#' Display AET01 Adverse Events Summary Table Teal Module
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
#' @param total_col argument for appearance of All Patients column,
#'   default here is TRUE
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
#' #Example using stream (adam) dataset 
#' 
#' library(dplyr)
#' suppressPackageStartupMessages(library(tidyverse))
#' library(rtables)
#' 
#' ASL <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adsl.sas7bdat")
#' AAE <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adae.sas7bdat")
#' 
#' x1 <- teal::init(
#'   data = list(ASL = ASL, AAE = AAE),
#'   modules = root_modules(
#'     tm_t_ae_oview(
#'        label = "AE Overview Summary Table",
#'        dataname = "AAE",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        total_col = FALSE
#'    )
#'   )
#' )
#'    
#' shinyApp(x1$ui, x1$server) 
#' 
#' \dontrun{
#' 
#' library(dplyr)
#' suppressPackageStartupMessages(library(tidyverse))
#' library(rtables)
#' 
#' adae <- read_bce("/opt/BIOSTAT/home_ext2/qit3/cdt70194/go39733/libraries/adae.sas7bdat")
#' adae <- data.frame(USUBJID = adae$USUBJID,
#'    AESOC = adae$AESOC,
#'    AEDECOD = adae$AEDECOD,
#'    DTHFL = adae$DTHFL,
#'    DCSREAS = adae$DCSREAS,
#'    AESDTH = adae$AESDTH,
#'    AESER = adae$AESER,
#'    AEACN = adae$AEACN,
#'    AREL = adae$AREL,
#'    AEREL = adae$AEREL,
#'    AETOXGR = adae$AETOXGR,
#'    ARM = adae$ARM)
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = adae),
#'   modules = root_modules(
#'     tm_t_ae_oview(
#'        label = "AE Overview Summary Table",
#'        dataname = "ADAE",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        total_col = FALSE
#'    )
#'   )
#' )
#'    
#' shinyApp(x$ui, x$server) 
#' }
#'   
#' 
tm_t_ae_oview <- function(label, 
                          dataname, 
                          arm_var, 
                          arm_var_choices, 
                          total_col = TRUE, 
                          pre_output = NULL, 
                          post_output = NULL, 
                          code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_ae_oview,
    ui = ui_t_ae_oview,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
  
}

ui_t_ae_oview <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_ae_oview <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    vars = "# Not Calculated",
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({
    #if merging asl and aae
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    ADAE  <- merge(ASL_FILTERED, AAE_FILTERED) %>% 
      filter(AEBODSYS != "") %>%
      filter(AEDECOD != "") %>%
      as.data.frame()
    
    #ADAE <- datasets$get_data("ASL", reactive = FALSE, filtered = FALSE)
    arm_var <- input$arm_var
    
    validate_has_data(ADAE, min_nrow = 15)    
    validate(need(ADAE[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ADAE[[arm_var]]), "arm values can not contain empty strings ''"))
    
    all_p <- input$All_Patients
    if(all_p == TRUE){
      total = "ALL Patients"
    }
    else{
      total = "NONE"
    }
    
    chunks$vars <<- bquote({
      ADAE <- .(ADAE)
      arm_var <- .(arm_var)
      dataname <- .(dataname)
      total <- .(total)
    })
    
    chunks$analysis <<- call(
      "t_ae_oview",
      id = bquote(ADAE$USUBJID), 
      class = bquote(ADAE$AESOC), 
      term = bquote(ADAE$AEDECOD), 
      dthfl = bquote(ADAE$DTHFL), 
      dcsreas = bquote(ADAE$DCSREAS), 
      aesdth = bquote(ADAE$AESDTH), 
      aeser = bquote(ADAE$AESER), 
      aeacn = bquote(ADAE$AEACN), 
      arel = bquote(ADAE$AREL), 
      aerel = bquote(ADAE$AEREL), 
      aetoxgr = bquote(ADAE$AETOXGR),
      col_by = bquote(as.factor(.(ADAE)[[.(arm_var)]])),
      total = total
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "AE Overview Summary Table",
      datanames = "ASL",
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