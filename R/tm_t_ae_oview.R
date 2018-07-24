
#' Adverse Events Summary Table Teal Module
#' 
#' Display AET01 Adverse Events Summary Table as a shiny Module
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
#' @template author_zhanc107
#' 
#' 
#' @examples
#' #Example using stream (adam) dataset 
#' library(dplyr)
#' 
#' data("rADSL")
#' data("rADAE")
#' 
#' ASL <- rADSL
#' AAE <- rADAE
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
    data = "# Not Calculated",
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    all_p <- input$All_Patients
    
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    aae_vars <- unique(c("USUBJID", "STUDYID", "DTHFL", "DCSREAS", 
                         "AESDTH", "AESER", "AESOC", "AEDECOD", 
                         "AEACN", "AREL", "AEREL", "AETOXGR")) ## add column name of extra flage here

    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      all_p <- .(all_p)
    })
    
    chunks$data <<- bquote({
      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame()
      AAE <- AAE_FILTERED[, .(aae_vars)] %>% as.data.frame() 
      
      ANL  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID")) %>% 
        as.data.frame()
      
      flag <- data.frame(dthfl = ANL$DTHFL,
                         dcsreas = ANL$DCSREAS,
                         aesdth = ANL$AESDTH,
                         aeser = ANL$AESER,
                         aeacn = ANL$AEACN,
                         arel = ANL$AREL,
                         aerel = ANL$AEREL,
                         aetoxgr = ANL$AETOXGR)
      display <- c("fatal", "ser", "serwd", "serdsm", "relser",
                   "wd", "dsm", "rel", "relwd", "reldsm", "ctc35")
      
      if(all_p == TRUE){
        total = "All Patients"
      }
      else{
        total = "None"
      }
    })
    eval(chunks$data)
    
    validate_has_data(ANL, min_nrow = 1)    
    validate(need(ANL[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ANL[[arm_var]]), "arm values can not contain empty strings ''"))
    
    chunks$analysis <<- call(
      "t_ae_oview",
      id = bquote(ANL$USUBJID), 
      class = bquote(ANL$AESOC), 
      term = bquote(ANL$AEDECOD), 
      flags = bquote(flag),
      ####--------------------------------
      #
      # add extra_flag variables here
      # example: extra_flag = data.frame(NAME_DISPALYED_IN_TABLE = VECTOR),
      #
      ####--------------------------------
      display_id = bquote(display),
      col_by = bquote(as.factor(ANL[[.(arm_var)]])),
      total = total
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "AE Overview Summary Table",
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
      title = "R Code for the Current AE Overview Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}