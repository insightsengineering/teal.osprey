
#' Adverse Events Table Teal Module
#' 
#' This shiny module displays the adverse events table (AET02). 
#' 
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param filter_var variable name of data filter, default here is \code{NULL}
#' @param filter_var_choices vector with \code{filter_var} choices, default 
#' here is \code{NULL}
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
#' @template author_zhanc107
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' #Example using stream (adam) dataset 
#' library(dplyr)
#' 
#' data("rADSL")
#' data("rADAE")
#' 
#' ASL <- rADSL
#' AAE <- rADAE
#' AAE <- AAE %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N")) 
#' 
#' x1 <- teal::init(
#'   data = list(ASL = ASL, AAE = AAE),
#'   modules = root_modules(
#'     tm_t_ae(
#'        label = "Adverse Events Table",
#'        dataname = "AAE",
#'        filter_var = NULL,
#'        filter_var_choices = c(NULL, "DTHFL", "flag1"), 
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        class_var = "AEBODSYS",
#'        class_var_choices = c("AEBODSYS", "DEFAULT"),
#'        term_var = "AEDECOD",
#'        term_var_choices = c("AEDECOD", "DEFAULT"),
#'        total_col = TRUE
#'    )
#'   )
#' )
#'    
#' shinyApp(x1$ui, x1$server)
#' 
#' }
#' 
tm_t_ae <- function(label, 
                    dataname,
                    filter_var = NULL,
                    filter_var_choices = NULL,
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
      optionalSelectInput(ns("filter_var"), "Preset Data Filters", a$filter_var_choices, a$filter_var, multiple = TRUE),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      optionalSelectInput(ns("class_var"), "Class Variables", a$class_var_choices, a$class_var, multiple = FALSE),
      optionalSelectInput(ns("term_var"), "Term Variables", a$term_var_choices, a$term_var, multiple = FALSE),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_t_ae <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  chunks <- list(
    vars = "# Not Calculated", 
    data = "# Not Calculated", 
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    filter_var <- input$filter_var
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    
    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      class_var <- .(class_var)
      term_var <- .(term_var)
      all_p <- .(all_p)
      filter_var <- .(filter_var)
    })
    
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    aae_vars <- unique(c("USUBJID", "STUDYID", arm_var, class_var, term_var)) 
    
    chunks$data <<- bquote({
      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame()
      
      {if(!("NULL" %in% .(filter_var)) && !is.null(.(filter_var))){
        AAE <- teal.osprey:::quick_filter(.(filter_var), AAE_FILTERED) %>% droplevels()
      } else{
        AAE <- AAE_FILTERED
      }}
      
      AAE <- AAE[, .(aae_vars)] %>% as.data.frame() 
      
      ANL  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID", .(arm_var))) %>% 
        as.data.frame()
      
      {if(all_p == TRUE) {
        total = "All Patients"
      } else {
        total = NULL
      }}
    })
    eval(chunks$data)
    
    chunks$analysis <<- call(
      "t_ae",
      class = bquote(ANL[, class_var]), 
      term = bquote(ANL[, term_var]), 
      id = bquote(ANL$USUBJID),
      col_by = bquote(as.factor(ANL[[.(arm_var)]])),
      total = bquote(total)
    )

    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header_osprey(
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