
#' Adverse Events Table by Highest NCI CTCAE Grade Teal Module
#' 
#' @inheritParams teal::standard_layout
#' @inheritParams tm_t_ae
#' @param toxgr_var variable name of AE toxicitiy grade
#'   
#' @details \code{filter_var} option is designed to work in conjuction with 
#'   filtering function provided by \code{teal} (encoding panel on the right 
#'   hand side of the shiny app). It can be used as quick access to pre-defined 
#'   subsets of the domain datasets (not subject-level dataset) to be used
#'   for analysis, denoted by an value of "Y". Each variable within the
#'   \code{filter_var_choices} is expected to contain values of either "Y" or
#'   "N". If multiple variables are selected as \code{filter_var}, only
#'   observations with "Y" value in each and every selected variables will be
#'   used for subsequent analysis. Flag variables (from ADaM datasets) can be
#'   used directly as filter.
#'   
#' @return an \code{\link[teal]{module}} object
#' @export
#' 
#' @template author_zhanc107
#' @template author_liaoc10
#'   
#'   
#' @examples 
#' 
#' \dontrun{
#' #Example 
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
#'     tm_t_ae_ctc(
#'        label = "Adverse Events Table By Highest NCI CTCAE Grade",
#'        dataname = "AAE",
#'        filter_var = NULL,
#'        filter_var_choices = c("DTHFL", "flag1"), 
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
#' }
#' 
#' 
tm_t_ae_ctc <- function(label, 
                    dataname, 
                    filter_var = NULL,
                    filter_var_choices = NULL,
                    arm_var, 
                    arm_var_choices, 
                    class_var, 
                    class_var_choices, 
                    term_var, 
                    term_var_choices, 
                    toxgr_var = "AETOXGR",
                    total_col = TRUE, 
                    pre_output = NULL, 
                    post_output = NULL, 
                    code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_t_ae_ctc,
    ui = ui_t_ae_ctc,
    ui_args = args,
    server_args = list(dataname = dataname,
                       toxgr_var = toxgr_var,
                       code_data_processing = code_data_processing),
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
      optionalSelectInput(ns("filter_var"), 
                          label = div("Preset Data Filters", tags$br(), helpText("Observations with value of 'Y' for selected variable(s) will be used for analysis")),
                          choices = a$filter_var_choices, selected = a$filter_var, multiple = TRUE),
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

srv_t_ae_ctc <- function(input, output, session, datasets, dataname, toxgr_var, code_data_processing) {
  
  chunks <- list(
    vars = "# Not Calculated", 
    data = "# Not Calculated", 
    analysis = "# Not Calculated"
  )
  
  output$table <- renderUI({
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    filter_var <- input$filter_var
    
    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      class_var <- .(class_var)
      term_var <- .(term_var)
      all_p <- .(all_p)
      filter_var <- .(filter_var)
      toxgr_var <- .(toxgr_var)
    })
    
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    aae_vars <- unique(c("USUBJID", "STUDYID", class_var, term_var, toxgr_var)) 
    
    chunks$data <<- bquote({
      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame()
      
      {if(!("NULL" %in% .(filter_var)) && !is.null(.(filter_var))){
        AAE <- teal.osprey:::quick_filter(.(filter_var), AAE_FILTERED) %>% droplevels()
      } else{
        AAE <- AAE_FILTERED
      }}
      
      AAE <- AAE[, .(aae_vars)] %>% as.data.frame() 
      
      ANL  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID")) %>% 
        as.data.frame()
      
      ANL$TOXGR <- as.numeric(ANL[, toxgr_var])
      
      attr(ANL[, class_var], "label") <- label_aevar(class_var)
      attr(ANL[, term_var], "label") <- label_aevar(term_var)
      attr(ANL[, "TOXGR"], "label") <- label_aevar(toxgr_var)
      
      {if(all_p == TRUE) {
        total = "All Patients"
      } else {
        total = NULL
      }}
    })
    eval(chunks$data)
    
    chunks$analysis <<- call(
      "t_ae_ctc_v2",
      class = bquote(ANL[,class_var]), 
      term = bquote(ANL[,term_var]), 
      id = bquote(ANL$USUBJID),
      grade = bquote(ANL$TOXGR),
      col_by = bquote(as.factor(ANL[[.(arm_var)]])),
      total = total
    )
    
    tbl <- try(eval(chunks$analysis))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
    
    as_html(tbl)
  })
  
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header_osprey(
      title = "Adverse Events Table By CTC Grade",
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