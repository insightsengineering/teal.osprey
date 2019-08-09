
#' Adverse Events Table Teal Module
#'
#' This shiny module displays the adverse events table (AET02).
#'
#' @inheritParams teal.devel::standard_layout
#'
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#' @param filter_var \code{\link[teal]{choices_selected}}
#'   variable name of data filter, please see details regarding 
#'   expected values, default is \code{NULL}
#' @param arm_var \code{\link[teal]{choices_selected}}
#'   variable name in analysis data that is used as 
#'   \code{col_by} argument for the respective \code{tern} or \code{osprey} 
#'   function.
#' @param class_var \code{\link[teal]{choices_selected}} class variables selected for display
#' @param term_var \code{\link[teal]{choices_selected}} term variables selected for display
#' @param total_col argument for appearance of "All Patients" column (default is
#'   \code{TRUE})
#' @param code_data_processing string with data preprocessing before the teal
#'   app is initialized, default is NULL
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
#' @examples 
#' #Example using stream (ADaM) dataset 
#' library(dplyr)
#' 
#' ASL <- rADSL %>% mutate(USUBJID = SUBJID)
#' AAE <- rADAE %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N"))  %>% mutate(USUBJID = SUBJID)
#' 
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL, 
#'     AAE = AAE,
#'     code = paste0(c(
#'       "ASL <- rADSL %>% mutate(USUBJID = SUBJID)",
#'       'AAE <- rADAE %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N")) %>% mutate(USUBJID = SUBJID)'),
#'       collapse = ";"
#'     ),
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_ae(
#'        label = "Adverse Events Table",
#'        dataname = "AAE",
#'        filter_var = choices_selected(
#'          choices = c("DTHFL", "flag1"),
#'          selected = NULL
#'        ),
#'        arm_var = choices_selected(
#'          choices = c("ARM", "ARMCD"),
#'          selected = "ARM"
#'        ),
#'        class_var = choices_selected(
#'          choices = c("AEBODSYS", "AEHLTCD"),
#'          selected = "AEBODSYS"
#'        ),
#'        term_var = choices_selected(
#'          choices = c("AEDECOD", "AETERM"),
#'          selected = "AEDECOD"
#'        ),
#'        total_col = TRUE
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_ae <- function(label,
                    dataname,
                    filter_var = NULL,
                    arm_var,
                    class_var,
                    term_var,
                    total_col = TRUE,
                    pre_output = NULL,
                    post_output = NULL,
                    code_data_processing = NULL) {
  args <- as.list(environment())
  
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.null(filter_var) || is.choices_selected(filter_var))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(class_var))
  stopifnot(is.choices_selected(term_var))
  stopifnot(is.logical.single(total_col))
  
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
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("filter_var"),
        label = div(
          "Preset data filters:",
          tags$br(),
          helpText("Observations with value of 'Y' for selected variable(s) will be used for analysis")
        ),
        choices = a$filter_var$choices,
        selected = a$filter_var$selected,
        multiple = TRUE
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Select arm variable:",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("class_var"),
        "Select class variables:",
        a$class_var$choices,
        a$class_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("term_var"),
        "Select term variables:",
        a$term_var$choices,
        a$term_var$selected,
        multiple = FALSE
      ),
      checkboxInput(
        ns("All_Patients"),
        "Add All Patients",
        value = a$total_col
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_ae <- function(input,
                     output,
                     session,
                     datasets,
                     dataname,
                     code_data_processing) {
  
  init_chunks()
  
  output$table <- renderUI({
    
    filter_var <- input$filter_var
    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    if (dataname != "ASL") {
      ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
      anl_name <- paste0(dataname, "_FILTERED")
      assign(anl_name, ANL_FILTERED)
    }
    
    chunks_reset(envir = environment())
    
    aae_name <- paste0(dataname, "_FILTERED")
    
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    aae_vars <- unique(c("USUBJID", "STUDYID", class_var, term_var, filter_var))
    anl_vars <- unique(c(asl_vars, aae_vars))
    
    chunks_push(bquote({
      ANL <- .(as.name(aae_name))
    }))
    
    if (!is.null(filter_var)) {
      chunks_push(bquote(
        ANL <- quick_filter(.(filter_var), ANL) %>%
          droplevels()
      ))
    }
    
    chunks_push(bquote({
      attr(ANL[, .(class_var)], "label") <- label_aevar(.(class_var))
      attr(ANL[, .(term_var)], "label") <- label_aevar(.(term_var))
    }))
    
    chunks_push_new_line()
    
    total <- if (isTRUE(all_p)) {
      "All Patients"
    } else {
      NULL
    }
    
    chunks_push(bquote({
      tbl <- t_ae(
        class = ANL[, .(class_var)],
        term = ANL[, .(term_var)],
        id = ANL$USUBJID,
        col_by = droplevels(as.factor(ANL[[.(arm_var)]])),
        total = .(total)
      )
      tbl
    }))
    
    chunks_eval()

    chunks_validate_all("tbl", "rtable", "Evaluation with tern t_ae failed.")
    tbl <- chunks_get_var("tbl")
    
    as_html(tbl)
  })
  
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Adverse Events Table",
      rcode = get_rcode(
        datasets = datasets,
        title = "R Code for the Current AE Overview Table"
      )
    )
  })
}