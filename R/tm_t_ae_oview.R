#' Adverse Events Summary Table Teal Module
#'
#' Display AET01 Adverse Events Summary Table as a shiny Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#'
#' @return an \code{\link[teal]{module}} object
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
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
#'
#' x1 <- teal::init(
#'   data = cdisc_data(
#'         ASL = ASL,
#'         AAE = AAE,
#'         code = "ASL <- rADSL; AAE <- rADAE"
#'         check = FALSE
#'       ),
#'   modules = root_modules(
#'     tm_t_ae_oview(
#'        label = "AE Overview Summary Table",
#'        dataname = "AAE",
#'        arm_var = choices_selected(choices = c("ARM", "ARMCD"),
#'                                   selected = "ARM"),
#'        total_col = FALSE
#'     )
#'   )
#' )
#'
#' shinyApp(x1$ui, x1$server)
#'
#' }
#'
#'
tm_t_ae_oview <- function(label,
                          dataname,
                          arm_var,
                          total_col = TRUE,
                          pre_output = NULL,
                          post_output = NULL,
                          code_data_processing = NULL) {

  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.logical.single(total_col))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_ae_oview,
    ui = ui_t_ae_oview,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )

}

ui_t_ae_oview <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("arm_var"),
        "Select arm variable:",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE
      ),
      checkboxInput(ns("All_Patients"), "Add All Patients", value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_t_ae_oview <- function(input, output, session, datasets, dataname) {
  init_chunks()

  output$table <- renderUI({
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE) # nolint
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) # nolint

    arm_var <- input$arm_var
    all_p <- input$All_Patients

    aae_name <- paste0(dataname, "_FILTERED")
    assign(aae_name, AAE_FILTERED) # so that we can refer to the 'correct' data name

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, "DTHFL", "DCSREAS")) # nolint
    aae_vars <- unique(c("USUBJID", "STUDYID", "AESOC", "AEDECOD",
                         "AESDTH", "AESER", "AEACN", "AEREL", "AETOXGR")) ## add column name of extra flage here

    chunks_reset(envir = environment())

    chunks_push(bquote({
      arm_var <- .(arm_var)
      all_p <- .(all_p)
    }))

    chunks_push(bquote({
      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame() # nolint
      AAE <- .(as.name(aae_name))[, .(aae_vars)] %>% as.data.frame() # nolint

      ANL  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID")) %>% # nolint
        as.data.frame()

      flag <- data.frame(dthfl = ANL$DTHFL,
                         dcsreas = ANL$DCSREAS,
                         aesdth = ANL$AESDTH,
                         aeser = ANL$AESER,
                         aeacn = ANL$AEACN,
                         aerel = ANL$AEREL,
                         aetoxgr = ANL$AETOXGR)
      display <- c("fatal", "ser", "serwd", "serdsm", "relser",
                   "wd", "dsm", "rel", "relwd", "reldsm", "ctc35")
    }))

    total <- if (isTRUE(all_p)) { # nolint
      "All Patients"
    } else {
      NULL
    }

    chunks_push(bquote({
      tbl <- t_ae_oview(
        id = ANL$USUBJID,
        class = ANL$AESOC,
        term = ANL$AEDECOD,
        flags = flag,
        display_id = display,
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
