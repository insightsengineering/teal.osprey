#' Adverse Events Summary Table Teal Module
#'
#' Display AET01 Adverse Events Summary Table as a shiny Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#'
#' @return an \code{\link[teal]{module}} object
#' @importFrom rtables as_html
#'
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#'
#' #Example using stream (ADaM) dataset
#' library(dplyr)
#'
#' ADSL <- rADSL
#' ADAE <- rADAE
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAE", ADAE,
#'                   keys = keys(primary = c("STUDYID", "USUBJID", "AETERM", "AESEQ"),
#'                               foreign = c("STUDYID", "USUBJID"),
#'                               parent = "ADSL")),
#'     code = "ADSL <- rADSL; ADAE <- rADAE",
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_t_ae_oview(
#'        label = "AE Overview Summary Table",
#'        dataname = "ADAE",
#'        arm_var = choices_selected(choices = c("ARM", "ARMCD"),
#'                                   selected = "ARM"),
#'        total_col = FALSE
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
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

  stopifnot(is_character_single(label))
  stopifnot(is_character_single(dataname))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is_logical_single(total_col))

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
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ADAE_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    arm_var <- input$arm_var
    all_p <- input$All_Patients

    adae_name <- paste0(dataname, "_FILTERED")
    assign(adae_name, ADAE_FILTERED)

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var, "DTHFL", "DCSREAS")) # nolint
    adae_vars <- unique(## add column name of extra flage here
      c("USUBJID", "STUDYID", "AESOC", "AEDECOD", "AESDTH", "AESER", "AEACN", "AEREL", "AETOXGR"))

    chunks_reset(envir = environment())

    chunks_push(bquote({
      ADSL <- ADSL_FILTERED[, .(adsl_vars)] %>% as.data.frame() # nolint
      ADAE <- .(as.name(adae_name))[, .(adae_vars)] %>% as.data.frame() # nolint
      ANL  <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID")) %>% as.data.frame() # nolint
    }))
    chunks_push_new_line()

    total <- if (isTRUE(all_p)) { # nolint
      "All Patients"
    } else {
      NULL
    }
    chunks_eval()

    chunks_push(call(
      "t_ae_oview",
      id = bquote(ANL$USUBJID),
      class = bquote(ANL$AESOC),
      term = bquote(ANL$AEDECOD),
      flags = bquote(data.frame(
        dthfl = ANL$DTHFL,
        dcsreas = ANL$DCSREAS,
        aesdth = ANL$AESDTH,
        aeser = ANL$AESER,
        aeacn = ANL$AEACN,
        aerel = ANL$AEREL,
        aetoxgr = ANL$AETOXGR)),
      display_id = c("fatal", "ser", "serwd", "serdsm", "relser", "wd", "dsm", "rel", "relwd", "reldsm", "ctc35"),
      col_by = bquote(droplevels(as.factor(ANL[[.(arm_var)]]))),
      total = total
    ))

    tbl <- chunks_eval()
    rtables::as_html(tbl)
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
