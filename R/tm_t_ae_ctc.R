
#' Adverse Events Table by Highest NCI CTCAE Grade Teal Module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#' @param toxgr_var variable name of AE toxicity grade
#'
#' @details \code{filter_var} option is designed to work in conjunction with
#'   filtering function provided by \code{teal} (encoding panel on the right
#'   hand side of the shiny app). It can be used as quick access to predefined
#'   subsets of the domain datasets (not subject-level dataset) to be used
#'   for analysis, denoted by an value of "Y". Each variable within the
#'   \code{filter_var$choices} is expected to contain values of either "Y" or
#'   "N". If multiple variables are selected as \code{filter_var}, only
#'   observations with "Y" value in each and every selected variables will be
#'   used for subsequent analysis. Flag variables (from ADaM datasets) can be
#'   used directly as filter.
#'
#' @return an \code{\link[teal]{module}} object
#' @importFrom rtables as_html
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#'
#' @examples
#' #Example using stream (ADaM) dataset
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE) %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADAE", ADAE),
#'     code = 'ADSL <- radsl(cached = TRUE)
#'             ADAE <- radae(cached = TRUE) %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N"))'),
#'   modules = root_modules(
#'     tm_t_ae_ctc(
#'       label = "Adverse Events Table By Highest NCI CTCAE Grade",
#'       dataname = "ADAE",
#'       filter_var = choices_selected(selected = NULL, choices = c("AESER", "flag1")),
#'       arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
#'       class_var = choices_selected(selected = "AEBODSYS", choices = c("AEBODSYS", "AEHLT")),
#'       term_var = choices_selected(selected = "AEDECOD", choices = c("AEDECOD", "AETERM")),
#'       total_col = TRUE
#'     )
#'   )
#'   )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_ae_ctc <- function(label,
                        dataname,
                        filter_var = NULL,
                        arm_var,
                        class_var,
                        term_var,
                        toxgr_var = "AETOXGR",
                        total_col = TRUE,
                        pre_output = NULL,
                        post_output = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is_character_single(dataname))
  stopifnot(is.null(filter_var) || is.choices_selected(filter_var))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(class_var))
  stopifnot(is.choices_selected(term_var))
  stopifnot(is_character_single(toxgr_var))
  stopifnot(is_logical_single(total_col))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_ae_ctc,
    ui = ui_t_ae_ctc,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      toxgr_var = toxgr_var
    ),
    filters = dataname
  )
}

ui_t_ae_ctc <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("filter_var"),
        label = div("Preset Data Filters",
        tags$br(),
        helpText("Observations with value of 'Y' for selected variable(s) will be used for analysis")),
        choices = a$filter_var$choices, selected = a$filter_var$selected, multiple = TRUE
      ),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE),
      optionalSelectInput(
        ns("class_var"),
        "Class Variables",
        a$class_var$choices,
        a$class_var$selected,
        multiple = FALSE),
      optionalSelectInput(
        ns("term_var"),
        "Term Variables",
        a$term_var$choices,
        a$term_var$selected,
        multiple = FALSE),
      checkboxInput(
        ns("All_Patients"),
        "Add All Patients",
        value = a$total_col)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_ae_ctc <- function(input, output, session, datasets, dataname, toxgr_var) {
  # use teal.devel code chunks
  init_chunks()

  output$table <- renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    if (dataname != "ADSL") {
      ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
      anl_name <- paste0(dataname, "_FILTERED")
      assign(anl_name, ANL_FILTERED)
    }

    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    filter_var <- input$filter_var

    chunks_reset(envir = environment())

    adae_name <- paste0(dataname, "_FILTERED") #nolint

    adsl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    adae_vars <- unique(c("USUBJID", "STUDYID", class_var, term_var, filter_var, toxgr_var))
    anl_vars <- c(adsl_vars, adae_vars) # nolint

    chunks_push(bquote({
      ANL <- .(as.name(adae_name)) %>% select(.(adae_vars)) # nolint
    }))

    if (!is.null(filter_var)) {
      chunks_push(bquote(
        ANL <- quick_filter(.(filter_var), ANL) %>% droplevels() # nolint
      ))
    }

    chunks_push(bquote({
      ANL <- ADSL_FILTERED %>%  # nolint
        select(.(adsl_vars)) %>%
        left_join(ANL) %>%
        select(.(anl_vars))
    }))

    chunks_push(bquote({
      ANL$TOXGR <- as.numeric(ANL[[.(toxgr_var)]]) # nolint
      attr(ANL[[.(class_var)]], "label") <- label_aevar(.(class_var))
      attr(ANL[[.(term_var)]], "label") <- label_aevar(.(term_var))
      attr(ANL[["TOXGR"]], "label") <- label_aevar(.(toxgr_var))
    }))

    chunks_push_new_line()

    total <- if (isTRUE(all_p)) {
      "All Patients"
    } else {
      NULL
    }

    chunks_eval()

    chunks_push(call(
      "t_ae_ctc_v2",
      class = bquote(ANL[[.(class_var)]]),
      term = bquote(ANL[[.(term_var)]]),
      id = bquote(ANL$USUBJID),
      grade = bquote(ANL$TOXGR),
      col_by = bquote(droplevels(as.factor(ANL[[.(arm_var)]]))),
      total = total
    ))

    tbl <- chunks_eval()

    validate(need(chunks_is_ok(), paste0("could not calculate the table:\n\n", tbl)))

    rtables::as_html(tbl)
  })


  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "Adverse Events Table By Highest NCI CTCAE Grade"
  )
}
