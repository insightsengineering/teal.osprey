
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
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#'
#' @examples
#'
#' library(dplyr)
#'
#' ASL <- rADSL %>% mutate(USUBJID = SUBJID)
#' AAE <- rADAE %>% mutate(flag1 = ifelse(SEX == "F", "Y", "N")) %>%
#'                  mutate(USUBJID = SUBJID)
#'
#' app <- init(
#'   data = cdisc_data(ASL = ASL,
#'                     AAE = AAE,
#'                     code = "ASL <- rADSL %>% mutate(USUBJID = SUBJID)
#'                     AAE <- rADAE %>%
#'                     mutate(flag1 = ifelse(SEX == 'F', 'Y', 'N')) %>%
#'                     mutate(USUBJID = SUBJID)"),
#'   modules = root_modules(
#'     tm_t_ae_ctc(
#'       label = "Adverse Events Table By Highest NCI CTCAE Grade",
#'       dataname = "AAE",
#'       filter_var = choices_selected(selected = NULL, choices = c("AESER", "flag1")),
#'       arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
#'       class_var = choices_selected(selected = "AEBODSYS", choices = c("AEBODSYS", "DEFAULT")),
#'       term_var = choices_selected(selected = "AEDECOD", choices = c("AEDECOD", "DEFAULT")),
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
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.null(filter_var) || is.choices_selected(filter_var))
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(class_var))
  stopifnot(is.choices_selected(term_var))
  stopifnot(is.character.single(toxgr_var))
  stopifnot(is.logical.single(total_col))

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
      optionalSelectInput(ns("filter_var"),
                          label = div("Preset Data Filters",
                                      tags$br(),
                                      helpText("Observations with value of 'Y' for selected variable(s) will be used for analysis")),
                          choices = a$filter_var$choices, selected = a$filter_var$selected, multiple = TRUE
      ),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var$choices,
                          a$arm_var$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("class_var"),
                          "Class Variables",
                          a$class_var$choices,
                          a$class_var$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("term_var"),
                          "Term Variables",
                          a$term_var$choices,
                          a$term_var$selected,
                          multiple = FALSE),
      checkboxInput(ns("All_Patients"),
                    "Add All Patients",
                    value = a$total_col)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_ae_ctc <- function(input, output, session, datasets, dataname, toxgr_var) {
  # use teal.devel code chunks
  init_chunks()

  output$table <- renderUI({
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE) # nolint
    if (dataname != "ASL") {
      ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) # nolint
      anl_name <- paste0(dataname, "_FILTERED")
      assign(anl_name, ANL_FILTERED)
    }

    arm_var <- input$arm_var
    class_var <- input$class_var
    term_var <- input$term_var
    all_p <- input$All_Patients
    filter_var <- input$filter_var

    chunks_reset(envir = environment())

    aae_name <- paste0(dataname, "_FILTERED")

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    aae_vars <- unique(c("USUBJID", "STUDYID", class_var, term_var, filter_var, toxgr_var))
    anl_vars <- c(asl_vars, aae_vars) # nolint

    chunks_push(bquote({
      ANL <- .(as.name(aae_name)) %>% select(.(aae_vars)) # nolint
    }))

    if (!is.null(filter_var)) {
      chunks_push(bquote(
        ANL <- quick_filter(.(filter_var), ANL) %>% droplevels() # nolint
      ))
    }

    chunks_push(bquote({
      ANL <- ASL_FILTERED %>%  # nolint
        select(.(asl_vars)) %>%
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

    as_html(tbl)
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Adverse Events Table By Highest NCI CTCAE Grade",
      rcode = get_rcode(
        datasets = datasets,
        title = "Adverse Events Table By Highest NCI CTCAE Grade"
      )
    )
  })
}