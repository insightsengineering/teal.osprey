#' Teal module for the heatmap by grade
#'
#' @description
#'
#' Display the heatmap by grade as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param sl_dataname (`character`) subject level dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()]
#' @param ex_dataname (`character`) exposures dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()] \cr
#' @param ae_dataname (`character`) adverse events dataset name,
#' needs to be available in the list passed to the `data`
#' argument of [teal::init()] \cr
#' specify to `NA` if no concomitant medications data is available
#' @param id_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) unique subject ID variable
#' @param visit_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) analysis visit variable
#' @param ongo_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) study ongoing status variable.
#' This variable is a derived logical variable. Usually it can be derived from `EOSSTT`.
#' @param anno_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) annotation variable
#' @param heat_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) heatmap variable
#' @param cm_dataname (`character`) concomitant medications dataset name,
#' @param conmed_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) concomitant medications variable,
#' specify to `NA` if no concomitant medications data is available
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @details `data`` object is only used for checks
#'
#' @export
#' @examples
#' # Using picks method
#' data <- teal_data() %>%
#'   within({
#'     library(dplyr)
#'     library(nestcolor)
#'     ADSL <- teal.data::rADSL %>% slice(1:30)
#'     ADEX <- teal.data::rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADAE <- teal.data::rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
#'     ADCM <- teal.data::rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
#'     # This preprocess is only to force legacy standard on ADCM
#'     ADCM <- ADCM %>%
#'       select(-starts_with("ATC")) %>%
#'       unique()
#'     # function to derive AVISIT from ADEX
#'     .add_visit <- function(data_need_visit) {
#'       visit_dates <- ADEX %>%
#'         filter(PARAMCD == "DOSE") %>%
#'         distinct(USUBJID, AVISIT, ASTDTM) %>%
#'         group_by(USUBJID) %>%
#'         arrange(ASTDTM) %>%
#'         mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
#'         rename(this_vis = ASTDTM)
#'       data_visit <- data_need_visit %>%
#'         select(USUBJID, ASTDTM) %>%
#'         left_join(visit_dates, by = "USUBJID") %>%
#'         filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
#'         left_join(data_need_visit) %>%
#'         distinct()
#'       return(data_visit)
#'     }
#'     # derive AVISIT for ADAE and ADCM
#'     ADAE <- .add_visit(ADAE)
#'     ADCM <- .add_visit(ADCM)
#'     # derive ongoing status variable for ADEX
#'     ADEX <- ADEX %>%
#'       filter(PARCAT1 == "INDIVIDUAL") %>%
#'       mutate(ongo_status = (EOSSTT == "ONGOING"))
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_heat_bygrade(
#'       label = "Heatmap by grade",
#'       sl_dataname = "ADSL",
#'       ex_dataname = "ADEX",
#'       ae_dataname = "ADAE",
#'       cm_dataname = "ADCM",
#'       id_var = variables(
#'         choices = teal.picks::is_categorical(min.len = 2),
#'         selected = 1L
#'       ),
#'       visit_var = variables(
#'         choices = dplyr::starts_with("AVISIT"),
#'         selected = 1L
#'       ),
#'       ongo_var = variables(
#'         choices = dplyr::starts_with("ongo"),
#'         selected = 1L
#'       ),
#'       anno_var = variables(
#'         choices = teal.picks::is_categorical(min.len = 2),
#'         selected = c("SEX", "COUNTRY"),
#'         multiple = TRUE
#'       ),
#'       heat_var = variables(
#'         choices = dplyr::starts_with("AETO"),
#'         selected = 1L
#'       ),
#'       conmed_var = variables(
#'         choices = dplyr::starts_with("CMDECOD"),
#'         selected = 1L
#'       ),
#'       plot_height = c(600L, 200L, 2000L)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_heat_bygrade <- function(
  label,
  sl_dataname,
  ex_dataname,
  ae_dataname,
  id_var,
  visit_var,
  ongo_var,
  anno_var,
  heat_var,
  cm_dataname = NULL,
  conmed_var = NULL,
  fontsize = c(5, 3, 7),
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  transformators = list()
) {
  message("Initializing tm_g_heat_bygrade")

  checkmate::assert_string(label)

  id_var <- migrate_choices_selected_to_variables(id_var)
  visit_var <- migrate_choices_selected_to_variables(visit_var)
  ongo_var <- migrate_choices_selected_to_variables(ongo_var)
  anno_var <- migrate_choices_selected_to_variables(anno_var)
  heat_var <- migrate_choices_selected_to_variables(heat_var)
  conmed_var <- migrate_choices_selected_to_variables(conmed_var, null.ok = TRUE)

  id_var <- create_picks_helper(teal.picks::datasets(sl_dataname, sl_dataname), id_var)
  visit_var <- create_picks_helper(teal.picks::datasets(ex_dataname, ex_dataname), visit_var)
  ongo_var <- create_picks_helper(teal.picks::datasets(ex_dataname, ex_dataname), ongo_var)
  anno_var <- create_picks_helper(teal.picks::datasets(sl_dataname, sl_dataname), anno_var)
  heat_var <- create_picks_helper(teal.picks::datasets(ae_dataname, ae_dataname), heat_var)
  if (!is.null(conmed_var)) {
    conmed_var <- create_picks_helper(teal.picks::datasets(cm_dataname, cm_dataname), conmed_var)
  }

  id_var <- force_pick_selection(id_var, "id_var")
  visit_var <- force_pick_selection(visit_var, "visit_var")
  ongo_var <- force_pick_selection(ongo_var, "ongo_var")
  heat_var <- force_pick_selection(heat_var, "heat_var")
  if (!is.null(conmed_var)) {
    conmed_var <- force_pick_selection(conmed_var, "conmed_var")
  }
  anno_var <- force_pick_selection(anno_var, "anno_var", multiple = TRUE)

  checkmate::assert_class(id_var, "picks")
  checkmate::assert_class(visit_var, "picks")
  checkmate::assert_class(ongo_var, "picks")
  checkmate::assert_class(anno_var, "picks")
  checkmate::assert_class(heat_var, "picks")
  checkmate::assert_class(conmed_var, "picks", null.ok = TRUE)

  checkmate::assert(
    checkmate::check_number(fontsize, finite = TRUE),
    checkmate::assert(
      combine = "and",
      .var.name = "fontsize",
      checkmate::check_numeric(fontsize, len = 3, any.missing = FALSE, finite = TRUE),
      checkmate::check_numeric(fontsize[1], lower = fontsize[2], upper = fontsize[3])
    )
  )
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  args <- as.list(environment())
  module(
    label = label,
    server = srv_g_heat_by_grade,
    server_args = args[names(args) %in% names(formals(srv_g_heat_by_grade))],
    ui = ui_g_heat_by_grade,
    ui_args = args[names(args) %in% names(formals(ui_g_heat_by_grade))],
    transformators = transformators,
    datanames = "all"
  )
}

ui_g_heat_by_grade <- function(
  id,
  cm_dataname,
  id_var,
  visit_var,
  ongo_var,
  anno_var,
  heat_var,
  conmed_var,
  fontsize
) {
  ns <- NS(id)

  shiny::tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        plot_decorate_output(id = ns(NULL))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        tags$div(
          tags$strong("ID Variable"),
          teal.picks::picks_ui(id = ns("id_var"), picks = id_var)
        ),
        tags$div(
          tags$strong("Visit Variable"),
          teal.picks::picks_ui(id = ns("visit_var"), picks = visit_var)
        ),
        tags$div(
          tags$strong("Study Ongoing Status Variable"),
          teal.picks::picks_ui(id = ns("ongo_var"), picks = ongo_var)
        ),
        tags$div(
          tags$strong("Annotation Variables"),
          teal.picks::picks_ui(id = ns("anno_var"), picks = anno_var)
        ),
        tags$div(
          tags$strong("Heat Variable"),
          teal.picks::picks_ui(id = ns("heat_var"), picks = heat_var)
        ),
        helpText("Plot conmed"),
        left_bordered_div(
          if (!is.na(cm_dataname)) {
            checkboxInput(
              ns("plot_cm"),
              "Yes",
              value = !is.na(cm_dataname)
            )
          }
        ),
        conditionalPanel(
          paste0("input['", ns("plot_cm"), "']"),
          if (!is.null(conmed_var)) {
            tags$div(
              tags$strong("Conmed Variable"),
              teal.picks::picks_ui(id = ns("conmed_var"), picks = conmed_var)
            )
          },
          selectInput(
            ns("conmed_level"),
            "Conmed Levels",
            choices = character(0),
            selected = character(0),
            multiple = TRUE
          )
        ),
        ui_g_decorate(
          ns(NULL),
          fontsize = fontsize,
          titles = "Heatmap by Grade",
          footnotes = ""
        )
      )
    )
  )
}

srv_g_heat_by_grade <- function(
  id,
  data,
  sl_dataname,
  ex_dataname,
  ae_dataname,
  cm_dataname,
  id_var,
  visit_var,
  ongo_var,
  anno_var,
  heat_var,
  conmed_var,
  label,
  plot_height,
  plot_width
) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  names_data <- isolate(names(data()))

  checkmate::assert_choice(sl_dataname, names_data, null.ok = TRUE)
  checkmate::assert_choice(ex_dataname, names_data, null.ok = TRUE)
  checkmate::assert_choice(ae_dataname, names_data, null.ok = TRUE)
  checkmate::assert_choice(cm_dataname, names_data, null.ok = TRUE)


  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Build picks list (exclude NULL optional picks)
    picks_list <- list(
      id_var = id_var,
      visit_var = visit_var,
      ongo_var = ongo_var,
      anno_var = anno_var,
      heat_var = heat_var,
      conmed_var = conmed_var
    )

    selectors <- teal.picks::picks_srv(picks = picks_list, data = data)

    merged_sl <- teal.picks::merge_srv(
      "merge_sl",
      data = data,
      selectors = selectors[c("id_var", "anno_var")],
      output_name = "ADSL_ANL"
    )
    merged_ex <- teal.picks::merge_srv(
      "merge_ex",
      data = data,
      selectors = selectors[c("visit_var", "ongo_var")],
      output_name = "ADEX_ANL"
    )
    merged_ae <- teal.picks::merge_srv(
      "merge_ae",
      data = data,
      selectors = selectors["heat_var"],
      output_name = "ADAE_ANL"
    )
    if (!is.null(conmed_var)) {
      merged_cm <- teal.picks::merge_srv(
        "merge_cm",
        data = data,
        selectors = selectors["conmed_var"],
        output_name = "ADCM_ANL"
      )
    }

    decorate_output <- srv_g_decorate(
      id = NULL,
      plt = plot_r,
      plot_height = plot_height,
      plot_width = plot_width
    )
    font_size <- decorate_output$font_size
    pws <- decorate_output$pws

    if (!is.null(conmed_var)) {
      observeEvent(merged_cm$variables()$conmed_var,
        {
          ADCM <- data()[[cm_dataname]]
          conmed_var_name <- merged_cm$variables()$conmed_var
          if (!is.null(conmed_var_name) && conmed_var_name %in% names(ADCM)) {
            choices <- levels(ADCM[[conmed_var_name]])
            updateSelectInput(
              session,
              "conmed_level",
              selected = choices[seq_len(min(3L, length(choices)))],
              choices = choices
            )
          }
        },
        ignoreNULL = FALSE
      )
    }

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        qenv <- data()
        teal.reporter::teal_card(qenv) <-
          c(
            teal.reporter::teal_card(qenv),
            teal.reporter::teal_card("## Module's output(s)")
          )
        qenv <- teal.code::eval_code(qenv, "library(dplyr)")

        id_var_name <- merged_sl$variables()$id_var
        anno_var_name <- merged_sl$variables()$anno_var
        visit_var_name <- merged_ex$variables()$visit_var
        ongo_var_name <- merged_ex$variables()$ongo_var
        heat_var_name <- merged_ae$variables()$heat_var
        plot_cm <- isTRUE(input$plot_cm)
        conmed_var_name <- if (plot_cm) merged_cm$variables()$conmed_var else NULL

        validated_q <- qenv

        ADSL <- validated_q[[sl_dataname]]
        teal::validate_has_data(ADSL, min_nrow = 1, msg = sprintf("%s contains no data", sl_dataname))

        shiny::validate(
          teal::need_input("id_var-variables-selected", length(id_var_name) > 0, "ID Variable is required."),
          teal::need_input("visit_var-variables-selected", length(visit_var_name) > 0, "Visit Variable is required."),
          teal::need_input(
            "ongo_var-variables-selected",
            length(ongo_var_name) > 0, "Study Ongoing Status Variable is required."
          ),
          teal::need_input(
            "anno_var-variables-selected",
            length(anno_var_name) > 0, "Annotation Variables is required."
          ),
          teal::need_input("heat_var-variables-selected", length(heat_var_name) > 0, "Heat Variable is required.")
        )

        if (plot_cm) {
          shiny::validate(
            teal::need_input(
              "conmed_var-variables-selected",
              length(conmed_var_name) > 0, "Conmed Variable is required."
            ),
            teal::need_input("conmed_level", length(input$conmed_level) > 0, "Select Conmed Levels.")
          )
        }

        teal.reporter::teal_card(validated_q) <- c(teal.reporter::teal_card(validated_q), "### Plot")

        if (plot_cm) {
          validated_q <- teal.code::eval_code(
            validated_q,
            code = substitute(
              expr = {
                conmed_data <- ADCM %>%
                  filter(conmed_var_name %in% conmed_level)
                conmed_data[[conmed_var]] <-
                  factor(conmed_data[[conmed_var]], levels = unique(conmed_data[[conmed_var]]))
                formatters::var_labels(conmed_data)[conmed_var] <-
                  formatters::var_labels(ADCM, fill = FALSE)[conmed_var]
              },
              env = list(
                ADCM = as.name(cm_dataname),
                conmed_var = conmed_var_name,
                conmed_var_name = as.name(conmed_var_name),
                conmed_level = input$conmed_level
              )
            )
          )
        }

        conmed_data <- if (plot_cm) validated_q[["conmed_data"]] else NULL

        PARCAT1 <- NULL # nolint: object_name_linter.

        validated_q <- within(
          validated_q,
          {
            plot <- osprey::g_heat_bygrade(
              id_var = id_var_name,
              exp_data = get(ex_dataname) %>% filter(PARCAT1 == "INDIVIDUAL"),
              visit_var = visit_var_name,
              ongo_var = ongo_var_name,
              anno_data = get(sl_dataname)[c(anno_var_name, id_var_name)],
              anno_var = anno_var_name,
              heat_data = get(ae_dataname) %>%
                select(all_of(c(id_var_name, visit_var_name, heat_var_name))),
              heat_color_var = heat_var_name,
              conmed_data = conmed_data,
              conmed_var = conmed_var_name
            )
          },
          id_var_name = id_var_name,
          ex_dataname = ex_dataname,
          visit_var_name = visit_var_name,
          ongo_var_name = ongo_var_name,
          sl_dataname = sl_dataname,
          anno_var_name = anno_var_name,
          ae_dataname = ae_dataname,
          heat_var_name = heat_var_name,
          conmed_data = conmed_data,
          conmed_var_name = conmed_var_name
        )

        validated_q
      })
    )

    plot_r <- reactive(output_q()[["plot"]])
    set_chunk_dims(pws, output_q)
  })
}
