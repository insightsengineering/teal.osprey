#' Teal Module for Waterfall Plot
#'
#' @description
#'
#' This is teal module that generates a waterfall plot for `ADaM` data
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param parentname (`character(1)`) parent analysis data used in teal module, usually this refers to `ADSL`,
#' which is the default.
#' @param dataname_tr (`character(1)`) tumor burden analysis data used in teal module to plot as bar height, needs to
#' be available in the list passed to the `data` argument of [teal::init()]
#' @param dataname_rs (`character(1)`) response analysis data used in teal module to label response parameters, needs to
#' be available in the list passed to the `data` argument of [teal::init()]
#' @param bar_paramcd ([`teal.picks::values`] or `choices_selected`)
#' parameter in tumor burden data that will be plotted as bar height.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::values()] instead.
#' @param bar_var ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' numeric variable from dataset to plot the bar height, e.g., `PCHG`.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param bar_color_var ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' color by variable (subject level). Defaults to no selection.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param bar_color_opt aesthetic values to map color values (named vector to map color values to each name).
#' If not `NULL`, please make sure this contains all possible values for `bar_color_var` values,
#' otherwise color will be assigned by `ggplot` default, please note that `NULL` needs to be specified
#' in this case
#' @param sort_var ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' sort by variable (subject level). Defaults to no selection.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param add_label_var_sl ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' add label to bars (subject level).
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param add_label_paramcd_rs ([`teal.picks::values`] or [`teal.transform::choices_selected`])
#' add label to bars (response dataset).
#' At least one of `add_label_var_sl` and `add_label_paramcd_rs` needs to not be selected.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::values()] instead.
#' @param anno_txt_var_sl ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' subject level variables to be displayed in the annotation table, default is no selection.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param anno_txt_paramcd_rs ([`teal.picks::values`] or [`teal.transform::choices_selected`])
#' analysis dataset variables to be displayed in the annotation table, default is no selection.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::values()] instead.
#' @param facet_var ([`teal.picks::variables`] or [`teal.transform::choices_selected`])
#' facet by variable (subject level). Defaults to no selection.
#' `choices_selected` is being deprecated as an argument type and will be removed in the future.
#' Please use [teal.picks::variables()] instead.
#' @param ytick_at (`numeric(1)`) bar height axis interval, default is 20
#' @param href_line (`numeric`) numeric vector to plot horizontal reference lines, default is `NULL`
#' @param gap_point_val (`numeric(1)`) singular numeric value for adding bar break when some bars
#' are significantly higher than others, default is `NULL`
#' @param show_value (`logical(1)`) boolean of whether value of bar height is shown, default is `TRUE`
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @template author_qit3
#' @author houx14 \email{houx14@gene.com}
#'
#' @examples
#' data <- teal_data() %>%
#'   within({
#'     library(nestcolor)
#'     ADSL <- rADSL
#'     ADRS <- rADRS
#'     ADTR <- rADTR
#'     ADSL$SEX <- factor(ADSL$SEX, levels = unique(ADSL$SEX))
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_waterfall(
#'       label = "Waterfall",
#'       dataname_tr = "ADTR",
#'       dataname_rs = "ADRS",
#'       bar_paramcd = values(c("SLDINV"), "SLDINV", multiple = FALSE),
#'       bar_var = variables(c("PCHG", "AVAL"), "PCHG"),
#'       bar_color_var = variables(c("ARMCD", "SEX"), "ARMCD"),
#'       bar_color_opt = NULL,
#'       sort_var = variables(c("ARMCD", "SEX"), NULL),
#'       add_label_var_sl = variables(c("SEX", "EOSDY"), NULL),
#'       add_label_paramcd_rs = values(c("BESRSPI", "OBJRSPI"), NULL, multiple = FALSE),
#'       anno_txt_var_sl = variables(c("SEX", "ARMCD", "BMK1", "BMK2"), NULL, multiple = TRUE),
#'       anno_txt_paramcd_rs = values(c("BESRSPI", "OBJRSPI"), NULL),
#'       facet_var = variables(c("SEX", "ARMCD", "STRATA1", "STRATA2"), NULL),
#'       href_line = "-30, 20"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_waterfall <- function(label,
                           parentname = "ADSL",
                           dataname_tr = "ADTR",
                           dataname_rs = "ADRS",
                           bar_paramcd = teal.picks::values(choices = teal.picks::is_categorical(), multiple = FALSE),
                           bar_var = teal.picks::variables(choices = is.numeric, multiple = FALSE),
                           bar_color_var = teal.picks::variables(
                             choices = teal.picks::is_categorical(max.len = 20), selected = NULL
                           ),
                           bar_color_opt = NULL,
                           sort_var = teal.picks::variables(selected = NULL),
                           add_label_var_sl = teal.picks::variables(selected = NULL),
                           add_label_paramcd_rs = teal.picks::values(selected = NULL, multiple = FALSE),
                           anno_txt_var_sl = teal.picks::variables(selected = NULL, multiple = TRUE),
                           anno_txt_paramcd_rs = teal.picks::values(selected = NULL),
                           facet_var = teal.picks::variables(selected = NULL),
                           ytick_at = 20,
                           href_line = NULL,
                           gap_point_val = NULL,
                           show_value = TRUE,
                           plot_height = c(1200L, 400L, 5000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL,
                           transformators = list()) {
  message("Initializing tm_g_waterfall")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname_tr)
  checkmate::assert_string(dataname_rs)

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

  checkmate::assert_multi_class(bar_paramcd, classes = c("picks", "values", "choices_selected"))
  checkmate::assert_multi_class(add_label_paramcd_rs, classes = c("picks", "values", "choices_selected"))
  checkmate::assert_multi_class(anno_txt_paramcd_rs, classes = c("picks", "values", "choices_selected"))

  checkmate::assert_multi_class(bar_var, classes = c("picks", "variables", "choices_selected"))
  checkmate::assert_multi_class(bar_color_var, classes = c("picks", "variables", "choices_selected"))
  checkmate::assert_multi_class(sort_var, classes = c("picks", "variables", "choices_selected"))
  checkmate::assert_multi_class(add_label_var_sl, classes = c("picks", "variables", "choices_selected"))
  checkmate::assert_multi_class(anno_txt_var_sl, classes = c("picks", "variables", "choices_selected"))

  bar_paramcd <- migrate_value_choices_to_picks(bar_paramcd, default_variable_name = "PARAMCD")
  bar_var <- migrate_choices_selected_to_variables(bar_var)
  bar_color_var <- migrate_choices_selected_to_variables(bar_color_var)
  sort_var <- migrate_choices_selected_to_variables(sort_var)
  anno_txt_var_sl <- migrate_choices_selected_to_variables(anno_txt_var_sl)
  anno_txt_paramcd_rs <- migrate_value_choices_to_picks(anno_txt_paramcd_rs, default_variable_name = "PARAMCD")
  facet_var <- migrate_choices_selected_to_variables(facet_var)
  add_label_var_sl <- migrate_choices_selected_to_variables(add_label_var_sl)
  add_label_paramcd_rs <- migrate_value_choices_to_picks(add_label_paramcd_rs, default_variable_name = "PARAMCD")

  # from ADSL
  bar_color_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), bar_color_var)
  sort_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), sort_var)
  anno_txt_var_sl <- create_picks_helper(teal.picks::datasets(parentname, parentname), anno_txt_var_sl)
  facet_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), facet_var)
  add_label_var_sl <- create_picks_helper(teal.picks::datasets(parentname, parentname), add_label_var_sl)

  # from ADTR
  bar_var <- create_picks_helper(teal.picks::datasets(dataname_tr, dataname_tr), bar_var)

  # from ADRS
  bar_paramcd <- create_picks_helper(teal.picks::datasets(dataname_tr, dataname_tr), bar_paramcd)
  anno_txt_paramcd_rs <- create_picks_helper(teal.picks::datasets(dataname_rs, dataname_rs), anno_txt_paramcd_rs)
  add_label_paramcd_rs <- create_picks_helper(teal.picks::datasets(dataname_rs, dataname_rs), add_label_paramcd_rs)

  bar_paramcd <- force_pick_selection(bar_paramcd, which = "values", multiple = FALSE)
  bar_var <- force_pick_selection(bar_var, which = "variables", multiple = FALSE)
  bar_color_var <- force_pick_selection(bar_color_var, which = "variables", multiple = FALSE)
  sort_var <- force_pick_selection(sort_var, which = "variables", multiple = FALSE)
  add_label_var_sl <- force_pick_selection(add_label_var_sl, which = "variables", multiple = FALSE)
  add_label_paramcd_rs <- force_pick_selection(add_label_paramcd_rs, which = "values", multiple = FALSE)
  anno_txt_var_sl <- force_pick_selection(anno_txt_var_sl, which = "variables", multiple = TRUE)
  anno_txt_paramcd_rs <- force_pick_selection(anno_txt_paramcd_rs, which = "values", multiple = TRUE)
  facet_var <- force_pick_selection(facet_var, which = "variables", multiple = FALSE)

  attr(add_label_paramcd_rs$values, "allow-clear") <- TRUE

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_waterfall,
    ui_args = args[names(args) %in% names(formals(ui_g_waterfall))],
    server = srv_g_waterfall,
    server_args = args[names(args) %in% names(formals(srv_g_waterfall))],
    transformators = transformators,
    datanames = .picks_datanames(list(
      bar_paramcd,
      bar_var,
      bar_color_var,
      sort_var,
      add_label_var_sl,
      add_label_paramcd_rs,
      anno_txt_var_sl,
      anno_txt_paramcd_rs,
      facet_var
    ))
  )
}

ui_g_waterfall <- function(id,
                           parentname,
                           dataname_tr,
                           dataname_rs,
                           bar_paramcd,
                           bar_var,
                           bar_color_var,
                           sort_var,
                           add_label_var_sl,
                           add_label_paramcd_rs,
                           anno_txt_var_sl,
                           anno_txt_paramcd_rs,
                           facet_var,
                           show_value,
                           href_line,
                           ytick_at,
                           gap_point_val,
                           pre_output,
                           post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("waterfallplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      tags$div(
        tags$strong("Tumor Burden Parameter"),
        teal.picks::picks_ui(ns("bar_paramcd"), bar_paramcd)
      ),
      tags$div(
        tags$strong("Bar Height"),
        teal.picks::picks_ui(ns("bar_var"), bar_var),
        helpText("Tumor change variable from ", tags$code(dataname_tr))
      ),
      tags$div(
        tags$strong("Bar Color"),
        teal.picks::picks_ui(ns("bar_color_var"), bar_color_var)
      ),
      tags$div(
        tags$strong("Sort by"),
        teal.picks::picks_ui(ns("sort_var"), sort_var),
        helpText("from ", tags$code(parentname))
      ),
      tags$div(
        tags$strong("Add ADSL Label to Bars"),
        teal.picks::picks_ui(ns("add_label_var_sl"), add_label_var_sl)
      ),
      tags$div(
        tags$strong("Add ADRS Label to Bars"),
        teal.picks::picks_ui(ns("add_label_paramcd_rs"), add_label_paramcd_rs)
      ),
      tags$div(
        tags$strong("Annotation Variables"),
        teal.picks::picks_ui(ns("anno_txt_var_sl"), anno_txt_var_sl),
        helpText("from ", tags$code(parentname))
      ),
      tags$div(
        tags$strong("Annotation Parameters"),
        teal.picks::picks_ui(ns("anno_txt_paramcd_rs"), anno_txt_paramcd_rs),
        helpText("from ", tags$code(dataname_rs))
      ),
      tags$div(
        tags$strong("Facet by"),
        teal.picks::picks_ui(ns("facet_var"), facet_var),
        helpText("from ", tags$code(parentname))
      ),
      checkboxInput(
        ns("show_value"),
        "Add Bar Height Value",
        value = show_value
      ),
      textInput(
        ns("href_line"),
        label = tags$div(
          "Horizontal Reference Line(s)",
          tags$br(),
          helpText("Enter numeric value(s) of reference lines, separated by comma (eg. -10, 20)")
        ),
        value = href_line
      ),
      textInput(
        ns("ytick_at"),
        label = tags$div(
          "Y-axis Interval",
          tags$br(),
          helpText("Enter a numeric value of Y axis interval")
        ),
        value = ytick_at
      ),
      textInput(
        ns("gap_point_val"),
        label = tags$div(
          "Break High Bars",
          tags$br(),
          helpText("Enter a numeric value to break very high bars")
        ),
        value = gap_point_val
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_g_waterfall <- function(id,
                            data,
                            bar_paramcd,
                            bar_var,
                            bar_color_var,
                            sort_var,
                            anno_txt_var_sl,
                            anno_txt_paramcd_rs,
                            facet_var,
                            add_label_var_sl,
                            add_label_paramcd_rs,
                            parentname,
                            dataname_tr,
                            dataname_rs,
                            bar_color_opt,
                            label,
                            plot_height,
                            plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    selectors <- teal.picks::picks_srv("", picks = list(
      bar_paramcd = bar_paramcd,
      bar_var = bar_var,
      bar_color_var = bar_color_var,
      sort_var = sort_var,
      add_label_var_sl = add_label_var_sl,
      add_label_paramcd_rs = add_label_paramcd_rs,
      anno_txt_var_sl = anno_txt_var_sl,
      anno_txt_paramcd_rs = anno_txt_paramcd_rs,
      facet_var = facet_var
    ), data = data)

    validated_q <- reactive({
      adsl <- data()[[parentname]]
      adtr <- data()[[dataname_tr]]
      adrs <- data()[[dataname_rs]]

      validate(
        teal::need_input(
          "bar_var-variables-selected",
          length(selectors$bar_var()$variables$selected) > 0,
          "Bar height is required."
        ),
        teal::need_input(
          "bar_paramcd-values-selected",
          length(selectors$bar_paramcd()$values$selected) > 0,
          "Tumor Burden Parameter is required."
        ),
        teal::need_input(
          "bar_paramcd-values-selected",
          all(selectors$bar_paramcd()$values$selected %in% data()[[dataname_tr]]$PARAMCD),
          "All values of Tumor Burden Parameter must be elements of ADTR PARAMCD."
        ),
        teal::need_input(
          "add_label_paramcd_rs-values-selected",
          all(selectors$add_label_paramcd_rs()$values$selected %in% data()[[dataname_rs]]$PARAMCD),
          "ADRS Label must be an element of ADRS PARAMCD."
        ),
        teal::need_input(
          c("add_label_paramcd_rs-values-selected", "add_label_var_sl-variables-selected"),
          length(selectors$add_label_var_sl()$variables$selected) == 0 ||
            length(selectors$add_label_paramcd_rs()$values$selected) == 0,
          "Only one of 'Add ADSL Label to Bars' and 'Add ADRS Label to Bars' can be selected."
        ),
        teal::need_input(
          "anno_txt_paramcd_rs-values-selected",
          all(selectors$anno_txt_paramcd_rs()$values$selected %in% data()[[dataname_rs]]$PARAMCD),
          "Annotation Parameters must be elements of ADRS PARAMCD."
        ),
        teal::need_input(
          "href_line",
          all(!is.na(suppressWarnings(as_numeric_from_comma_sep_str(input$href_line)))),
          "Horizontal Reference Line(s) are invalid."
        ),
        teal::need_input(
          "ytick_at",
          length(input$ytick_at) > 0L,
          "Y-axis Interval is required."
        ),
        teal::need_input(
          "ytick_at",
          !is.na(suppressWarnings(as.numeric(input$ytick_at))) &&
            checkmate::test_number(suppressWarnings(as.numeric(input$ytick_at)), lower = 1),
          "Y-axis Interval must be a single positive number."
        ),
        teal::need_input(
          "gap_point_val",
          input$gap_point_val == "" || (
            !is.na(suppressWarnings(as.numeric(input$gap_point_val))) &&
              checkmate::test_number(suppressWarnings(as.numeric(input$gap_point_val)), lower = 1)
          ),
          "Break High Bars must be a single positive number."
        )
      )
      data()
    })

    output_q <- reactive({
      obj <- validated_q()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj <- teal.code::eval_code(obj, "library(dplyr)")

      adsl <- obj[[parentname]]
      adtr <- obj[[dataname_tr]]
      adrs <- obj[[dataname_rs]]

      # validate data rows
      teal::validate_has_data(adsl, min_nrow = 2)
      teal::validate_has_data(adtr, min_nrow = 2)
      teal::validate_has_data(adrs, min_nrow = 2)

      # get variables
      bar_var_selected <- selectors$bar_var()$variables$selected
      bar_paramcd_selected <- selectors$bar_paramcd()$values$selected
      add_label_var_sl_selected <- selectors$add_label_var_sl()$variables$selected
      add_label_paramcd_rs_selected <- selectors$add_label_paramcd_rs()$values$selected
      anno_txt_var_sl_selected <- selectors$anno_txt_var_sl()$variables$selected
      anno_txt_paramcd_rs_selected <- selectors$anno_txt_paramcd_rs()$values$selected
      bar_color_var_selected <- selectors$bar_color_var()$variables$selected
      sort_var_selected <- selectors$sort_var()$variables$selected
      facet_var_selected <- selectors$facet_var()$variables$selected

      ytick_at_selected <- input$ytick_at
      href_line_selected <- input$href_line
      gap_point_val_selected <- input$gap_point_val
      show_value_selected <- input$show_value
      href_line_selected <- suppressWarnings(as_numeric_from_comma_sep_str(href_line_selected))

      adsl_vars <- unique(
        c(
          "USUBJID", "STUDYID",
          bar_color_var_selected,
          sort_var_selected,
          add_label_var_sl_selected,
          anno_txt_var_sl_selected,
          facet_var_selected
        )
      )
      adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", bar_var_selected))
      adrs_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", "AVALC"))
      adrs_paramcd <- unique(c(add_label_paramcd_rs_selected, anno_txt_paramcd_rs_selected))

      # validate data input
      teal::validate_has_variable(adsl, adsl_vars)
      teal::validate_has_variable(adrs, adrs_vars)
      teal::validate_has_variable(adtr, adtr_vars)

      gap_point_val_selected <- if (gap_point_val_selected == "") {
        NULL
      } else {
        as.numeric(gap_point_val_selected)
      }
      ytick_at_selected <- as.numeric(ytick_at_selected)

      # write variables to qenv
      q1 <- teal.code::eval_code(obj, bquote({
        bar_var <- .(bar_var_selected)
        bar_color_var <- .(bar_color_var_selected)
        sort_var <- .(sort_var_selected)
        add_label_var_sl <- .(add_label_var_sl_selected)
        add_label_paramcd_rs <- .(add_label_paramcd_rs_selected)
        anno_txt_var_sl <- .(anno_txt_var_sl_selected)
        anno_txt_paramcd_rs <- .(anno_txt_paramcd_rs_selected)
        facet_var <- .(facet_var_selected)
        href_line <- .(href_line_selected)
        gap_point_val <- .(gap_point_val_selected)
        show_value <- .(show_value_selected)
      }))

      bar_data <- NULL # To avoid R CMD Check NOTES on global binding

      # data processing
      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          adsl <- .(as.name(parentname))[, .(adsl_vars)]
          adtr <- .(as.name(dataname_tr))[, .(adtr_vars)]
          adrs <- .(as.name(dataname_rs))[, .(adrs_vars)]

          bar_tr <- .(as.name(dataname_tr)) %>%
            dplyr::filter(PARAMCD == .(bar_paramcd_selected)) %>%
            dplyr::select(USUBJID, .(as.name(bar_var_selected))) %>%
            dplyr::group_by(USUBJID) %>%
            dplyr::slice(which.min(.(as.name(bar_var_selected))))
          bar_data <- adsl %>% dplyr::inner_join(bar_tr, "USUBJID")
        })
      )

      rs_sub <- NULL # To avoid no visible binding R CMD Check NOTE

      q1 <- if (is.null(adrs_paramcd)) {
        teal.code::eval_code(
          q1,
          code = bquote({
            anl <- bar_data
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), utils::tail, 1))
          })
        )
      } else {
        q_temp <- teal.code::eval_code(
          q1,
          code = bquote(
            rs_sub <- .(as.name(dataname_rs)) %>%
              dplyr::filter(PARAMCD %in% .(adrs_paramcd))
          )
        )

        teal::validate_one_row_per_id(q_temp[["rs_sub"]], key = c("STUDYID", "USUBJID", "PARAMCD"))

        USUBJID <- PARAMCD <- AVALC <- NULL # To avoid no visible binding R CMD NOTE

        within(q_temp, {
          rs_label <- rs_sub %>%
            dplyr::select(USUBJID, PARAMCD, AVALC) %>%
            tidyr::pivot_wider(names_from = PARAMCD, values_from = AVALC)
          anl <- bar_data %>% dplyr::left_join(rs_label, by = c("USUBJID"))
          anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), utils::tail, 1))
        })
      }

      # write plotting code to qenv
      anl <- q1[["anl"]]

      teal.reporter::teal_card(q1) <-
        c(
          teal.reporter::teal_card(q1),
          "### Selected Options",
          paste0("Tumor Burden Parameter: ", bar_paramcd_selected, ".")
        )

      if (!is.null(facet_var_selected)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          paste0("Faceted by: ", paste(facet_var_selected, collapse = ", "), ".")
        )
      }
      if (!is.null(sort_var_selected)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          paste0("Sorted by: ", paste(sort_var_selected, collapse = ", "), ".")
        )
      }

      teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          plot <- osprey::g_waterfall(
            bar_id = anl[["USUBJID"]],
            bar_height = anl[[bar_var]],
            sort_by = .(if (length(sort_var_selected) > 0) {
              quote(anl[[sort_var]])
            } else {
              NULL
            }),
            col_by = .(if (length(bar_color_var_selected) > 0) {
              quote(anl[[bar_color_var]])
            } else {
              NULL
            }),
            bar_color_opt = .(if (length(bar_color_var_selected) == 0) {
              NULL
            } else if (
              length(bar_color_var_selected) > 0 &&
                all(unique(anl[[bar_color_var_selected]]) %in% names(bar_color_opt))
            ) {
              bar_color_opt
            } else {
              NULL
            }),
            anno_txt = .(if (length(anno_txt_var_sl_selected) == 0 && length(anno_txt_paramcd_rs_selected) == 0) {
              NULL
            } else if (length(anno_txt_var_sl_selected) >= 1 & length(anno_txt_paramcd_rs_selected) == 0) {
              quote(data.frame(anl[anno_txt_var_sl]))
            } else if (length(anno_txt_paramcd_rs_selected) >= 1 & length(anno_txt_var_sl_selected) == 0) {
              quote(data.frame(anl[anno_txt_paramcd_rs]))
            } else {
              quote(cbind(anl[anno_txt_var_sl], anl[anno_txt_paramcd_rs]))
            }),
            href_line = .(href_line_selected),
            facet_by = .(if (length(facet_var_selected) > 0) {
              quote(as.factor(anl[[facet_var]]))
            } else {
              NULL
            }),
            show_datavalue = .(show_value_selected),
            add_label = .(if (length(add_label_var_sl_selected) > 0 & length(add_label_paramcd_rs_selected) == 0) {
              quote(anl[[add_label_var_sl]])
            } else if (length(add_label_paramcd_rs_selected) > 0 & length(add_label_var_sl_selected) == 0) {
              quote(anl[[add_label_paramcd_rs]])
            } else {
              NULL
            }),
            gap_point = .(gap_point_val_selected),
            ytick_at = .(ytick_at_selected),
            y_label = "Tumor Burden Change from Baseline",
            title = "Waterfall Plot"
          )
        })
      )
    })

    plot_r <- reactive(output_q()[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "waterfallplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, output_q)
  })
}
