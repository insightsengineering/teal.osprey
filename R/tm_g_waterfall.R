#' Teal Module for Waterfall Plot
#'
#' @description
#'
#' This is a teal module that generates a waterfall plot for `ADaM` data using
#' [teal.picks::picks()] encodings.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param dataname_tr (`character(1)`)\cr
#'   Tumor burden dataset name (e.g. `"ADTR"`).
#' @param dataname_rs (`character(1)`)\cr
#'   Response dataset name (e.g. `"ADRS"`).
#' @param bar_paramcd (`picks`)\cr
#'   `PARAMCD` selection for tumor burden data (`values` slot).
#' @param bar_var (`picks`)\cr
#'   Numeric variable for bar height (e.g. `PCHG`).
#' @param bar_color_var (`picks` or `NULL`)\cr
#'   Subject-level color variable from `ADSL`.
#' @param bar_color_opt (`character`)\cr
#'   Named vector mapping color values to colors.
#' @param sort_var (`picks` or `NULL`)\cr
#'   Subject-level sort variable from `ADSL`.
#' @param add_label_var_sl (`picks` or `NULL`)\cr
#'   Subject-level bar label variable from `ADSL`.
#' @param add_label_paramcd_rs (`picks` or `NULL`)\cr
#'   `PARAMCD` label from response data (`ADRS`).
#' @param anno_txt_var_sl (`picks` or `NULL`)\cr
#'   Subject-level annotation variables from `ADSL`.
#' @param anno_txt_paramcd_rs (`picks` or `NULL`)\cr
#'   `PARAMCD` annotation parameters from `ADRS`.
#' @param facet_var (`picks` or `NULL`)\cr
#'   Subject-level facet variable from `ADSL`.
#' @param ytick_at (`numeric`)\cr
#'   Bar height axis interval.
#' @param href_line (`character`)\cr
#'   Comma-separated horizontal reference lines.
#' @param gap_point_val (`character`)\cr
#'   Value for breaking high bars.
#' @param show_value (`logical`)\cr
#'   Whether to show bar height values.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @template author_qit3
#' @author houx14 \email{houx14@gene.com}
#'
tm_g_waterfall <- function(label,
                                 dataname_tr = "ADTR",
                                 dataname_rs = "ADRS",
                                 bar_paramcd,
                                 bar_var,
                                 bar_color_var = NULL,
                                 bar_color_opt = NULL,
                                 sort_var = NULL,
                                 add_label_var_sl = NULL,
                                 add_label_paramcd_rs = NULL,
                                 anno_txt_var_sl = NULL,
                                 anno_txt_paramcd_rs = NULL,
                                 facet_var = NULL,
                                 ytick_at = 20,
                                 href_line = NULL,
                                 gap_point_val = NULL,
                                 show_value = TRUE,
                                 plot_height = c(1200L, 400L, 5000L),
                                 plot_width = NULL,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 transformators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_string(dataname_tr)
  checkmate::assert_string(dataname_rs)
  checkmate::assert_class(bar_paramcd, "picks", .var.name = "bar_paramcd")
  .assert_picks_single_var(bar_var, "bar_var")
  if (!is.null(bar_color_var)) .assert_picks_single_var(bar_color_var, "bar_color_var")
  if (!is.null(sort_var)) .assert_picks_single_var(sort_var, "sort_var")
  if (!is.null(add_label_var_sl)) .assert_picks_single_var(add_label_var_sl, "add_label_var_sl")
  if (!is.null(facet_var)) .assert_picks_single_var(facet_var, "facet_var")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(
    plot_height[1],
    lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height"
  )
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  pick_slots <- Filter(
    Negate(is.null),
    list(
      bar_paramcd = bar_paramcd,
      bar_var = bar_var,
      bar_color_var = bar_color_var,
      sort_var = sort_var,
      add_label_var_sl = add_label_var_sl,
      add_label_paramcd_rs = add_label_paramcd_rs,
      anno_txt_var_sl = anno_txt_var_sl,
      anno_txt_paramcd_rs = anno_txt_paramcd_rs,
      facet_var = facet_var
    )
  )

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_waterfall,
    server = srv_g_waterfall,
    ui_args = args[names(args) %in% names(formals(ui_g_waterfall))],
    server_args = args[names(args) %in% names(formals(srv_g_waterfall))],
    transformators = transformators,
    datanames = unique(c("ADSL", dataname_tr, dataname_rs, .picks_all_datanames(pick_slots)))
  )
}

#' @keywords internal
ui_g_waterfall <- function(id,
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
                                  ytick_at,
                                  href_line,
                                  gap_point_val,
                                  show_value,
                                  pre_output,
                                  post_output) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("waterfallplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(dataname_tr), tags$code(dataname_rs)),
      tags$div(
        tags$label("Tumor burden parameter"),
        teal.picks::picks_ui(ns("bar_paramcd"), bar_paramcd)
      ),
      tags$div(
        tags$label("Bar height"),
        teal.picks::picks_ui(ns("bar_var"), bar_var)
      ),
      if (!is.null(bar_color_var)) {
        tags$div(
          tags$label("Bar color"),
          teal.picks::picks_ui(ns("bar_color_var"), bar_color_var)
        )
      },
      if (!is.null(sort_var)) {
        tags$div(
          tags$label("Sort by"),
          teal.picks::picks_ui(ns("sort_var"), sort_var)
        )
      },
      if (!is.null(add_label_var_sl)) {
        tags$div(
          tags$label("Add ADSL label to bars"),
          teal.picks::picks_ui(ns("add_label_var_sl"), add_label_var_sl)
        )
      },
      if (!is.null(add_label_paramcd_rs)) {
        tags$div(
          tags$label("Add ADRS label to bars"),
          teal.picks::picks_ui(ns("add_label_paramcd_rs"), add_label_paramcd_rs)
        )
      },
      if (!is.null(anno_txt_var_sl)) {
        tags$div(
          tags$label("Annotation variables (ADSL)"),
          teal.picks::picks_ui(ns("anno_txt_var_sl"), anno_txt_var_sl)
        )
      },
      if (!is.null(anno_txt_paramcd_rs)) {
        tags$div(
          tags$label("Annotation parameters (ADRS)"),
          teal.picks::picks_ui(ns("anno_txt_paramcd_rs"), anno_txt_paramcd_rs)
        )
      },
      if (!is.null(facet_var)) {
        tags$div(
          tags$label("Facet by"),
          teal.picks::picks_ui(ns("facet_var"), facet_var)
        )
      },
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

#' @keywords internal
.waterfall_picks_selected_var <- function(selector_state) {
  if (is.null(selector_state) || is.null(selector_state$variables)) {
    return(character())
  }
  as.character(selector_state$variables$selected)
}

#' @keywords internal
.waterfall_picks_selected_values <- function(selector_state) {
  if (is.null(selector_state) || is.null(selector_state$values)) {
    return(character())
  }
  as.character(selector_state$values$selected)
}

#' @keywords internal
srv_g_waterfall <- function(id,
                                  data,
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
                                  bar_color_opt,
                                  plot_height,
                                  plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    picks_inputs <- Filter(
      Negate(is.null),
      list(
        bar_paramcd = bar_paramcd,
        bar_var = bar_var,
        bar_color_var = bar_color_var,
        sort_var = sort_var,
        add_label_var_sl = add_label_var_sl,
        add_label_paramcd_rs = add_label_paramcd_rs,
        anno_txt_var_sl = anno_txt_var_sl,
        anno_txt_paramcd_rs = anno_txt_paramcd_rs,
        facet_var = facet_var
      )
    )

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = picks_inputs,
      data = data
    )

    output_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      obj <- teal.code::eval_code(obj, "library(dplyr)")

      adsl <- obj[["ADSL"]]
      adtr <- obj[[dataname_tr]]
      adrs <- obj[[dataname_rs]]

      teal::validate_has_data(adsl, min_nrow = 2)
      teal::validate_has_data(adtr, min_nrow = 2)
      teal::validate_has_data(adrs, min_nrow = 2)

      bar_var_name <- .waterfall_picks_selected_var(selectors$bar_var())
      bar_paramcd_val <- .waterfall_picks_selected_values(selectors$bar_paramcd())

      validate(
        need(length(bar_var_name) > 0L, "Please select a bar height variable."),
        need(length(bar_paramcd_val) > 0L, "Please select a tumor burden parameter.")
      )

      bar_color_var_name <- if (!is.null(bar_color_var)) {
        .waterfall_picks_selected_var(selectors$bar_color_var())
      } else {
        character()
      }
      sort_var_name <- if (!is.null(sort_var)) {
        .waterfall_picks_selected_var(selectors$sort_var())
      } else {
        character()
      }
      add_label_var_sl_name <- if (!is.null(add_label_var_sl)) {
        .waterfall_picks_selected_var(selectors$add_label_var_sl())
      } else {
        character()
      }
      add_label_paramcd_rs_val <- if (!is.null(add_label_paramcd_rs)) {
        .waterfall_picks_selected_values(selectors$add_label_paramcd_rs())
      } else {
        character()
      }
      anno_txt_var_sl_name <- if (!is.null(anno_txt_var_sl)) {
        .waterfall_picks_selected_var(selectors$anno_txt_var_sl())
      } else {
        character()
      }
      anno_txt_paramcd_rs_val <- if (!is.null(anno_txt_paramcd_rs)) {
        .waterfall_picks_selected_values(selectors$anno_txt_paramcd_rs())
      } else {
        character()
      }
      facet_var_name <- if (!is.null(facet_var)) {
        .waterfall_picks_selected_var(selectors$facet_var())
      } else {
        character()
      }

      bar_paramcd_one <- bar_paramcd_val[[1L]]
      validate(
        need(
          bar_paramcd_one %in% adtr$PARAMCD,
          "Tumor burden parameter must be an element of ADTR PARAMCD."
        )
      )
      if (length(add_label_paramcd_rs_val) > 0L) {
        validate(need(
          all(add_label_paramcd_rs_val %in% adrs$PARAMCD),
          "ADRS label must be an element of ADRS PARAMCD."
        ))
      }
      if (length(add_label_var_sl_name) > 0L && length(add_label_paramcd_rs_val) > 0L) {
        validate(need(FALSE, "Only one \"Label to Bars\" is allowed."))
      }
      if (length(anno_txt_paramcd_rs_val) > 0L) {
        validate(need(
          all(anno_txt_paramcd_rs_val %in% adrs$PARAMCD),
          "Annotation parameters must be elements of ADRS PARAMCD."
        ))
      }

      adsl_vars <- unique(c(
        "USUBJID", "STUDYID",
        bar_color_var_name, sort_var_name, add_label_var_sl_name, anno_txt_var_sl_name, facet_var_name
      ))
      adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", bar_var_name))
      adrs_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", "AVALC"))
      adrs_paramcd <- unique(c(add_label_paramcd_rs_val, anno_txt_paramcd_rs_val))

      teal::validate_has_variable(adsl, adsl_vars)
      teal::validate_has_variable(adrs, adrs_vars)
      teal::validate_has_variable(adtr, adtr_vars)

      href_line <- suppressWarnings(as_numeric_from_comma_sep_str(input$href_line))
      gap_point_val <- input$gap_point_val
      ytick_at <- input$ytick_at
      show_value <- input$show_value

      validate(
        need(
          !is.na(suppressWarnings(as.numeric(ytick_at))) &&
            checkmate::test_number(suppressWarnings(as.numeric(ytick_at)), lower = 1),
          "Y-axis Interval must be a single positive number."
        )
      )
      if (!is.null(gap_point_val) && nzchar(gap_point_val)) {
        validate(need(
          checkmate::test_number(suppressWarnings(as.numeric(gap_point_val)), lower = 1),
          "Break High Bars must be a single positive number."
        ))
      }
      if (!is.null(href_line) && anyNA(href_line)) {
        validate(need(FALSE, "Horizontal Reference Line(s) are invalid."))
      }

      if (gap_point_val == "" || is.null(gap_point_val)) {
        gap_point_val <- NULL
      } else {
        gap_point_val <- as.numeric(gap_point_val)
      }
      ytick_at <- as.numeric(ytick_at)

      bar_color_var <- if (length(bar_color_var_name) > 0L) bar_color_var_name else NULL
      sort_var <- if (length(sort_var_name) > 0L) sort_var_name else NULL
      facet_var <- if (length(facet_var_name) > 0L) facet_var_name else NULL
      add_label_var_sl <- if (length(add_label_var_sl_name) > 0L) add_label_var_sl_name else NULL
      add_label_paramcd_rs <- if (length(add_label_paramcd_rs_val) > 0L) add_label_paramcd_rs_val else NULL
      anno_txt_var_sl <- if (length(anno_txt_var_sl_name) > 0L) anno_txt_var_sl_name else NULL
      anno_txt_paramcd_rs <- if (length(anno_txt_paramcd_rs_val) > 0L) anno_txt_paramcd_rs_val else NULL

      q1 <- teal.code::eval_code(
        obj,
        code = bquote({
          bar_var <- .(bar_var_name)
          bar_color_var <- .(bar_color_var)
          sort_var <- .(sort_var)
          add_label_var_sl <- .(add_label_var_sl)
          add_label_paramcd_rs <- .(add_label_paramcd_rs)
          anno_txt_var_sl <- .(anno_txt_var_sl)
          anno_txt_paramcd_rs <- .(anno_txt_paramcd_rs)
          facet_var <- .(facet_var)
          href_line <- .(href_line)
          gap_point_val <- .(gap_point_val)
          show_value <- .(show_value)
        })
      )

      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          adsl <- ADSL[, .(adsl_vars)]
          adtr <- .(as.name(dataname_tr))[, .(adtr_vars)]
          adrs <- .(as.name(dataname_rs))[, .(adrs_vars)]

          bar_tr <- .(as.name(dataname_tr)) %>%
            dplyr::filter(PARAMCD == .(bar_paramcd_one)) %>%
            dplyr::select(USUBJID, .(as.name(bar_var))) %>%
            dplyr::group_by(USUBJID) %>%
            dplyr::slice(which.min(.(as.name(bar_var))))
          bar_data <- adsl %>% dplyr::inner_join(bar_tr, "USUBJID")
        })
      )

      q1 <- if (length(adrs_paramcd) == 0L) {
        teal.code::eval_code(
          q1,
          code = bquote({
            anl <- bar_data
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1))
          })
        )
      } else {
        qq1 <- teal.code::eval_code(
          q1,
          code = bquote(
            rs_sub <- .(as.name(dataname_rs)) %>%
              dplyr::filter(PARAMCD %in% .(adrs_paramcd))
          )
        )

        teal::validate_one_row_per_id(qq1[["rs_sub"]], key = c("STUDYID", "USUBJID", "PARAMCD"))

        teal.code::eval_code(
          qq1,
          code = bquote({
            rs_label <- rs_sub %>%
              dplyr::select(USUBJID, PARAMCD, AVALC) %>%
              tidyr::pivot_wider(names_from = PARAMCD, values_from = AVALC)
            anl <- bar_data %>% dplyr::left_join(rs_label, by = c("USUBJID"))
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1))
          })
        )
      }

      teal.reporter::teal_card(q1) <-
        c(
          teal.reporter::teal_card(q1),
          "### Selected Options",
          paste0("Tumor Burden Parameter: ", bar_paramcd_one, ".")
        )

      if (!is.null(facet_var)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          paste0("Faceted by: ", paste(facet_var, collapse = ", "), ".")
        )
      }
      if (!is.null(sort_var)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          paste0("Sorted by: ", paste(sort_var, collapse = ", "), ".")
        )
      }

      teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

      teal.code::eval_code(
        q1,
        code = bquote({
          plot <- osprey::g_waterfall(
            bar_id = anl[["USUBJID"]],
            bar_height = anl[[bar_var]],
            sort_by = .(if (length(sort_var) > 0) quote(anl[[sort_var]]) else NULL),
            col_by = .(if (length(bar_color_var) > 0) quote(anl[[bar_color_var]]) else NULL),
            bar_color_opt = .(if (length(bar_color_var) == 0) {
              NULL
            } else if (length(bar_color_var) > 0 & all(unique(anl[[bar_color_var]]) %in% names(bar_color_opt))) {
              bar_color_opt
            } else {
              NULL
            }),
            anno_txt = .(if (length(anno_txt_var_sl) == 0 & length(anno_txt_paramcd_rs) == 0) {
              NULL
            } else if (length(anno_txt_var_sl) >= 1 & length(anno_txt_paramcd_rs) == 0) {
              quote(data.frame(anl[anno_txt_var_sl]))
            } else if (length(anno_txt_paramcd_rs) >= 1 & length(anno_txt_var_sl) == 0) {
              quote(data.frame(anl[anno_txt_paramcd_rs]))
            } else {
              quote(cbind(anl[anno_txt_var_sl], anl[anno_txt_paramcd_rs]))
            }),
            href_line = .(href_line),
            facet_by = .(if (length(facet_var) > 0) quote(as.factor(anl[[facet_var]])) else NULL),
            show_datavalue = .(show_value),
            add_label = .(if (length(add_label_var_sl) > 0 & length(add_label_paramcd_rs) == 0) {
              quote(anl[[add_label_var_sl]])
            } else if (length(add_label_paramcd_rs) > 0 & length(add_label_var_sl) == 0) {
              quote(anl[[add_label_paramcd_rs]])
            } else {
              NULL
            }),
            gap_point = .(gap_point_val),
            ytick_at = .(ytick_at),
            y_label = "Tumor Burden Change from Baseline",
            title = "Waterfall Plot"
          )
        })
      )
    })

    plot_r <- reactive(output_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "waterfallplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, output_q)
  })
}
