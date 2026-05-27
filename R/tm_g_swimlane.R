#' Teal Module for `Swimlane` Plot
#'
#' @description
#'
#' This is a teal module that generates a `swimlane` plot (bar plot with markers) for `ADaM` data
#' using [teal.picks::picks()] encodings.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param dataname (`character(1)`)\cr
#'   Analysis data used for markers. Use `"ADSL"` when no markers are plotted.
#' @param bar_var (`picks`)\cr
#'   Subject-level numeric variable for bar length (from `ADSL`).
#' @param bar_color_var (`picks` or `NULL`)\cr
#'   Subject-level color variable from `ADSL`.
#' @param sort_var (`picks` or `NULL`)\cr
#'   Subject-level sort variable from `ADSL`.
#' @param marker_pos_var (`picks` or `NULL`)\cr
#'   Marker position variable from `dataname` (when not `"ADSL"`).
#' @param marker_shape_var (`picks` or `NULL`)\cr
#'   Marker shape variable from `dataname`.
#' @param marker_shape_opt (`numeric`)\cr
#'   Named vector mapping shape values to ggplot shapes.
#' @param marker_color_var (`picks` or `NULL`)\cr
#'   Marker color variable from `dataname`.
#' @param marker_color_opt (`character`)\cr
#'   Named vector mapping color values to colors.
#' @param anno_txt_var (`picks` or `NULL`)\cr
#'   Subject-level annotation variables from `ADSL` (multiple selection allowed).
#' @param vref_line (`numeric`)\cr
#'   Vertical reference lines.
#' @param x_label (`character`)\cr
#'   Label of the x axis.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @template author_qit3
#'
tm_g_swimlane <- function(label,
                                dataname,
                                bar_var,
                                bar_color_var = NULL,
                                sort_var = NULL,
                                marker_pos_var = NULL,
                                marker_shape_var = NULL,
                                marker_shape_opt = NULL,
                                marker_color_var = NULL,
                                marker_color_opt = NULL,
                                anno_txt_var = NULL,
                                vref_line = NULL,
                                plot_height = c(1200L, 400L, 5000L),
                                plot_width = NULL,
                                pre_output = NULL,
                                post_output = NULL,
                                x_label = "Time from First Treatment (Day)",
                                transformators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  .assert_picks_single_var(bar_var, "bar_var")
  if (!is.null(bar_color_var)) .assert_picks_single_var(bar_color_var, "bar_color_var")
  if (!is.null(sort_var)) .assert_picks_single_var(sort_var, "sort_var")
  if (!is.null(marker_pos_var)) .assert_picks_single_var(marker_pos_var, "marker_pos_var")
  if (!is.null(marker_shape_var)) .assert_picks_single_var(marker_shape_var, "marker_shape_var")
  if (!is.null(marker_color_var)) .assert_picks_single_var(marker_color_var, "marker_color_var")
  checkmate::assert_numeric(marker_shape_opt, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_character(marker_color_opt, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_numeric(vref_line, min.len = 1, null.ok = TRUE, any.missing = FALSE)
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
  checkmate::assert_string(x_label)

  pick_slots <- Filter(
    Negate(is.null),
    list(
      bar_var = bar_var,
      bar_color_var = bar_color_var,
      sort_var = sort_var,
      marker_pos_var = marker_pos_var,
      marker_shape_var = marker_shape_var,
      marker_color_var = marker_color_var,
      anno_txt_var = anno_txt_var
    )
  )
  all_datanames <- unique(c("ADSL", dataname, .picks_all_datanames(pick_slots)))

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    ui_args = args[names(args) %in% names(formals(ui_g_swimlane))],
    server_args = args[names(args) %in% names(formals(srv_g_swimlane))],
    transformators = transformators,
    datanames = all_datanames
  )
}

#' @keywords internal
ui_g_swimlane <- function(id,
                                dataname,
                                bar_var,
                                bar_color_var,
                                sort_var,
                                marker_pos_var,
                                marker_shape_var,
                                marker_color_var,
                                anno_txt_var,
                                vref_line,
                                pre_output,
                                post_output) {
  ns <- NS(id)

  shiny::tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("swimlaneplot"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis data:", tags$code(dataname)),
        left_bordered_div(
          tags$div(
            tags$label("Bar length"),
            teal.picks::picks_ui(ns("bar_var"), bar_var)
          ),
          if (!is.null(bar_color_var)) {
            tags$div(
              tags$label("Bar color"),
              teal.picks::picks_ui(ns("bar_color_var"), bar_color_var)
            )
          }
        ),
        if (!is.null(sort_var)) {
          tags$div(
            tags$label("Sort by"),
            teal.picks::picks_ui(ns("sort_var"), sort_var)
          )
        },
        if (dataname != "ADSL" && !is.null(marker_pos_var)) {
          left_bordered_div(
            tags$div(
              tags$label("Marker position"),
              teal.picks::picks_ui(ns("marker_pos_var"), marker_pos_var)
            ),
            if (!is.null(marker_shape_var)) {
              tags$div(
                tags$label("Marker shape"),
                teal.picks::picks_ui(ns("marker_shape_var"), marker_shape_var)
              )
            },
            if (!is.null(marker_color_var)) {
              tags$div(
                tags$label("Marker color"),
                teal.picks::picks_ui(ns("marker_color_var"), marker_color_var)
              )
            }
          )
        },
        if (!is.null(anno_txt_var)) {
          tags$div(
            tags$label("Annotation variables"),
            teal.picks::picks_ui(ns("anno_txt_var"), anno_txt_var)
          )
        },
        textInput(
          ns("vref_line"),
          label = tags$div(
            "Vertical Reference Line(s)",
            tags$br(),
            helpText("Enter numeric value(s) of reference lines, separated by comma (eg. 100, 200)")
          ),
          value = paste(vref_line, collapse = ", ")
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

#' @keywords internal
.swimlane_picks_selected_var <- function(selector_state) {
  if (is.null(selector_state) || is.null(selector_state$variables)) {
    return(character())
  }
  as.character(selector_state$variables$selected)
}

#' @keywords internal
srv_g_swimlane <- function(id,
                                 data,
                                 dataname,
                                 marker_shape_opt,
                                 marker_color_opt,
                                 plot_height,
                                 plot_width,
                                 x_label,
                                 bar_var,
                                 bar_color_var,
                                 sort_var,
                                 marker_pos_var,
                                 marker_shape_var,
                                 marker_color_var,
                                 anno_txt_var) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    picks_inputs <- Filter(
      Negate(is.null),
      list(
        bar_var = bar_var,
        bar_color_var = bar_color_var,
        sort_var = sort_var,
        marker_pos_var = if (dataname != "ADSL") marker_pos_var else NULL,
        marker_shape_var = if (dataname != "ADSL") marker_shape_var else NULL,
        marker_color_var = if (dataname != "ADSL") marker_color_var else NULL,
        anno_txt_var = anno_txt_var
      )
    )

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = picks_inputs,
      data = data
    )

    iv <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("vref_line", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Vertical Reference Line(s) are invalid"
      })
      iv$enable()
      iv
    })

    output_q <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )

      teal::validate_inputs(iv())

      bar_var_name <- .swimlane_picks_selected_var(selectors$bar_var())
      validate(
        need(length(bar_var_name) > 0L, "Please select a bar length variable.")
      )

      bar_color_var_name <- if (!is.null(bar_color_var)) {
        .swimlane_picks_selected_var(selectors$bar_color_var())
      } else {
        character()
      }
      sort_var_name <- if (!is.null(sort_var)) {
        .swimlane_picks_selected_var(selectors$sort_var())
      } else {
        character()
      }
      anno_txt_var_name <- if (!is.null(anno_txt_var)) {
        .swimlane_picks_selected_var(selectors$anno_txt_var())
      } else {
        character()
      }

      marker_pos_var_name <- if (dataname != "ADSL" && !is.null(marker_pos_var)) {
        .swimlane_picks_selected_var(selectors$marker_pos_var())
      } else {
        character()
      }
      marker_shape_var_name <- if (dataname != "ADSL" && !is.null(marker_shape_var)) {
        .swimlane_picks_selected_var(selectors$marker_shape_var())
      } else {
        character()
      }
      marker_color_var_name <- if (dataname != "ADSL" && !is.null(marker_color_var)) {
        .swimlane_picks_selected_var(selectors$marker_color_var())
      } else {
        character()
      }

      validate(need("ADSL" %in% names(obj), "'ADSL' not included in data"))
      validate(need(
        (length(obj) == 1 && dataname == "ADSL") ||
          (length(obj) >= 2 && dataname != "ADSL"),
        paste(
          "Please either add just 'ADSL' as dataname when just ADSL is available.",
          "In case 2 datasets are available ADSL is not supposed to be the dataname."
        )
      ))

      ADSL <- obj[["ADSL"]]

      anl_vars <- unique(c(
        "USUBJID", "STUDYID",
        marker_pos_var_name, marker_shape_var_name, marker_color_var_name
      ))
      adsl_vars <- unique(c(
        "USUBJID", "STUDYID",
        bar_var_name, bar_color_var_name, sort_var_name, anno_txt_var_name
      ))

      if (dataname == "ADSL") {
        teal::validate_has_data(ADSL, min_nrow = 3)
        teal::validate_has_variable(ADSL, adsl_vars)
      } else {
        anl <- obj[[dataname]]
        teal::validate_has_data(anl, min_nrow = 3)
        teal::validate_has_variable(anl, anl_vars)
        validate(need(
          length(marker_pos_var_name) > 0L,
          "Please select a marker position variable."
        ))
      }

      vref_line <- suppressWarnings(as_numeric_from_comma_sep_str(debounce(reactive(input$vref_line), 1500)()))

      q1 <- obj

      q2 <- teal.code::eval_code(
        q1,
        code = bquote({
          bar_var <- .(bar_var_name)
          bar_color_var <- .(bar_color_var_name)
          sort_var <- .(sort_var_name)
          marker_pos_var <- .(marker_pos_var_name)
          marker_shape_var <- .(marker_shape_var_name)
          marker_color_var <- .(marker_color_var_name)
          anno_txt_var <- .(anno_txt_var_name)
        })
      )

      q3 <- if (dataname == "ADSL") {
        teal.code::eval_code(
          q2,
          code = bquote({
            ADSL_p <- ADSL
            ADSL <- ADSL_p[, .(adsl_vars), drop = FALSE]
            ADSL$USUBJID <- unlist(lapply(strsplit(ADSL$USUBJID, "-", fixed = TRUE), tail, 1))
          })
        )
      } else {
        teal.code::eval_code(
          q2,
          code = bquote({
            ADSL_p <- ADSL
            ANL_p <- .(as.name(dataname))

            ADSL <- ADSL_p[, .(adsl_vars), drop = FALSE]
            ANL <- merge(
              x = ADSL,
              y = ANL_p[, .(anl_vars), drop = FALSE],
              all.x = FALSE, all.y = FALSE,
              by = c("USUBJID", "STUDYID")
            )
            ADSL$USUBJID <- unlist(lapply(strsplit(ADSL$USUBJID, "-", fixed = TRUE), tail, 1))
            ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1))
          })
        )
      }

      plot_call <- if (dataname == "ADSL") {
        bquote(
          plot <- osprey::g_swimlane(
            bar_id = ADSL[["USUBJID"]],
            bar_length = ADSL[[bar_var]],
            sort_by = .(if (length(sort_var) > 0) quote(ADSL[[sort_var]]) else NULL),
            col_by = .(if (length(bar_color_var) > 0) quote(ADSL[[bar_color_var]]) else NULL),
            marker_id = NULL,
            marker_pos = NULL,
            marker_shape = NULL,
            marker_shape_opt = NULL,
            marker_color = NULL,
            marker_color_opt = NULL,
            anno_txt = .(if (length(anno_txt_var) > 0) quote(ADSL[, anno_txt_var, drop = FALSE]) else NULL),
            xref_line = .(vref_line),
            xtick_at = ggplot2::waiver(),
            xlab = .(x_label),
            title = "Swimlane Plot"
          )
        )
      } else {
        bquote(
          plot <- osprey::g_swimlane(
            bar_id = ADSL[["USUBJID"]],
            bar_length = ADSL[[bar_var]],
            sort_by = .(if (length(sort_var) > 0) quote(ADSL[[sort_var]]) else NULL),
            col_by = .(if (length(bar_color_var) > 0) quote(ADSL[[bar_color_var]]) else NULL),
            marker_id = ANL[["USUBJID"]],
            marker_pos = .(if (length(marker_pos_var) > 0) quote(ANL[[marker_pos_var]]) else NULL),
            marker_shape = .(if (length(marker_shape_var) > 0) quote(ANL[[marker_shape_var]]) else NULL),
            marker_shape_opt = .(if (length(marker_shape_var) == 0) {
              NULL
            } else if (
              length(marker_shape_var) > 0 &&
                all(unique(ANL[[marker_shape_var]]) %in% names(marker_shape_opt))
            ) {
              bquote(.(marker_shape_opt))
            } else {
              NULL
            }),
            marker_color = .(if (length(marker_color_var) > 0) quote(ANL[[marker_color_var]]) else NULL),
            marker_color_opt = .(if (length(marker_color_var) == 0) {
              NULL
            } else if (
              length(marker_color_var) > 0 &&
                all(unique(ANL[[marker_color_var]]) %in% names(marker_color_opt))
            ) {
              bquote(.(marker_color_opt))
            } else {
              NULL
            }),
            anno_txt = .(if (length(anno_txt_var) > 0) quote(ADSL[, anno_txt_var, drop = FALSE]) else NULL),
            xref_line = .(vref_line),
            xtick_at = ggplot2::waiver(),
            xlab = .(x_label),
            title = "Swimlane Plot"
          )
        )
      }

      teal.reporter::teal_card(q3) <- c(teal.reporter::teal_card(q3), "### Plot")

      if (length(sort_var_name) > 0L) {
        teal.reporter::teal_card(q3) <- c(teal.reporter::teal_card(q3), "### Selected Options")
        teal.reporter::teal_card(q3) <- c(teal.reporter::teal_card(q3), paste("Sorted by:", sort_var_name))
      }

      teal.code::eval_code(q3, code = plot_call)
    })

    plot_r <- reactive(output_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "swimlaneplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, output_q)
  })
}
