#' Teal Module for `Swimlane` Plot
#'
#' @description
#'
#' This is a teal module that generates a `swimlane` plot (bar plot with markers) for `ADaM` data
#' using [teal.picks::variables()] encodings.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param dataname (`character(1)`)\cr
#'   analysis data used for markers. Use `"ADSL"` when no markers are plotted.
#' @param bar_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Subject-level numeric variable for bar length (from `parentname`).
#' @param parentname (`character(1)`)\cr
#'  analysis data used for several variables in the teal module, needs to be
#'  available in the list passed to the `data` argument of [teal::init()]. The default is
#' `"ADSL"`
#' @param bar_color_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Subject-level color variable from `parentname`.
#' @param sort_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Subject-level sort variable from `parentname`.
#' @param marker_pos_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Marker position variable from `dataname`).
#' @param marker_shape_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Marker shape variable from `dataname`.
#' @param marker_shape_opt (`numeric`)\cr
#'   Named vector mapping shape values to ggplot shapes.
#' @param marker_color_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Marker color variable from `dataname`.
#' @param marker_color_opt (`character`)\cr
#'   Named vector mapping color values to colors.
#' @param anno_txt_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object,
#'   Subject-level annotation variables from `parentname` (multiple selection allowed).
#' @param vref_line (`numeric`)\cr
#'   Vertical reference lines.
#' @param x_label (`character`)\cr
#'   Label of the x axis.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @examples
#' data <- within(teal_data(), {
#'   library(nestcolor)
#'   library(dplyr)
#'   ADSL <- rADSL %>%
#'     mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1) %>%
#'     filter(STRATA1 == "A" & ARMCD == "ARM A")
#'   ADRS <- rADRS %>%
#'     filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#'     mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
#'     rbind(rADRS %>% filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#'     arrange(USUBJID)
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_swimlane(
#'       label = "Swimlane Plot",
#'       dataname = "ADRS",
#'       bar_var = variables(
#'         choices = c("TRTDURD", "EOSDY"),
#'         selected = "TRTDURD"
#'       ),
#'       bar_color_var = variables(
#'         choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "SEX"),
#'         selected = "EOSSTT"
#'       ),
#'       sort_var = variables(
#'         choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD"),
#'         selected = "ACTARMCD"
#'       ),
#'       marker_pos_var = variables(
#'         choices = c("ADY"),
#'         selected = "ADY"
#'       ),
#'       marker_shape_var = variables(
#'         selected = "AVALC",
#'         c("AVALC", "AVISIT")
#'       ),
#'       marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
#'       marker_color_var = variables(
#'         selected = "AVALC",
#'         choices = c("AVALC", "AVISIT")
#'       ),
#'       marker_color_opt = c(
#'         "CR" = "green", "PR" = "blue", "SD" = "goldenrod",
#'         "PD" = "red", "Death" = "black"
#'       ),
#'       vref_line = c(30, 60),
#'       anno_txt_var = variables(
#'         selected = c("ACTARM", "SEX"),
#'         choices = c(
#'           "ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1",
#'           "SEX", "RACE", "COUNTRY", "DCSREAS", "DCSREASP"
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
#' @template author_qit3
#'
tm_g_swimlane <- function(label,
                          dataname,
                          bar_var,
                          parentname = "ADSL",
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

  bar_var <- migrate_choices_selected_to_variables(bar_var, "bar_var")
  bar_color_var <- migrate_choices_selected_to_variables(bar_color_var, "bar_color_var", null.ok = TRUE)
  sort_var <- migrate_choices_selected_to_variables(sort_var, "sort_var", null.ok = TRUE)
  marker_pos_var <- migrate_choices_selected_to_variables(marker_pos_var, "marker_pos_var", null.ok = TRUE)
  marker_shape_var <- migrate_choices_selected_to_variables(marker_shape_var, "marker_shape_var", null.ok = TRUE)
  marker_color_var <- migrate_choices_selected_to_variables(marker_color_var, "marker_color_var", null.ok = TRUE)
  anno_txt_var <- migrate_choices_selected_to_variables(anno_txt_var, "anno_txt_var", null.ok = TRUE)

  bar_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), bar_var)
  if (!is.null(bar_color_var)) {
    bar_color_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), bar_color_var)
  }
  if (!is.null(sort_var)) {
    sort_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), sort_var)
  }
  if (!is.null(marker_pos_var)) {
    marker_pos_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), marker_pos_var)
  }
  if (!is.null(marker_shape_var)) {
    marker_shape_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), marker_shape_var)
  }
  if (!is.null(marker_color_var)) {
    marker_color_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), marker_color_var)
  }
  if (!is.null(anno_txt_var)) {
    anno_txt_var <- create_picks_helper(teal.picks::datasets(parentname, parentname), anno_txt_var)
  }

  bar_var <- force_pick_selection(bar_var, "bar_var")
  if (!is.null(bar_color_var)) bar_color_var <- force_pick_selection(bar_color_var, "bar_color_var")
  if (!is.null(marker_pos_var)) marker_pos_var <- force_pick_selection(marker_pos_var, "marker_pos_var")
  if (!is.null(marker_shape_var)) {
    marker_shape_var <- force_pick_selection(marker_shape_var, "marker_shape_var")
  }
  if (!is.null(marker_color_var)) {
    marker_color_var <- force_pick_selection(marker_color_var, "marker_color_var")
  }


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

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    ui_args = args[names(args) %in% names(formals(ui_g_swimlane))],
    server_args = args[names(args) %in% names(formals(srv_g_swimlane))],
    transformators = transformators,
    datanames = .picks_datanames(pick_slots)
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
        if (!is.null(marker_pos_var)) {
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
srv_g_swimlane <- function(id,
                           data,
                           dataname,
                           parentname,
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

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = pick_slots,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      teal::validate_input(
        "vref_line",
        condition = !anyNA(suppressWarnings(as_numeric_from_comma_sep_str(input$vref_line))),
        "Vertical Reference Line(s) are invalid"
      )
      teal::validate_input(
        "bar_var",
        condition = !is.null(pick_selected("bar_var", selectors)),
        "Please select a bar length variable."
      )
      teal::validate_input(
        "marker_pos_var",
        condition = !is.null(pick_selected("marker_pos_var", selectors))
      )
      obj
    })

    merged <- teal.picks::merge_srv(
      "merge",
      data = validated_q,
      selectors = selectors,
      output_name = "ANL"
    )

    output_q <- reactive({
      qenv <- merged$data()
      teal.reporter::teal_card(qenv) <-
        c(
          teal.reporter::teal_card(qenv),
          teal.reporter::teal_card("## Module's output(s)")
        )

      bar_var_name <- pick_selected("bar_var", selectors)
      bar_color_var_name <- pick_selected("bar_color_var", selectors)
      sort_var_name <- pick_selected("sort_var", selectors)
      anno_txt_var_name <- pick_selected("anno_txt_var", selectors)
      marker_pos_var_name <- pick_selected("marker_pos_var", selectors)
      marker_shape_var_name <- pick_selected("marker_shape_var", selectors)
      marker_color_var_name <- pick_selected("marker_color_var", selectors)


      validate(need(parentname %in% names(qenv), sprintf("'%s' not included in data", parentname)))

      parentdata <- qenv[[parentname]]

      dataname_vars <- unique(c(
        "USUBJID", "STUDYID",
        marker_pos_var_name, marker_shape_var_name, marker_color_var_name
      ))
      parentname_vars <- unique(c(
        "USUBJID", "STUDYID",
        bar_var_name, bar_color_var_name, sort_var_name, anno_txt_var_name
      ))

      teal::validate_has_data(parentdata, min_nrow = 3)
      teal::validate_has_variable(parentdata, parentname_vars)

      dataname_data <- qenv[[dataname]]
      teal::validate_has_data(dataname_data, min_nrow = 3)
      teal::validate_has_variable(dataname_data, dataname_vars)

      vref_line <- suppressWarnings(as_numeric_from_comma_sep_str(debounce(reactive(input$vref_line), 1500)()))

      q1 <- teal.code::eval_code(
        qenv,
        code = bquote({
          parentdata_p <- .(as.name(parentname))

          parentdata <- parentdata_p[, .(parentname_vars), drop = FALSE]
          parentdata$USUBJID <- unlist(lapply(strsplit(parentdata$USUBJID, "-", fixed = TRUE), tail, 1))
          ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1))
        })
      )

      ANL <- q1[["ANL"]]
      parentdata <- q1[["parentdata"]]

      teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

      if (length(sort_var_name) > 0L) {
        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Selected Options")
        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), paste("Sorted by:", sort_var_name))
      }

      q2 <- within(
        q1,
        expr = {
          plot <- osprey::g_swimlane(
            bar_id = parentdata[["USUBJID"]],
            bar_length = parentdata[[bar_var_name]],
            sort_by = if (length(sort_var_name) > 0) parentdata[[sort_var_name]] else NULL,
            col_by = if (length(bar_color_var_name) > 0) parentdata[[bar_color_var_name]] else NULL,
            marker_id = ANL[["USUBJID"]],
            marker_pos = if (length(marker_pos_var_name) > 0) ANL[[marker_pos_var_name]] else NULL,
            marker_shape = if (length(marker_shape_var_name) > 0) ANL[[marker_shape_var_name]] else NULL,
            marker_shape_opt = if (length(marker_shape_var_name) == 0) {
              NULL
            } else if (
              length(marker_shape_var_name) > 0 &&
                all(unique(ANL[[marker_shape_var_name]]) %in% names(marker_shape_opt))
            ) {
              bquote(.(marker_shape_opt))
            } else {
              NULL
            },
            marker_color = if (length(marker_color_var_name) > 0) ANL[[marker_color_var_name]] else NULL,
            marker_color_opt = if (length(marker_color_var_name) == 0) {
              NULL
            } else if (
              length(marker_color_var_name) > 0 &&
                all(unique(ANL[[marker_color_var_name]]) %in% names(marker_color_opt))
            ) {
              marker_color_opt
            } else {
              NULL
            },
            anno_txt = if (length(anno_txt_var_name) > 0) parentdata[, anno_txt_var_name, drop = FALSE] else NULL,
            xref_line = vref_line,
            xtick_at = ggplot2::waiver(),
            xlab = x_label,
            title = "Swimlane Plot"
          )
        },
        bar_var_name = bar_var_name,
        sort_var_name = sort_var_name,
        bar_color_var_name = bar_color_var_name,
        marker_pos_var_name = marker_pos_var_name,
        marker_shape_var_name = marker_shape_var_name,
        marker_color_var_name = marker_color_var_name,
        marker_shape_opt = marker_shape_opt,
        marker_color_opt = marker_color_opt,
        anno_txt_var_name = anno_txt_var_name,
        vref_line = vref_line,
        x_label = x_label
      )
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
