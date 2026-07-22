#' Spider plot Teal Module
#'
#' @description
#'
#' Display spider plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param x_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for x-axis variables.
#' @param y_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for y-axis variables.
#' @param marker_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for marker symbol.
#' @param line_colorby_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for line color.
#' @param vref_line  vertical reference lines
#' @param href_line horizontal reference lines
#' @param anno_txt_var annotation text
#' @param legend_on boolean value for whether legend is displayed
#' @param xfacet_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for x facets.
#' @param yfacet_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`teal.transform::choices_selected`]) `choices_selected` object for y facets.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#' data <- teal_data() %>%
#'   within({
#'     library(nestcolor)
#'     ADSL <- teal.data::rADSL
#'     ADTR <- teal.data::rADTR
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_spiderplot(
#'       label = "Spider plot (picks)",
#'       dataname = "ADTR",
#'       paramcd = variables(
#'         choices = "PARAMCD",
#'         selected = "PARAMCD"
#'       ),
#'       x_var = variables(
#'         choices = dplyr::where(is.numeric),
#'         selected = 1L
#'       ),
#'       y_var = variables(
#'         choices = c("PCHG", "CHG", "AVAL"),
#'         selected = "PCHG"
#'       ),
#'       marker_var = variables(
#'         choices = c("SEX", "RACE", "USUBJID"),
#'         selected = "SEX"
#'       ),
#'       line_colorby_var = variables(
#'         choices = c("SEX", "USUBJID", "RACE"),
#'         selected = "SEX"
#'       ),
#'       xfacet_var = variables(
#'         choices = c("SEX", "ARM"),
#'         selected = "SEX"
#'       ),
#'       yfacet_var = variables(
#'         choices = c("SEX", "ARM"),
#'         selected = "ARM"
#'       ),
#'       vref_line = "10, 37",
#'       href_line = "-20, 0"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_g_spiderplot <- function(label,
                            dataname,
                            paramcd,
                            x_var,
                            y_var,
                            marker_var,
                            line_colorby_var,
                            xfacet_var = NULL,
                            yfacet_var = NULL,
                            vref_line = NULL,
                            href_line = NULL,
                            anno_txt_var = TRUE,
                            legend_on = FALSE,
                            plot_height = c(600L, 200L, 2000L),
                            plot_width = NULL,
                            pre_output = NULL,
                            post_output = NULL,
                            transformators = list()) {
  message("Initializing tm_g_spiderplot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)

  paramcd <- migrate_choices_selected_to_variables(paramcd)
  x_var <- migrate_choices_selected_to_variables(x_var)
  y_var <- migrate_choices_selected_to_variables(y_var)
  marker_var <- migrate_choices_selected_to_variables(marker_var)
  line_colorby_var <- migrate_choices_selected_to_variables(line_colorby_var)
  xfacet_var <- migrate_choices_selected_to_variables(xfacet_var, null.ok = TRUE)
  yfacet_var <- migrate_choices_selected_to_variables(yfacet_var, null.ok = TRUE)

  paramcd <- create_picks_helper(teal.picks::datasets(dataname, dataname), paramcd)
  x_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), x_var)
  y_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), y_var)
  marker_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), marker_var)
  line_colorby_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), line_colorby_var)
  if (!is.null(xfacet_var)) {
    xfacet_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xfacet_var)
  }
  if (!is.null(yfacet_var)) {
    yfacet_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), yfacet_var)
  }

  paramcd <- force_pick_variable_selection(paramcd, "paramcd")
  x_var <- force_pick_variable_selection(x_var, "x_var")
  y_var <- force_pick_variable_selection(y_var, "y_var")
  marker_var <- force_pick_variable_selection(marker_var, "marker_var")
  line_colorby_var <- force_pick_variable_selection(line_colorby_var, "line_colorby_var")

  checkmate::assert_class(paramcd, "picks")
  checkmate::assert_class(x_var, "picks")
  checkmate::assert_class(y_var, "picks")
  checkmate::assert_class(marker_var, "picks")
  checkmate::assert_class(line_colorby_var, "picks")
  checkmate::assert_class(xfacet_var, "picks", null.ok = TRUE)
  checkmate::assert_class(yfacet_var, "picks", null.ok = TRUE)

  checkmate::assert_string(vref_line, null.ok = TRUE)
  checkmate::assert_string(href_line, null.ok = TRUE)
  checkmate::assert_flag(anno_txt_var)
  checkmate::assert_flag(legend_on)
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
    datanames = c("ADSL", dataname),
    server = srv_g_spider,
    server_args = args[names(args) %in% names(formals(srv_g_spider))],
    ui = ui_g_spider,
    ui_args = args[names(args) %in% names(formals(ui_g_spider))],
    transformators = transformators
  )
}

ui_g_spider <- function(id,
                        dataname,
                        paramcd,
                        x_var,
                        y_var,
                        marker_var,
                        line_colorby_var,
                        xfacet_var,
                        yfacet_var,
                        vref_line,
                        href_line,
                        anno_txt_var,
                        legend_on,
                        pre_output,
                        post_output) {
  ns <- NS(id)
  shiny::tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("spiderplot"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis data:", tags$code(dataname)),
        tags$div(
          tags$strong("Parameter Column"),
          teal.picks::picks_ui(id = ns("paramcd"), picks = paramcd)
        ),
        teal.widgets::optionalSelectInput(
          ns("paramcd_val"),
          label = "Parameter Value",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        tags$div(
          tags$strong("X-axis Variable"),
          teal.picks::picks_ui(id = ns("x_var"), picks = x_var)
        ),
        tags$div(
          tags$strong("Y-axis Variable"),
          teal.picks::picks_ui(id = ns("y_var"), picks = y_var)
        ),
        tags$div(
          tags$strong("Color By Variable (Line)"),
          teal.picks::picks_ui(id = ns("line_colorby_var"), picks = line_colorby_var)
        ),
        tags$div(
          tags$strong("Marker Symbol By Variable"),
          teal.picks::picks_ui(id = ns("marker_var"), picks = marker_var)
        ),
        if (!is.null(xfacet_var)) {
          tags$div(
            tags$strong("X-facet By Variable"),
            teal.picks::picks_ui(id = ns("xfacet_var"), picks = xfacet_var)
          )
        },
        if (!is.null(yfacet_var)) {
          tags$div(
            tags$strong("Y-facet By Variable"),
            teal.picks::picks_ui(id = ns("yfacet_var"), picks = yfacet_var)
          )
        },
        checkboxInput(
          ns("anno_txt_var"),
          "Add subject ID label",
          value = anno_txt_var
        ),
        checkboxInput(
          ns("legend_on"),
          "Add legend",
          value = legend_on
        ),
        textInput(
          ns("vref_line"),
          label = tags$div(
            "Vertical reference line(s)",
            bslib::tooltip(
              trigger = icon("circle-info"),
              tags$span(
                "Enter numeric value(s) of vertical reference lines, separated by comma (eg. -2, 1)"
              )
            )
          ),
          value = vref_line
        ),
        textInput(
          ns("href_line"),
          label = tags$div(
            "Hortizontal reference line(s)",
            bslib::tooltip(
              trigger = icon("circle-info"),
              tags$span(
                "Enter numeric value(s) of horizontal reference lines, separated by comma (eg. -2, 1)"
              )
            )
          ),
          value = href_line
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

srv_g_spider <- function(
  id,
  data,
  dataname,
  paramcd,
  x_var,
  y_var,
  marker_var,
  line_colorby_var,
  xfacet_var,
  yfacet_var,
  label,
  plot_height,
  plot_width
) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Build picks list (exclude NULL optional picks)
    picks_list <- list(
      paramcd = paramcd,
      x_var = x_var,
      y_var = y_var,
      marker_var = marker_var,
      line_colorby_var = line_colorby_var,
      xfacet_var = xfacet_var,
      yfacet_var = yfacet_var
    )

    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    # Merge datasets based on picks selections
    merged <- teal.picks::merge_srv(
      "merge",
      data = data,
      selectors = selectors,
      output_name = "ANL"
    )

    # populate paramcd_val choices from the selected paramcd column
    observeEvent(merged$variables()$paramcd,
      {
        paramcd_col <- merged$variables()$paramcd
        if (!is.null(paramcd_col) && paramcd_col %in% names(merged$data()[["ANL"]])) {
          choices <- sort(unique(as.character(merged$data()[["ANL"]][[paramcd_col]])))
          teal.widgets::updateOptionalSelectInput(
            session,
            "paramcd_val",
            choices = choices,
            selected = choices[1]
          )
        }
      },
      ignoreNULL = FALSE
    )

    # render plot
    output_q <- reactive({
      qenv <- merged$data()
      teal.reporter::teal_card(qenv) <-
        c(
          teal.reporter::teal_card(qenv),
          teal.reporter::teal_card("## Module's output(s)")
        )
      qenv <- teal.code::eval_code(qenv, "library(dplyr)")

      # We add USUBJID if from ADSL if it is not present in the merge keys
      qenv <- teal.code::eval_code(
        qenv,
        code = bquote({
          if (!"USUBJID" %in% names(ANL)) {
            ANL[["USUBJID"]] <- .(as.name(dataname))[["USUBJID"]]
          }
        })
      )

      # get datasets ---
      validated_q <- qenv
      ADTR <- validated_q[[dataname]]

      teal::validate_has_data(validated_q[["ANL"]], min_nrow = 1, msg = "ANL data has zero rows")
      teal::validate_has_data(ADTR, min_nrow = 1, msg = sprintf("%s data has zero rows", dataname))

      paramcd_col <- merged$variables()$paramcd
      paramcd <- input$paramcd_val
      x_var <- merged$variables()$x_var
      y_var <- merged$variables()$y_var
      marker_var <- merged$variables()$marker_var
      line_colorby_var <- merged$variables()$line_colorby_var
      anno_txt_var <- input$anno_txt_var
      legend_on <- input$legend_on
      xfacet_var <- merged$variables()$xfacet_var
      yfacet_var <- merged$variables()$yfacet_var
      vref_line <- input$vref_line
      href_line <- input$href_line

      # reference lines preprocessing
      vref_line <- as_numeric_from_comma_sep_str(vref_line)
      href_line <- as_numeric_from_comma_sep_str(href_line)

      validate_input(
        inputId = "paramcd",
        condition = length(paramcd_col) > 0,
        message = "Parameter Column is required."
      )

      validate_input(
        inputId = "paramcd_val",
        condition = length(paramcd) > 0,
        message = "Parameter Value is required."
      )

      validate_input(
        c("x_var", "y_var"),
        condition = x_var != y_var,
        message = "X and Y variable can't be the same."
      )
      # format and filter (ANL already merged by merge_srv)
      q1 <- teal.code::eval_code(
        validated_q,
        code = bquote({
          ANL <- ANL %>%
            group_by(USUBJID, .(as.name(paramcd_col))) %>%
            arrange(ANL[, .(x_var)]) %>%
            as.data.frame()
        })
      )

      ANL_f <- lbl <- NULL # to avoid R CMD check global binding NOTEs

      # format and filter
      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1))
          ANL_f <- ANL %>%
            filter(.data[[.(paramcd_col)]] == .(paramcd)) %>%
            as.data.frame()
        })
      )

      # label
      q1 <- if (anno_txt_var) {
        teal.code::eval_code(
          q1,
          code = quote(lbl <- list(txt_ann = as.factor(ANL_f$USUBJID)))
        )
      } else {
        teal.code::eval_code(q1, code = quote(lbl <- NULL))
      }

      # plot code to qenv ---

      teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")
      if (!is.null(paramcd) || !is.null(xfacet_var) || !is.null(yfacet_var)) {
        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Selected Options")
      }
      if (!is.null(paramcd)) {
        teal.reporter::teal_card(q1) <-
          c(
            teal.reporter::teal_card(q1),
            paste0("Parameter - ", paramcd_col, " == ", paramcd, " (from ", dataname, ").")
          )
      }
      if (!is.null(xfacet_var)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          sprintf("Faceted horizontally by: %s.", paste(xfacet_var, collapse = ", "))
        )
      }
      if (!is.null(yfacet_var)) {
        teal.reporter::teal_card(q1) <- c(
          teal.reporter::teal_card(q1),
          sprintf("Faceted vertically by: %s.", paste(yfacet_var, collapse = ", "))
        )
      }

      q1 <- within(q1,
        {
          plot <- osprey::g_spiderplot(
            marker_x = ANL_f[[x_var]],
            marker_id = ANL_f$USUBJID,
            marker_y = ANL_f[[y_var]],
            line_colby = if (line_colorby_var != "None") {
              ANL_f[[line_colorby_var]]
            } else {
              NULL
            },
            marker_shape = if (marker_var != "None") {
              ANL_f[[marker_var]]
            } else {
              NULL
            },
            marker_size = 4,
            datalabel_txt = lbl,
            facet_rows = if (!is.null(yfacet_var)) {
              data.frame(ANL_f[, yfacet_var, drop = FALSE])
            } else {
              NULL
            },
            facet_columns = if (!is.null(xfacet_var)) {
              data.frame(ANL_f[, xfacet_var, drop = FALSE])
            } else {
              NULL
            },
            vref_line = vref_line,
            href_line = href_line,
            x_label = if (is.null(formatters::var_labels(dataname[x_var], fill = FALSE))) {
              x_var
            } else {
              formatters::var_labels(dataname[x_var], fill = FALSE)
            },
            y_label = if (is.null(formatters::var_labels(dataname[y_var], fill = FALSE))) {
              y_var
            } else {
              formatters::var_labels(dataname[y_var], fill = FALSE)
            },
            show_legend = legend_on
          )
        },
        x_var = x_var,
        y_var = y_var,
        line_colorby_var = line_colorby_var,
        marker_var = marker_var,
        yfacet_var = yfacet_var,
        xfacet_var = xfacet_var,
        vref_line = vref_line,
        href_line = href_line,
        dataname = as.name(dataname),
        legend_on = legend_on
      )
    })

    plot_r <- reactive(output_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "spiderplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, output_q)
  })
}
