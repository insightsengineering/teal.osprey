#' Teal Module for `Swimlane` Plot
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is teal module that generates a `swimlane` plot (bar plot with markers) for `ADaM` data
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param dataname analysis data used for plotting, needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}}. If no markers are to be plotted in the module, `"ADSL"` should be
#' the input. If markers are to be plotted, data name for the marker data should be the input
#' @param bar_var (\code{\link[teal.transform]{choices_selected}}) subject-level numeric variable from dataset
#' to plot as the bar length
#' @param bar_color_var (\code{\link[teal.transform]{choices_selected}}) color by variable (subject-level)
#' @param sort_var (\code{choices_selected}) sort by variable (subject-level)
#' @param marker_pos_var (\code{\link[teal.transform]{choices_selected}}) variable for marker position from marker data
#' (Note: make sure that marker position has the same relative start day as bar length variable \code{bar_var})
#' @param marker_shape_var (\code{\link[teal.transform]{choices_selected}}) marker shape variable from marker data
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name).
#' If not \code{NULL}, please make sure this contains all possible values for \code{marker_shape_var} values,
#' otherwise shape will be assigned by \code{ggplot} default
#' @param marker_color_var marker color variable from marker data
#' @param marker_color_opt aesthetic values to map color values (named vector to map color values to each name).
#' If not \code{NULL}, please make sure this contains all possible values for \code{marker_color_var} values,
#' otherwise color will be assigned by \code{ggplot} default
#' @param vref_line vertical reference lines
#' @param anno_txt_var character vector with subject-level variable names that are selected as annotation
#' @param x_label the label of the x axis
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @template author_qit3
#'
#' @examples
#' # Example using stream (ADaM) dataset
#' data <- teal.data::cdisc_data() |>
#'   within(library(dplyr)) |>
#'   within(library(nestcolor)) |>
#'   within(ADSL <- osprey::rADSL %>%
#'     dplyr::mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1) %>%
#'     dplyr::filter(STRATA1 == "A" & ARMCD == "ARM A")) |>
#'   within(ADRS <- osprey::rADRS) |>
#'   within(ADRS <- ADRS %>%
#'     dplyr::filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#'     mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
#'     base::rbind(ADRS %>% dplyr::filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#'     arrange(USUBJID))
#'
#' teal.data::datanames(data) <- c("ADSL", "ADRS")
#' teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[teal.data::datanames(data)]
#'
#' ADSL <- data[["ADSL"]]
#' ADRS <- data[["ADRS"]]
#'
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     tm_g_swimlane(
#'       label = "Swimlane Plot",
#'       dataname = "ADRS",
#'       bar_var = teal.transform::choices_selected(
#'         selected = "TRTDURD",
#'         choices = c("TRTDURD", "EOSDY")
#'       ),
#'       bar_color_var = teal.transform::choices_selected(
#'         selected = "EOSSTT",
#'         choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "SEX")
#'       ),
#'       sort_var = teal.transform::choices_selected(
#'         selected = "ACTARMCD",
#'         choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD")
#'       ),
#'       marker_pos_var = teal.transform::choices_selected(
#'         selected = "ADY",
#'         choices = c("ADY")
#'       ),
#'       marker_shape_var = teal.transform::choices_selected(
#'         selected = "AVALC",
#'         c("AVALC", "AVISIT")
#'       ),
#'       marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
#'       marker_color_var = teal.transform::choices_selected(
#'         selected = "AVALC",
#'         choices = c("AVALC", "AVISIT")
#'       ),
#'       marker_color_opt = c(
#'         "CR" = "green", "PR" = "blue", "SD" = "goldenrod",
#'         "PD" = "red", "Death" = "black"
#'       ),
#'       vref_line = c(30, 60),
#'       anno_txt_var = teal.transform::choices_selected(
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
                          x_label = "Time from First Treatment (Day)") {
  logger::log_info("Initializing tm_g_swimlane")
  args <- as.list(environment())

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_class(bar_var, classes = "choices_selected")
  checkmate::assert_class(bar_color_var, classes = "choices_selected")
  checkmate::assert_class(marker_pos_var, classes = "choices_selected")
  checkmate::assert_class(marker_shape_var, classes = "choices_selected")
  checkmate::assert_numeric(marker_shape_opt, min.len = 1, any.missing = FALSE)
  checkmate::assert_class(marker_color_var, classes = "choices_selected")
  checkmate::assert_character(marker_color_opt, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_class(anno_txt_var, classes = "choices_selected")
  checkmate::assert_numeric(vref_line, min.len = 1, null.ok = TRUE, any.missing = FALSE)
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
  checkmate::assert_string(x_label)


  module(
    label = label,
    ui = ui_g_swimlane,
    ui_args = args,
    server = srv_g_swimlane,
    server_args = list(
      dataname = dataname,
      marker_pos_var = marker_pos_var,
      marker_shape_var = marker_shape_var,
      marker_shape_opt = marker_shape_opt,
      marker_color_var = marker_color_var,
      marker_color_opt = marker_color_opt,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width,
      x_label = x_label
    ),
    datanames = c("ADSL", dataname)
  )
}


ui_g_swimlane <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("swimlaneplot"))
      ),
      encoding = div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis data:", code(a$dataname)),
        div(
          class = "pretty-left-border",
          teal.widgets::optionalSelectInput(
            ns("bar_var"),
            "Bar Length",
            choices = a$bar_var$choices,
            selected = a$bar_var$selected,
            multiple = FALSE,
            label_help = helpText("from ", code("ADSL"))
          ),
          teal.widgets::optionalSelectInput(
            ns("bar_color_var"),
            "Bar Color",
            choices = a$bar_color_var$choices,
            selected = a$bar_color_var$selected,
            multiple = FALSE,
            label_help = helpText("from ", code("ADSL"))
          )
        ),
        teal.widgets::optionalSelectInput(
          ns("sort_var"),
          "Sort by",
          choices = a$sort_var$choices,
          selected = a$sort_var$selected,
          multiple = FALSE,
          label_help = helpText("from ", code("ADSL"))
        ),
        div(
          class = "pretty-left-border",
          if (a$dataname == "ADSL") {
            NULL
          } else if (is.null(a$marker_pos_var)) {
            NULL
          } else {
            teal.widgets::optionalSelectInput(
              ns("marker_pos_var"),
              "Marker Position",
              choices = a$marker_pos_var$choices,
              selected = a$marker_pos_var$selected,
              multiple = FALSE,
              label_help = helpText("from ", code(a$dataname))
            )
          },
          uiOutput(ns("marker_shape_sel")),
          uiOutput(ns("marker_color_sel"))
        ),
        teal.widgets::optionalSelectInput(
          ns("anno_txt_var"),
          "Annotation Variables",
          choices = a$anno_txt_var$choices,
          selected = a$anno_txt_var$selected,
          multiple = TRUE,
          label_help = helpText("from ", code("ADSL"))
        ),
        textInput(
          ns("vref_line"),
          label = div(
            "Vertical Reference Line(s)",
            tags$br(),
            helpText("Enter numeric value(s) of reference lines, separated by comma (eg. 100, 200)")
          ),
          value = paste(a$vref_line, collapse = ", ")
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      ),
      pre_output = a$pre_output,
      post_output = a$post_output
    )
  )
}

srv_g_swimlane <- function(id,
                           data,
                           filter_panel_api,
                           reporter,
                           dataname,
                           marker_pos_var,
                           marker_shape_var,
                           marker_shape_opt,
                           marker_color_var,
                           marker_color_opt,
                           label,
                           plot_height,
                           plot_width,
                           x_label) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("bar_var", shinyvalidate::sv_required(
        message = "Bar Length is required"
      ))
      # If reference lines are requested
      iv$add_rule("vref_line", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Vertical Reference Line(s) are invalid"
      })
      iv$enable()
      iv
    })

    # if marker position is NULL, then hide options for marker shape and color
    output$marker_shape_sel <- renderUI({
      if (dataname == "ADSL" || is.null(marker_shape_var) || is.null(input$marker_pos_var)) {
        NULL
      } else {
        ns <- session$ns
        teal.widgets::optionalSelectInput(
          ns("marker_shape_var"), "Marker Shape",
          choices = marker_shape_var$choices,
          selected = marker_shape_var$selected, multiple = FALSE,
          label_help = helpText("from ", code(dataname))
        )
      }
    })
    output$marker_color_sel <- renderUI({
      if (dataname == "ADSL" || is.null(marker_color_var) || is.null(input$marker_pos_var)) {
        NULL
      } else {
        ns <- session$ns
        teal.widgets::optionalSelectInput(
          ns("marker_color_var"), "Marker Color",
          choices = marker_color_var$choices,
          selected = marker_color_var$selected, multiple = FALSE,
          label_help = helpText("from ", code(dataname))
        )
      }
    })

    # create plot
    output_q <- reactive({
      teal::validate_inputs(iv())

      validate(need("ADSL" %in% teal.data::datanames(data()), "'ADSL' not included in data"))
      validate(need(
        (length(teal.data::datanames(data())) == 1 && dataname == "ADSL") ||
          (length(teal.data::datanames(data())) >= 2 && dataname != "ADSL"), paste(
          "Please either add just 'ADSL' as dataname when just ADSL is available.",
          "In case 2 datasets are available ADSL is not supposed to be the dataname."
        )
      ))

      ADSL <- data()[["ADSL"]]

      anl_vars <- unique(c(
        "USUBJID", "STUDYID",
        input$marker_pos_var, input$marker_shape_var, input$marker_color_var
      ))
      adsl_vars <- unique(c(
        "USUBJID", "STUDYID",
        input$bar_var, input$bar_color_var, input$sort_var, input$anno_txt_var
      ))

      if (dataname == "ADSL") {
        teal::validate_has_data(ADSL, min_nrow = 3)
        teal::validate_has_variable(ADSL, adsl_vars)
      } else {
        anl <- data()[[dataname]]
        teal::validate_has_data(anl, min_nrow = 3)
        teal::validate_has_variable(anl, anl_vars)

        validate(need(
          !any(c(marker_pos_var, marker_shape_var, marker_color_var) %in% adsl_vars),
          "marker-related variables need to come from marker data"
        ))
      }

      # VARIABLE GETTERS
      # lookup bar variables
      bar_var <- input$bar_var
      bar_color_var <- input$bar_color_var
      sort_var <- input$sort_var
      anno_txt_var <- input$anno_txt_var

      # Check if marker inputs can be used
      if (dataname == "ADSL") {
        marker_pos_var <- NULL
        marker_shape_var <- NULL
        marker_color_var <- NULL
      } else {
        marker_pos_var <- input$marker_pos_var
        marker_shape_var <- input$marker_shape_var
        marker_color_var <- input$marker_color_var
      }
      vref_line <- suppressWarnings(as_numeric_from_comma_sep_str(debounce(reactive(input$vref_line), 1500)()))

      q1 <- data()

      q2 <- teal.code::eval_code(
        q1,
        code = bquote({
          bar_var <- .(bar_var)
          bar_color_var <- .(bar_color_var)
          sort_var <- .(sort_var)
          marker_pos_var <- .(marker_pos_var)
          marker_shape_var <- .(marker_shape_var)
          marker_color_var <- .(marker_color_var)
          anno_txt_var <- .(anno_txt_var)
        })
      )

      # WRITE DATA SELECTION TO qenv
      q3 <- if (dataname == "ADSL") {
        teal.code::eval_code(
          q2,
          code = bquote({
            ADSL_p <- ADSL
            ADSL <- ADSL_p[, .(adsl_vars)]
            # only take last part of USUBJID
            ADSL$USUBJID <- unlist(lapply(strsplit(ADSL$USUBJID, "-", fixed = TRUE), tail, 1))
          })
        )
      } else {
        teal.code::eval_code(
          q2,
          code = bquote({
            ADSL_p <- ADSL
            ANL_p <- .(as.name(dataname))

            ADSL <- ADSL_p[, .(adsl_vars)]
            ANL <- merge(
              x = ADSL,
              y = ANL_p[, .(anl_vars)],
              all.x = FALSE, all.y = FALSE,
              by = c("USUBJID", "STUDYID")
            )
            # only take last part of USUBJID
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
            anno_txt = .(if (length(anno_txt_var) > 0) quote(ADSL[, anno_txt_var]) else NULL),
            xref_line = .(vref_line),
            xtick_at = waiver(),
            xlab = .(x_label),
            title = "Swimlane Plot"
          )
        )
      } else {
        bquote(
          plot <- osprey::g_swimlane(
            bar_id = ADSL[["USUBJID"]],
            bar_length = ADSL[[bar_var]],
            sort_by = .(if (length(sort_var) > 0) {
              quote(ADSL[[sort_var]])
            } else {
              NULL
            }),
            col_by = .(if (length(bar_color_var) > 0) {
              quote(ADSL[[bar_color_var]])
            } else {
              NULL
            }),
            marker_id = ANL[["USUBJID"]],
            marker_pos = .(if (length(marker_pos_var) > 0) {
              quote(ANL[[marker_pos_var]])
            } else {
              NULL
            }),
            marker_shape = .(if (length(marker_shape_var) > 0) {
              quote(ANL[[marker_shape_var]])
            } else {
              NULL
            }),
            marker_shape_opt = .(if (length(marker_shape_var) == 0) {
              NULL
            } else if (length(marker_shape_var) > 0 & all(unique(anl[[marker_shape_var]]) %in% names(marker_shape_opt))) { # nolint: line_length.
              bquote(.(marker_shape_opt))
            } else {
              NULL
            }),
            marker_color = .(if (length(marker_color_var) > 0) {
              quote(ANL[[marker_color_var]])
            } else {
              NULL
            }),
            marker_color_opt = .(if (length(marker_color_var) == 0) {
              NULL
            } else if (length(marker_color_var) > 0 & all(unique(anl[[marker_color_var]]) %in% names(marker_color_opt))) { # nolint: line_length.
              bquote(.(marker_color_opt))
            } else {
              NULL
            }),
            anno_txt = .(if (length(anno_txt_var) > 0) {
              quote(ADSL[, anno_txt_var])
            } else {
              NULL
            }),
            xref_line = .(vref_line),
            xtick_at = waiver(),
            xlab = .(x_label),
            title = "Swimlane Plot"
          )
        )
      }

      q4 <- teal.code::eval_code(q3, code = plot_call)
      teal.code::eval_code(q4, quote(plot))
    })

    plot_r <- reactive(output_q()[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "swimlaneplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      title = paste("R code for", label),
      verbatim_content = reactive(teal.code::get_code(output_q()))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Swimlane Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        if (!is.null(input$sort_var)) {
          card$append_text("Selected Options", "header3")
          card$append_text(paste("Sorted by:", input$sort_var))
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(teal.code::get_code(output_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
