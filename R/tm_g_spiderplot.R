#' Spider plot Teal Module
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Display spider plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param x_var x-axis variables
#' @param y_var y-axis variables
#' @param marker_var variable dictates marker symbol
#' @param line_colorby_var variable dictates line color
#' @param vref_line vertical reference lines
#' @param href_line horizontal reference lines
#' @param anno_txt_var annotation text
#' @param legend_on boolean value for whether legend is displayed
#' @param xfacet_var variable for x facets
#' @param yfacet_var variable for y facets
#'
#' @inherit argument_convention return
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#'
#' # Example using stream (ADaM) dataset
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- osprey::rADSL
#' ADTR <- osprey::rADTR
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- osprey::rADSL"),
#'     cdisc_dataset("ADTR", ADTR,
#'       code = "ADTR <- osprey::rADTR",
#'       keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_spiderplot(
#'       label = "Spider plot",
#'       dataname = "ADTR",
#'       paramcd = teal.transform::choices_selected(
#'         choices = "SLDINV",
#'         selected = "SLDINV"
#'       ),
#'       x_var = teal.transform::choices_selected(
#'         choices = "ADY",
#'         selected = "ADY"
#'       ),
#'       y_var = teal.transform::choices_selected(
#'         choices = c("PCHG", "CHG", "AVAL"),
#'         selected = "PCHG"
#'       ),
#'       marker_var = teal.transform::choices_selected(
#'         choices = c("SEX", "RACE", "USUBJID"),
#'         selected = "SEX"
#'       ),
#'       line_colorby_var = teal.transform::choices_selected(
#'         choices = c("SEX", "USUBJID", "RACE"),
#'         selected = "SEX"
#'       ),
#'       xfacet_var = teal.transform::choices_selected(
#'         choices = c("SEX", "ARM"),
#'         selected = "SEX"
#'       ),
#'       yfacet_var = teal.transform::choices_selected(
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
                            post_output = NULL) {
  logger::log_info("Initializing tm_g_spiderplot")
  checkmate::assert_class(paramcd, classes = "choices_selected")
  checkmate::assert_class(x_var, classes = "choices_selected")
  checkmate::assert_class(y_var, classes = "choices_selected")
  checkmate::assert_class(marker_var, classes = "choices_selected")
  checkmate::assert_class(line_colorby_var, classes = "choices_selected")
  checkmate::assert_class(xfacet_var, classes = "choices_selected")
  checkmate::assert_class(yfacet_var, classes = "choices_selected")
  checkmate::assert_string(vref_line)
  checkmate::assert_string(href_line)
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
    server_args = list(dataname = dataname, label = label, plot_height = plot_height, plot_width = plot_width),
    ui = ui_g_spider,
    ui_args = args
  )
}

ui_g_spider <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("spiderplot"))
      ),
      encoding = div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis data:", tags$code(a$dataname)),
        div(
          class = "pretty-left-border",
          teal.widgets::optionalSelectInput(
            ns("paramcd"),
            paste("Parameter - from", a$dataname),
            a$paramcd$choices,
            a$paramcd$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("x_var"),
            "X-axis Variable",
            a$x_var$choices,
            a$x_var$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("y_var"),
            "Y-axis Variable",
            a$y_var$choices,
            a$y_var$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("line_colorby_var"),
            "Color By Variable (Line)",
            a$line_colorby_var$choices,
            a$line_colorby_var$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("marker_var"),
            "Marker Symbol By Variable",
            a$marker_var$choices,
            a$marker_var$selected,
            multiple = FALSE
          ),
          teal.widgets::optionalSelectInput(
            ns("xfacet_var"),
            "X-facet By Variable",
            a$xfacet_var$choices,
            a$xfacet_var$selected,
            multiple = TRUE
          ),
          teal.widgets::optionalSelectInput(
            ns("yfacet_var"),
            "Y-facet By Variable",
            a$yfacet_var$choices,
            a$yfacet_var$selected,
            multiple = TRUE
          )
        ),
        checkboxInput(
          ns("anno_txt_var"),
          "Add subject ID label",
          value = a$anno_txt_var
        ),
        checkboxInput(
          ns("legend_on"),
          "Add legend",
          value = a$legend_on
        ),
        textInput(
          ns("vref_line"),
          label = div(
            "Vertical Reference Line(s)",
            tags$br(),
            helpText("Enter numeric value(s) of vertical reference lines, separated by comma (eg. -2, 1)")
          ),
          value = a$vref_line
        ),
        textInput(
          ns("href_line"),
          label = div(
            "Hortizontal Reference Line(s)",
            tags$br(),
            helpText("Enter numeric value(s) of horizontal reference lines, separated by comma (eg. -2, 1)")
          ),
          value = a$href_line
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

srv_g_spider <- function(id, data, filter_panel_api, reporter, dataname, label, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      ADSL <- data[["ADSL"]]() # nolint
      ADTR <- data[[dataname]]() # nolint

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("paramcd", shinyvalidate::sv_required(
        message = "Parameter is required"
      ))
      iv$add_rule("x_var", shinyvalidate::sv_required(
        message = "X Axis Variable is required"
      ))
      iv$add_rule("y_var", shinyvalidate::sv_required(
        message = "Y Axis Variable is required"
      ))
      iv$add_rule("line_colorby_var", shinyvalidate::sv_required(
        message = "Color Variable is required"
      ))
      iv$add_rule("marker_var", shinyvalidate::sv_required(
        message = "Marker Symbol Variable is required"
      ))
      fac_dupl <- function(value, other) {
        if (length(value) * length(other) > 0L && anyDuplicated(c(value, other))) {
          "X- and Y-facet Variables must not overlap"
        }
      }
      iv$add_rule("xfacet_var", fac_dupl, other = input$yfacet_var)
      iv$add_rule("yfacet_var", fac_dupl, other = input$xfacet_var)
      iv$add_rule("vref_line", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Vertical Reference Line(s) are invalid"
      })
      iv$add_rule("href_line", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Horizontal Reference Line(s) are invalid"
      })
      iv$enable()
    })

    vals <- reactiveValues(spiderplot = NULL) # nolint

    # render plot
    output_q <- reactive({
      # get datasets ---
      ADSL <- data[["ADSL"]]() # nolint
      ADTR <- data[[dataname]]() # nolint

      teal::validate_inputs(iv())

      teal::validate_has_data(ADSL, min_nrow = 1, msg = sprintf("%s data has zero rows", "ADSL"))
      teal::validate_has_data(ADTR, min_nrow = 1, msg = sprintf("%s data has zero rows", dataname))

      paramcd <- input$paramcd # nolint
      x_var <- input$x_var
      y_var <- input$y_var
      marker_var <- input$marker_var
      line_colorby_var <- input$line_colorby_var
      anno_txt_var <- input$anno_txt_var
      legend_on <- input$legend_on # nolint
      xfacet_var <- input$xfacet_var
      yfacet_var <- input$yfacet_var
      vref_line <- input$vref_line
      href_line <- input$href_line

      # reference lines preprocessing
      vref_line <- as_numeric_from_comma_sep_str(vref_line)
      href_line <- as_numeric_from_comma_sep_str(href_line)

      # define variables ---
      # if variable is not in ADSL, then take from domain VADs
      varlist <- c(xfacet_var, yfacet_var, marker_var, line_colorby_var)
      varlist_from_adsl <- varlist[varlist %in% names(ADSL)]
      varlist_from_anl <- varlist[!varlist %in% names(ADSL)]

      adsl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_adsl)) # nolint
      adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", x_var, y_var, varlist_from_anl))

      # preprocessing of datasets to qenv ---

      # vars definition
      adtr_vars <- adtr_vars[adtr_vars != "None"]
      adtr_vars <- adtr_vars[!is.null(adtr_vars)]

      # merge
      q1 <- teal.code::eval_code(
        teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)),
        code = bquote({
          ADSL <- ADSL[, .(adsl_vars)] %>% as.data.frame() # nolint
          ADTR <- .(as.name(dataname))[, .(adtr_vars)] %>% as.data.frame() # nolint

          ANL <- merge(ADSL, ADTR, by = c("USUBJID", "STUDYID")) # nolint
          ANL <- ANL %>% # nolint
            group_by(USUBJID, PARAMCD) %>%
            arrange(ANL[, .(x_var)]) %>%
            as.data.frame()
        })
      )

      # format and filter
      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
          ANL_f <- ANL %>% # nolint
            filter(PARAMCD == .(paramcd)) %>%
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

      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          plot <- osprey::g_spiderplot(
            marker_x = ANL_f[, .(x_var)],
            marker_id = ANL_f$USUBJID,
            marker_y = ANL_f[, .(y_var)],
            line_colby = .(if (line_colorby_var != "None") {
              bquote(ANL_f[, .(line_colorby_var)])
            } else {
              NULL
            }),
            marker_shape = .(if (marker_var != "None") {
              bquote(ANL_f[, .(marker_var)])
            } else {
              NULL
            }),
            marker_size = 4,
            datalabel_txt = lbl,
            facet_rows = .(if (!is.null(yfacet_var)) {
              bquote(data.frame(ANL_f[, .(yfacet_var)]))
            } else {
              NULL
            }),
            facet_columns = .(if (!is.null(xfacet_var)) {
              bquote(data.frame(ANL_f[, .(xfacet_var)]))
            } else {
              NULL
            }),
            vref_line = .(vref_line),
            href_line = .(href_line),
            x_label = if (is.null(formatters::var_labels(ADTR[.(x_var)], fill = FALSE))) {
              .(x_var)
            } else {
              formatters::var_labels(ADTR[.(x_var)], fill = FALSE)
            },
            y_label = if (is.null(formatters::var_labels(ADTR[.(y_var)], fill = FALSE))) {
              .(y_var)
            } else {
              formatters::var_labels(ADTR[.(y_var)], fill = FALSE)
            },
            show_legend = .(legend_on)
          )

          plot
        })
      )
    })

    plot_r <- reactive(output_q()[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "spiderplot",
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
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Spider Plot")
        card$append_text("Spider Plot", "header2")
        if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
        if (!is.null(input$paramcd) || !is.null(input$xfacet_var) || !is.null(input$yfacet_var)) {
          card$append_text("Selected Options", "header3")
        }
        if (!is.null(input$paramcd)) {
          card$append_text(paste0("Parameter - (from ", dataname, "): ", input$paramcd, "."))
        }
        if (!is.null(input$xfacet_var)) {
          card$append_text(paste0("Faceted horizontally by: ", paste(input$xfacet_var, collapse = ", "), "."))
        }
        if (!is.null(input$yfacet_var)) {
          card$append_text(paste0("Faceted vertically by: ", paste(input$yfacet_var, collapse = ", "), "."))
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(output_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
