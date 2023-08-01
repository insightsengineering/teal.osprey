#' Teal Module for Waterfall Plot
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is teal module that generates a waterfall plot for `ADaM` data
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param dataname_tr tumor burden analysis data used in teal module to plot as bar height, needs to
#' be available in the list passed to the \code{data} argument of \code{\link[teal]{init}}
#' @param dataname_rs response analysis data used in teal module to label response parameters, needs to
#' be available in the list passed to the \code{data} argument of \code{\link[teal]{init}}
#' @param bar_paramcd (\code{choices_selected}) parameter in tumor burden data that will be plotted as
#' bar height
#' @param bar_var (\code{choices_selected}) numeric variable from dataset to plot the bar height, e.g., `PCHG`
#' @param bar_color_var (\code{choices_selected}) color by variable (subject level), \code{None} corresponds
#' to \code{NULL}
#' @param bar_color_opt aesthetic values to map color values (named vector to map color values to each name).
#' If not \code{NULL}, please make sure this contains all possible values for \code{bar_color_var} values,
#' otherwise color will be assigned by \code{ggplot} default, please note that \code{NULL} needs to be specified
#' in this case
#' @param sort_var (\code{choices_selected}) sort by variable (subject level), \code{None} corresponds
#' to \code{NULL}
#' @param add_label_var_sl (\code{choices_selected}) add label to bars (subject level), \code{None}
#' corresponds to \code{NULL}
#' @param add_label_paramcd_rs (\code{choices_selected}) add label to bars (response dataset), \code{None}
#' corresponds to \code{NULL}. At least one of \code{add_label_var_sl} and \code{add_label_paramcd_rs} needs
#' to be \code{NULL}
#' @param anno_txt_var_sl (\code{choices_selected}) subject level variables to be displayed in the annotation
#' table, default is \code{NULL}
#' @param anno_txt_paramcd_rs (\code{choices_selected}) analysis dataset variables to be displayed in the
#' annotation table, default is \code{NULL}
#' @param facet_var (\code{choices_selected}) facet by variable (subject level), \code{None} corresponds to
#' \code{NULL}
#' @param ytick_at bar height axis interval, default is \code{20}
#' @param href_line numeric vector to plot horizontal reference lines, default is \code{NULL}
#' @param gap_point_val singular numeric value for adding bar break when some bars are significantly higher
#' than others, default is \code{NULL}
#' @param show_value boolean of whether value of bar height is shown, default is \code{TRUE}
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @template author_qit3
#' @author houx14 \email{houx14@gene.com}
#'
#' @examples
#' library(nestcolor)
#' ADSL <- osprey::rADSL
#' ADRS <- osprey::rADRS
#' ADTR <- osprey::rADTR
#'
#' ADSL$SEX <- factor(ADSL$SEX, levels = unique(ADSL$SEX))
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = "ADSL <- rADSL
#'               ADSL$SEX <- factor(ADSL$SEX, levels = unique(ADSL$SEX))"
#'     ),
#'     cdisc_dataset("ADRS", ADRS, code = "ADRS <- rADRS"),
#'     cdisc_dataset("ADTR", ADTR,
#'       code = " ADTR <- rADTR",
#'       c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_waterfall(
#'       label = "Waterfall",
#'       dataname_tr = "ADTR",
#'       dataname_rs = "ADRS",
#'       bar_paramcd = teal.transform::choices_selected(c("SLDINV"), "SLDINV"),
#'       bar_var = teal.transform::choices_selected(c("PCHG", "AVAL"), "PCHG"),
#'       bar_color_var = teal.transform::choices_selected(c("ARMCD", "SEX"), "ARMCD"),
#'       bar_color_opt = NULL,
#'       sort_var = teal.transform::choices_selected(c("ARMCD", "SEX"), NULL),
#'       add_label_var_sl = teal.transform::choices_selected(c("SEX", "EOSDY"), NULL),
#'       add_label_paramcd_rs = teal.transform::choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
#'       anno_txt_var_sl = teal.transform::choices_selected(c("SEX", "ARMCD", "BMK1", "BMK2"), NULL),
#'       anno_txt_paramcd_rs = teal.transform::choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
#'       facet_var = teal.transform::choices_selected(c("SEX", "ARMCD", "STRATA1", "STRATA2"), NULL),
#'       href_line = "-30, 20"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_waterfall <- function(label,
                           dataname_tr = "ADTR",
                           dataname_rs = "ADRS",
                           bar_paramcd,
                           bar_var,
                           bar_color_var,
                           bar_color_opt = NULL,
                           sort_var,
                           add_label_var_sl,
                           add_label_paramcd_rs,
                           anno_txt_var_sl,
                           anno_txt_paramcd_rs,
                           facet_var,
                           ytick_at = 20,
                           href_line = NULL,
                           gap_point_val = NULL,
                           show_value = TRUE,
                           plot_height = c(1200L, 400L, 5000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL) {
  logger::log_info("Initializing tm_g_waterfall")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname_tr)
  checkmate::assert_string(dataname_rs)
  checkmate::assert_class(bar_paramcd, classes = "choices_selected")
  checkmate::assert_class(bar_var, classes = "choices_selected")
  checkmate::assert_class(bar_color_var, classes = "choices_selected")
  checkmate::assert_class(sort_var, classes = "choices_selected")
  checkmate::assert_class(anno_txt_var_sl, classes = "choices_selected")
  checkmate::assert_class(anno_txt_paramcd_rs, classes = "choices_selected")
  checkmate::assert_class(facet_var, classes = "choices_selected")
  checkmate::assert_class(add_label_var_sl, classes = "choices_selected")
  checkmate::assert_class(add_label_paramcd_rs, classes = "choices_selected")
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
    ui = ui_g_waterfall,
    ui_args = args,
    server = srv_g_waterfall,
    server_args = list(
      dataname_tr = dataname_tr,
      dataname_rs = dataname_rs,
      label = label,
      bar_color_opt = bar_color_opt,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    datanames = "all"
  )
}

ui_g_waterfall <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("waterfallplot"))
    ),
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname_tr), tags$code(a$dataname_rs)),
      teal.widgets::optionalSelectInput(
        ns("bar_paramcd"),
        "Tumor Burden Parameter",
        choices = a$bar_paramcd$choices,
        selected = a$bar_paramcd$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("bar_var"),
        "Bar Height",
        choices = a$bar_var$choices,
        selected = a$bar_var$selected,
        multiple = FALSE,
        label_help = helpText("Tumor change variable from ", tags$code("ADTR"))
      ),
      teal.widgets::optionalSelectInput(
        ns("bar_color_var"),
        "Bar Color",
        choices = a$bar_color_var$choices,
        selected = a$bar_color_var$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("sort_var"),
        "Sort by",
        choices = a$sort_var$choices,
        selected = a$sort_var$selected,
        multiple = FALSE,
        label_help = helpText("from ", tags$code("ADSL"))
      ),
      teal.widgets::optionalSelectInput(
        ns("add_label_var_sl"),
        "Add ADSL Label to Bars",
        choices = a$add_label_var_sl$choices,
        selected = a$add_label_var_sl$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("add_label_paramcd_rs"),
        "Add ADRS Label to Bars",
        choices = a$add_label_paramcd_rs$choices,
        selected = a$add_label_paramcd_rs$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("anno_txt_var_sl"),
        "Annotation Variables",
        choices = a$anno_txt_var_sl$choices,
        selected = a$anno_txt_var_sl$selected,
        multiple = TRUE,
        label_help = helpText("from ", tags$code("ADSL"))
      ),
      teal.widgets::optionalSelectInput(
        ns("anno_txt_paramcd_rs"),
        "Annotation Parameters",
        choices = a$anno_txt_paramcd_rs$choices,
        selected = a$anno_txt_paramcd_rs$selected,
        multiple = TRUE,
        label_help = helpText("from ", tags$code("ADRS"))
      ),
      teal.widgets::optionalSelectInput(
        ns("facet_var"),
        "Facet by",
        choices = a$facet_var$choices,
        selected = NULL,
        multiple = FALSE,
        label_help = helpText("from ", tags$code("ADSL"))
      ),
      checkboxInput(
        ns("show_value"),
        "Add Bar Height Value",
        value = a$show_value
      ),
      textInput(
        ns("href_line"),
        label = div(
          "Horizontal Reference Line(s)",
          tags$br(),
          helpText("Enter numeric value(s) of reference lines, separated by comma (eg. -10, 20)")
        ),
        value = a$href_line
      ),
      textInput(
        ns("ytick_at"),
        label = div(
          "Y-axis Interval",
          tags$br(),
          helpText("Enter a numeric value of Y axis interval")
        ),
        value = a$ytick_at
      ),
      textInput(
        ns("gap_point_val"),
        label = div(
          "Break High Bars",
          tags$br(),
          helpText("Enter a numeric value to break very high bars")
        ),
        value = a$gap_point_val
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_waterfall <- function(id,
                            data,
                            filter_panel_api,
                            reporter,
                            dataname_tr,
                            dataname_rs,
                            bar_color_opt,
                            label,
                            plot_height,
                            plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      adsl <- data[["ADSL"]]()
      adtr <- data[[dataname_tr]]()
      adrs <- data[[dataname_rs]]()

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("bar_var", shinyvalidate::sv_required(
        message = "Bar Height is required"
      ))
      iv$add_rule("bar_paramcd", shinyvalidate::sv_required(
        message = "Tumor Burden Parameter is required"
      ))
      iv$add_rule("bar_paramcd", shinyvalidate::sv_in_set(
        set = adtr$PARAMCD,
        message_fmt = "All values of Tumor Burden Parameter must be elements of ADTR PARAMCD"
      ))
      iv$add_rule("add_label_paramcd_rs", shinyvalidate::sv_optional())
      iv$add_rule("add_label_paramcd_rs", shinyvalidate::sv_in_set(
        set = adrs$PARAMCD,
        message_fmt = "ADRS Label must be an element of ADRS PARAMCD"
      ))
      rule_excl <- function(value, other) {
        if (length(value) > 0L && length(other) > 0L) {
          "Only one \"Label to Bars\" is allowed"
        }
      }
      iv$add_rule("add_label_paramcd_rs", rule_excl, other = input$add_label_var_sl)
      iv$add_rule("add_label_var_sl", rule_excl, other = input$add_label_paramcd_rs)
      iv$add_rule("anno_txt_paramcd_rs", shinyvalidate::sv_optional())
      iv$add_rule("anno_txt_paramcd_rs", shinyvalidate::sv_in_set(
        set = adrs$PARAMCD,
        message_fmt = "Annotation Parameters must be elements of ADRS PARAMCD"
      ))
      iv$add_rule("href_line", shinyvalidate::sv_optional())
      iv$add_rule("href_line", ~ if (anyNA(suppressWarnings(as_numeric_from_comma_sep_str(.)))) {
        "Horizontal Reference Line(s) are invalid"
      })
      iv$add_rule("ytick_at", shinyvalidate::sv_required(
        message = "Y-axis Interval is required"
      ))
      iv$add_rule("ytick_at", ~ if (!checkmate::test_number(suppressWarnings(as.numeric(.)), lower = 1)) {
        "Y-axis Interval must be a single positive number"
      })
      iv$add_rule("gap_point_val", shinyvalidate::sv_optional())
      iv$add_rule("gap_point_val", ~ if (!checkmate::test_number(suppressWarnings(as.numeric(.)), lower = 1)) {
        "Break High Bars must be a single positive number"
      })
      iv$enable()
      iv
    })

    output_q <- reactive({
      adsl <- data[["ADSL"]]()
      adtr <- data[[dataname_tr]]()
      adrs <- data[[dataname_rs]]()

      # validate data rows
      teal::validate_has_data(adsl, min_nrow = 2)
      teal::validate_has_data(adtr, min_nrow = 2)
      teal::validate_has_data(adrs, min_nrow = 2)

      adsl_vars <- unique(
        c(
          "USUBJID", "STUDYID",
          input$bar_color_var, input$sort_var, input$add_label_var_sl, input$anno_txt_var_sl, input$facet_var
        )
      )
      adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", input$bar_var))
      adrs_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", "AVALC"))
      adrs_paramcd <- unique(c(input$add_label_paramcd_rs, input$anno_txt_paramcd_rs))

      # validate data input
      teal::validate_has_variable(adsl, adsl_vars)
      teal::validate_has_variable(adrs, adrs_vars)
      teal::validate_has_variable(adtr, adtr_vars)

      teal::validate_inputs(iv())

      # get variables
      bar_var <- input$bar_var
      bar_paramcd <- input$bar_paramcd
      add_label_var_sl <- input$add_label_var_sl
      add_label_paramcd_rs <- input$add_label_paramcd_rs
      anno_txt_var_sl <- input$anno_txt_var_sl
      anno_txt_paramcd_rs <- input$anno_txt_paramcd_rs
      ytick_at <- input$ytick_at
      href_line <- input$href_line
      gap_point_val <- input$gap_point_val
      show_value <- input$show_value # nolint
      href_line <- suppressWarnings(as_numeric_from_comma_sep_str(href_line))

      if (gap_point_val == "") {
        gap_point_val <- NULL
      } else {
        gap_point_val <- as.numeric(gap_point_val)
      }
      ytick_at <- as.numeric(ytick_at)

      bar_color_var <- if (!is.null(input$bar_color_var) &&
        input$bar_color_var != "None" &&
        input$bar_color_var != "") {
        input$bar_color_var
      } else {
        NULL
      }
      sort_var <- if (!is.null(input$sort_var) && input$sort_var != "None" && input$sort_var != "") {
        input$sort_var
      } else {
        NULL
      }
      facet_var <- if (!is.null(input$facet_var) && input$facet_var != "None" && input$facet_var != "") {
        input$facet_var
      } else {
        NULL
      }

      # write variables to qenv
      q1 <- teal.code::eval_code(
        teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)),
        code = bquote({
          bar_var <- .(bar_var)
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

      # data processing
      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          adsl <- ADSL[, .(adsl_vars)]
          adtr <- .(as.name(dataname_tr))[, .(adtr_vars)] # nolint
          adrs <- .(as.name(dataname_rs))[, .(adrs_vars)] # nolint

          bar_tr <- .(as.name(dataname_tr)) %>%
            dplyr::filter(PARAMCD == .(bar_paramcd)) %>%
            dplyr::select(USUBJID, .(as.name(bar_var))) %>%
            dplyr::group_by(USUBJID) %>%
            dplyr::slice(which.min(.(as.name(bar_var))))
          bar_data <- adsl %>% dplyr::inner_join(bar_tr, "USUBJID")
        })
      )

      q1 <- if (is.null(adrs_paramcd)) {
        teal.code::eval_code(
          q1,
          code = bquote({
            anl <- bar_data
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
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
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
          })
        )
      }

      # write plotting code to qenv
      anl <- q1[["anl"]] # nolint

      q1 <- teal.code::eval_code(
        q1,
        code = bquote({
          plot <- osprey::g_waterfall(
            bar_id = anl[["USUBJID"]],
            bar_height = anl[[bar_var]],
            sort_by = .(if (length(sort_var) > 0) {
              quote(anl[[sort_var]])
            } else {
              NULL
            }),
            col_by = .(if (length(bar_color_var) > 0) {
              quote(anl[[bar_color_var]])
            } else {
              NULL
            }),
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
            facet_by = .(if (length(facet_var) > 0) {
              quote(as.factor(anl[[facet_var]]))
            } else {
              NULL
            }),
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
          plot
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

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    # Show R Code
    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      title = paste("R code for", label),
      verbatim_content = reactive(teal.code::get_code(output_q()))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal::TealReportCard$new()
        card$set_name("Waterfall")
        card$append_text("Waterfall Plot", "header2")
        if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
        card$append_text("Selected Options", "header3")
        card$append_text(paste0("Tumor Burden Parameter: ", input$bar_paramcd, "."))
        if (!is.null(input$sort_var)) {
          card$append_text(paste0("Sorted by: ", input$sort_var, "."))
        }
        if (!is.null(input$facet_var)) {
          card$append_text(paste0("Faceted by: ", paste(input$facet_var, collapse = ", "), "."))
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
