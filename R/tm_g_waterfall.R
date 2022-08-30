#' Teal Module for Waterfall Plot
#'
#' This is teal module that generates a waterfall plot for ADaM data
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
#' @param dataname_tr tumor burden analysis data used in teal module to plot as bar height, needs to
#' be available in the list passed to the \code{data} argument of \code{\link[teal]{init}}
#' @param dataname_rs response analysis data used in teal module to label response parameters, needs to
#' be available in the list passed to the \code{data} argument of \code{\link[teal]{init}}
#' @param bar_paramcd (\code{choices_selected}) parameter in tumor burden data that will be plotted as
#' bar height
#' @param bar_var (\code{choices_selected}) numeric variable from dataset to plot the bar height, e.g., PCHG
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
#' ADSL <- rADSL
#' ADRS <- rADRS
#' ADTR <- rADTR
#'
#' ADSL$SEX <- factor(ADSL$SEX, levels = unique(ADSL$SEX))
#'
#' x <- teal::init(
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
#'       bar_paramcd = choices_selected(c("SLDINV"), "SLDINV"),
#'       bar_var = choices_selected(c("PCHG", "AVAL"), "PCHG"),
#'       bar_color_var = choices_selected(c("ARMCD", "SEX"), "ARMCD"),
#'       bar_color_opt = NULL,
#'       sort_var = choices_selected(c("ARMCD", "SEX"), NULL),
#'       add_label_var_sl = choices_selected(c("SEX", "EOSDY"), NULL),
#'       add_label_paramcd_rs = choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
#'       anno_txt_var_sl = choices_selected(c("SEX", "ARMCD", "BMK1", "BMK2"), NULL),
#'       anno_txt_paramcd_rs = choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
#'       facet_var = choices_selected(c("SEX", "ARMCD", "STRATA1", "STRATA2"), NULL),
#'       href_line = "-30, 20"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(x$ui, x$server)
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
    filters = "all"
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
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_waterfall <- function(id,
                            datasets,
                            reporter,
                            dataname_tr,
                            dataname_rs,
                            bar_color_opt,
                            label,
                            plot_height,
                            plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("bar_var", shinyvalidate::sv_required(message = "Please select Bar Height."))
    iv$add_rule("bar_paramcd", shinyvalidate::sv_required(message = "Please select Tumor Burden Parameter."))
    iv$enable()

    # use teal.code code chunks
    teal.code::init_chunks()

    plot_r <- reactive({
      validate(need(iv$is_valid(), "Misspecification error: please observe red flags in the interface."))
      adsl <- datasets$get_data("ADSL", filtered = TRUE)
      adtr <- datasets$get_data(dataname_tr, filtered = TRUE)
      adrs <- datasets$get_data(dataname_rs, filtered = TRUE)

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

      validate(need(
        length(add_label_paramcd_rs) == 0 || length(add_label_var_sl) == 0,
        "`Add ADSL Label to Bars` and `Add ADRS Label to Bars` fields cannot both have values simultaneously."
      ))

      # validate data rows
      validate_has_data(adsl, min_nrow = 2)
      validate_has_data(adtr, min_nrow = 2)
      validate_has_data(adrs, min_nrow = 2)

      validate_in(
        bar_paramcd,
        adtr$PARAMCD,
        "Tumor burden parameter is not selected or is not found in ADTR PARAMCD."
      )
      if (!is.null(add_label_paramcd_rs)) {
        validate_in(add_label_paramcd_rs, adrs$PARAMCD, "Response parameter cannot be found in ADRS PARAMCD.")
      }
      if (!is.null(anno_txt_paramcd_rs)) {
        validate_in(anno_txt_paramcd_rs, adrs$PARAMCD, "Response parameter cannot be found in ADRS PARAMCD.")
      }

      # get variables
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

      # If reference lines are requested
      href_line <- as_numeric_from_comma_sep_str(href_line)
      validate(need(
        all(!is.na(href_line)),
        "Please enter a comma separated set of numeric values for the reference line(s)"
      ))

      # If gap point is requested
      if (gap_point_val != "" || is.null(gap_point_val)) {
        gap_point_val <- as.numeric(gap_point_val)
        validate(need(
          !anyNA(gap_point_val),
          "Value entered for break point was not numeric"
        ))
      } else {
        gap_point_val <- NULL
      }

      # If y tick is requested
      if (ytick_at != "" || is.null(ytick_at)) {
        ytick_at <- as.numeric(ytick_at)
        validate(need(!anyNA(ytick_at), "Value entered for Y-axis interval was not numeric"))
      } else {
        ytick_at <- 20
      }

      adsl_vars <- unique(
        c("USUBJID", "STUDYID", bar_color_var, sort_var, add_label_var_sl, anno_txt_var_sl, facet_var)
      )
      adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", bar_var))
      adrs_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", "AVALC"))
      adrs_paramcd <- unique(c(add_label_paramcd_rs, anno_txt_paramcd_rs))

      # write data selection to chunks
      adsl_name <- "ADSL"
      adtr_name <- dataname_tr
      adrs_name <- dataname_rs

      assign(adsl_name, adsl)
      assign(adtr_name, adtr)
      assign(adrs_name, adrs)

      # validate data input
      validate_has_variable(adsl, adsl_vars)
      validate_has_variable(adrs, adrs_vars)
      validate_has_variable(adtr, adtr_vars)

      # restart the chunks for showing code
      teal.code::chunks_reset(envir = environment())

      # write variables to chunks
      teal.code::chunks_push(
        id = "variables call",
        expression = bquote({
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
      teal.code::chunks_push_new_line()

      # data processing
      teal.code::chunks_push(
        id = "bar_data call",
        expression = bquote({
          adsl <- .(as.name(adsl_name))[, .(adsl_vars)]
          adtr <- .(as.name(adtr_name))[, .(adtr_vars)] # nolint
          adrs <- .(as.name(adrs_name))[, .(adrs_vars)] # nolint

          bar_tr <- .(as.name(adtr_name)) %>%
            dplyr::filter(PARAMCD == .(bar_paramcd)) %>%
            dplyr::select(USUBJID, .(as.name(bar_var))) %>%
            dplyr::group_by(USUBJID) %>%
            dplyr::slice(which.min(.(as.name(bar_var))))
          bar_data <- adsl %>% dplyr::inner_join(bar_tr, "USUBJID")
        })
      )
      teal.code::chunks_push_new_line()
      teal.code::chunks_safe_eval()
      bar_data <- teal.code::chunks_get_var("bar_data") # nolint

      if (is.null(adrs_paramcd)) {
        teal.code::chunks_push(
          id = "anl call",
          expression = bquote({
            anl <- bar_data
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
          })
        )
      } else {
        teal.code::chunks_push(
          id = "rs_sub call",
          expression = bquote({
            rs_sub <- .(as.name(adrs_name)) %>%
              dplyr::filter(PARAMCD %in% .(adrs_paramcd))
          })
        )
        teal.code::chunks_push_new_line()
        teal.code::chunks_safe_eval()

        rs_sub <- teal.code::chunks_get_var("rs_sub")
        validate_one_row_per_id(rs_sub, key = c("STUDYID", "USUBJID", "PARAMCD"))

        teal.code::chunks_push(
          id = "anl call",
          expression = bquote({
            rs_label <- rs_sub %>%
              dplyr::select(USUBJID, PARAMCD, AVALC) %>%
              tidyr::pivot_wider(names_from = PARAMCD, values_from = AVALC)
            anl <- bar_data %>% dplyr::left_join(rs_label, by = c("USUBJID"))
            anl$USUBJID <- unlist(lapply(strsplit(anl$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
          })
        )
      }
      teal.code::chunks_push_new_line()

      teal.code::chunks_safe_eval()


      # write plotting code to chunks
      anl <- teal.code::chunks_get_var("anl") # nolint

      teal.code::chunks_push(
        id = "g_waterfall call",
        expression = bquote({
          osprey::g_waterfall(
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
            } else if (length(bar_color_var) > 0 & all(unique(anl[[bar_color_var]]) %in% names(bar_color_opt)) == T) {
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
        })
      )

      teal.code::chunks_safe_eval()
    })

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "waterfallplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # Show R Code
    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = datasets$datanames()
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Waterfall")
        card$append_text("Waterfall Plot", "header2")
        card$append_fs(datasets$get_filter_state())
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
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 2L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
