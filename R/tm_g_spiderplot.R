#' Spider plot Teal Module
#'
#' @description
#'
#' Display spider plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param paramcd Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`)
#' variable value designating the studied parameter.
#' @param x_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) x-axis variables
#' @param y_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) y-axis variables
#' @param marker_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) variable dictates marker symbol
#' @param line_colorby_var Either a ([`teal.transform::choices_selected`])
#' `choices_selected` object or a (`[teal.picks::variables()]`) variable dictates line color
#' @param vref_line  vertical reference lines
#' @param href_line horizontal reference lines
#' @param anno_txt_var annotation text
#' @param legend_on boolean value for whether legend is displayed
#' @param xfacet_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`) variable for x facets
#' @param yfacet_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`) variable for y facets
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
tm_g_spiderplot <- function(label,
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
                            plot_height,
                            plot_width,
                            pre_output,
                            post_output,
                            transformators) {
  message("Initializing tm_g_spiderplot")
  UseMethod("tm_g_spiderplot", x_var)
}

#' @rdname tm_g_spiderplot
#' @examples
#' # Example using stream (ADaM) dataset and default method choices selected
#' data <- teal_data() %>%
#'   within({
#'     library(nestcolor)
#'     ADSL <- rADSL
#'     ADTR <- rADTR
#'   })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_spiderplot(
#'       label = "Spider plot",
#'       dataname = "ADTR",
#'       paramcd = choices_selected(
#'         choices = "PARAMCD",
#'         selected = "PARAMCD"
#'       ),
#'       x_var = choices_selected(
#'         choices = c("ADY", "AGE"),
#'         selected = "ADY"
#'       ),
#'       y_var = choices_selected(
#'         choices = c("PCHG", "CHG", "AVAL"),
#'         selected = "PCHG"
#'       ),
#'       marker_var = choices_selected(
#'         choices = c("SEX", "RACE", "USUBJID"),
#'         selected = "SEX"
#'       ),
#'       line_colorby_var = choices_selected(
#'         choices = c("SEX", "USUBJID", "RACE"),
#'         selected = "SEX"
#'       ),
#'       xfacet_var = choices_selected(
#'         choices = c("SEX", "ARM"),
#'         selected = "SEX"
#'       ),
#'       yfacet_var = choices_selected(
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
tm_g_spiderplot.default <- function(label,
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
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_class(paramcd, classes = "choices_selected")
  checkmate::assert_class(x_var, classes = "choices_selected")
  checkmate::assert_class(y_var, classes = "choices_selected")
  checkmate::assert_class(marker_var, classes = "choices_selected")
  checkmate::assert_class(line_colorby_var, classes = "choices_selected")
  checkmate::assert_class(xfacet_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(yfacet_var, classes = "choices_selected", null.ok = TRUE)
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

  paramcd <- teal.picks::as.picks(paramcd)
  x_var <- teal.picks::as.picks(x_var)
  y_var <- teal.picks::as.picks(y_var)
  marker_var <- teal.picks::as.picks(marker_var)
  line_colorby_var <- teal.picks::as.picks(line_colorby_var)
  if (!is.null(xfacet_var)) xfacet_var <- teal.picks::as.picks(xfacet_var)
  if (!is.null(yfacet_var)) yfacet_var <- teal.picks::as.picks(yfacet_var)

  tm_g_spiderplot.pick(
    label = label,
    dataname = dataname,
    paramcd = paramcd,
    x_var = x_var,
    y_var = y_var,
    marker_var = marker_var,
    line_colorby_var = line_colorby_var,
    xfacet_var = xfacet_var,
    yfacet_var = yfacet_var,
    vref_line = vref_line,
    href_line = href_line,
    anno_txt_var = anno_txt_var,
    legend_on = legend_on,
    plot_height = plot_height,
    plot_width = plot_width,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators
  )
}
