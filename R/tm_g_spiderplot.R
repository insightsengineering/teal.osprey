
#' Spider plot Teal Module
#'
#' Display spider plot as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param paramcd single selected endpoint filtered with \code{PARAMCD} variable
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
#' @return an \code{\link[teal]{module}} object
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#'
#' #Example using stream (ADaM) dataset
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- rADSL
#' ADTR <- rADTR
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- rADSL"),
#'     cdisc_dataset("ADTR",  ADTR, code = "ADTR <- rADTR",
#'                   keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
#'                               foreign = c("STUDYID", "USUBJID"),
#'                               parent = "ADSL")),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_spiderplot(
#'       label = "Spider plot",
#'       dataname = "ADTR",
#'       paramcd = choices_selected(choices = "SLDINV", selected = "SLDINV"),
#'       x_var = choices_selected(choices = "ADY", selected = "ADY"),
#'       y_var = choices_selected(choices = c("PCHG", "CHG", "AVAL"), selected = "PCHG"),
#'       marker_var = choices_selected(choices = c("SEX", "RACE", "USUBJID"), selected = "SEX"),
#'       line_colorby_var = choices_selected(choices = c("SEX","USUBJID", "RACE"), selected = "SEX"),
#'       xfacet_var = choices_selected(choices = c("SEX", "ARM"), selected = "SEX"),
#'       yfacet_var = choices_selected(choices = c("SEX", "ARM"), selected = "ARM"),
#'       vref_line = "10, 37",
#'       href_line = "-20, 0"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
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

  stopifnot(is.choices_selected(paramcd))
  stopifnot(is.choices_selected(x_var))
  stopifnot(is.choices_selected(y_var))
  stopifnot(is.choices_selected(marker_var))
  stopifnot(is.choices_selected(line_colorby_var))
  stopifnot(is.choices_selected(xfacet_var))
  stopifnot(is.choices_selected(yfacet_var))
  stopifnot(is_character_single(vref_line))
  stopifnot(is_character_single(href_line))
  stopifnot(is_logical_single(anno_txt_var))
  stopifnot(is_logical_single(legend_on))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  module(
    label = label,
    filters = dataname,
    server = srv_g_spider,
    server_args = list(dataname = dataname, label = label, plot_height = plot_height, plot_width = plot_width),
    ui = ui_g_spider,
    ui_args = args
  )
}

ui_g_spider <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("spiderplot"), height = a$plot_height, width = a$plot_width)
      ),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      div(
        style = "border-left: 3px solid #e3e3e3; padding-left: 0.6em; border-radius: 5px; margin-left: -0.6em;",
        optionalSelectInput(
          ns("paramcd"),
          "Parameter - from ADTR",
          a$paramcd$choices,
          a$paramcd$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("x_var"),
          "X-axis Variable",
          a$x_var$choices,
          a$x_var$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("y_var"),
          "Y-axis Variable",
          a$y_var$choices,
          a$y_var$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("line_colorby_var"),
          "Color By Variable (Line)",
          a$line_colorby_var$choices,
          a$line_colorby_var$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("marker_var"),
          "Marker Symbol By Variable",
          a$marker_var$choices,
          a$marker_var$selected,
          multiple = FALSE),
        optionalSelectInput(
          ns("xfacet_var"),
          "X-facet By Variable",
          a$xfacet_var$choices,
          a$xfacet_var$selected,
          multiple = TRUE),
        optionalSelectInput(
          ns("yfacet_var"),
          "Y-facet By Variable",
          a$yfacet_var$choices,
          a$yfacet_var$selected,
          multiple = TRUE)
      ),
      checkboxInput(
        ns("anno_txt_var"),
        "Add subject ID label",
        value = a$anno_txt_var),
      checkboxInput(
        ns("legend_on"),
        "Add legend",
        value = a$legend_on),
      textInput(
        ns("vref_line"),
        label = div(
          "Vertical Reference Line(s)",
          tags$br(),
          helpText("Enter numeric value(s) of vertical reference lines, separated by comma (eg. -2, 1)")),
        value = a$vref_line),
      textInput(
        ns("href_line"),
        label = div(
          "Hortizontal Reference Line(s)",
          tags$br(),
          helpText("Enter numeric value(s) of horizontal reference lines, separated by comma (eg. -2, 1)")),
        value = a$href_line)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_spider <- function(input, output, session, datasets, dataname, label, plot_height, plot_width) {

  vals <- reactiveValues(spiderplot = NULL) # nolint

  # initialize chunks
  init_chunks()

  # render plot
  plot_r <- reactive({

    # get datasets ---

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ADTR_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    adtr_name <- paste0(dataname, "_FILTERED")
    assign(adtr_name, ADTR_FILTERED) # so that we can refer to the 'correct' data name


    # restart chunks & include current environment ---

    chunks_reset(envir = environment())


    # get inputs ---

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

    validate(need(paramcd, "`Parameter - from ADTR` field is empty"))
    validate(need(x_var, "`X-axis Variable` field is empty"))
    validate(need(y_var, "`Y-axis Variable` field is empty"))
    validate(need(marker_var, "`Marker Symbol By Variable` field is empty"))
    validate(need(line_colorby_var, "`Color By Variable (Line)` field is empty"))
    validate(need(nrow(ADSL_FILTERED) > 0, "ADSL data has zero rows"))
    validate(need(nrow(ADTR_FILTERED) > 0, "ADTR data has zero rows"))

    # define variables ---

    # if variable is not in ADSL, then take from domain VADs
    varlist <- c(xfacet_var, yfacet_var, marker_var, line_colorby_var)
    varlist_from_adsl <- varlist[varlist %in% names(ADSL_FILTERED)]
    varlist_from_anl <- varlist[!varlist %in% names(ADSL_FILTERED)]

    adsl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_adsl)) # nolint
    adtr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", x_var, y_var, varlist_from_anl))

    # preprocessing of datasets to chunks ---

    # vars definition
    adtr_vars <- adtr_vars[adtr_vars != "None"]
    adtr_vars <- adtr_vars[!is.null(adtr_vars)]

    # merge
    chunks_push(bquote({
      ADSL <- ADSL_FILTERED[, .(adsl_vars)] %>% as.data.frame() # nolint
      ADTR <- .(as.name(adtr_name))[, .(adtr_vars)] %>% as.data.frame() # nolint

      ANL <- merge(ADSL, ADTR, by = c("USUBJID", "STUDYID")) # nolint
      ANL <- ANL %>% group_by(USUBJID, PARAMCD) %>% arrange(ANL[, .(x_var)]) %>% as.data.frame() # nolint
    }))

    chunks_push_new_line()

    # format and filter
    chunks_push(bquote({
      ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
      ANL_f <- ANL %>% filter(PARAMCD == .(paramcd)) %>% as.data.frame() # nolint
    }))

    chunks_push_new_line()

    # check
    chunks_safe_eval()

    # reference lines preprocessing - vertical
    vref_line <- as_numeric_from_comma_sep_str(vref_line)
    validate(need(all(!is.na(vref_line)),
      "Please enter a comma separated set of numeric values for the vertical reference line(s)"))

    # reference lines preprocessing - horizontal
    href_line <- as_numeric_from_comma_sep_str(href_line)
    validate(need(all(!is.na(href_line)),
      "Please enter a comma separated set of numeric values for the horizontal reference line(s)"))

    # label
    if (anno_txt_var) {
      chunks_push(quote(lbl <- list(txt_ann = as.factor(ANL_f$USUBJID))))
    } else {
      chunks_push(quote(lbl <- NULL))
    }

    chunks_push_new_line()

    # check
    chunks_safe_eval()

    # plot code to chunks ---

    chunks_push(call(
      "g_spiderplot",
      marker_x = bquote(ANL_f[, .(x_var)]),
      marker_id = quote(ANL_f$USUBJID),
      marker_y = bquote(ANL_f[, .(y_var)]),
      line_colby = if (line_colorby_var != "None") {
        bquote(ANL_f[, .(line_colorby_var)])
      } else {
        NULL
      },
      marker_shape = if (marker_var != "None") {
        bquote(ANL_f[, .(marker_var)])
      } else {
        NULL
      },
      marker_size = 4,
      datalabel_txt = quote(lbl),
      facet_rows = if (!is.null(yfacet_var)) {
        bquote(data.frame(ANL_f[, .(yfacet_var)]))
      } else {
        NULL
      },
      facet_columns = if (!is.null(xfacet_var)) {
        bquote(data.frame(ANL_f[, .(xfacet_var)]))
      } else {
        NULL
      },
      vref_line = bquote(.(vref_line)),
      href_line = bquote(.(href_line)),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = bquote(.(legend_on))
    ))

    chunks_safe_eval()

  })

  callModule(
    plot_with_settings_srv,
    id = "spiderplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = label
  )
}
