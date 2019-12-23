
#' Spider plot Teal Module
#'
#' Display spider plot as a shiny module
#'
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
#' @param plot_height range of plot height
#' @inheritParams teal.devel::standard_layout
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
#'
#' ADSL <- rADSL
#' ADTR <- rADTR
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTR",  ADTR,
#'                   keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
#'                               foreign = c("STUDYID", "USUBJID"),
#'                               parent = "ADSL")),
#'     code = 'ADSL <- rADSL; ADTR <- rADTR'),
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
#'       href_line = "-20, 0",
#'       anno_txt_var = TRUE,
#'       legend_on = FALSE,
#'       plot_height = c(600, 200, 2000)
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
                            plot_height = c(600, 200, 2000),
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
  stopifnot(is_numeric_vector(plot_height))

  args <- as.list(environment())
  module(
    label = label,
    filters = dataname,
    server = srv_g_spider,
    server_args = list(dataname = dataname, label = label),
    ui = ui_g_spider,
    ui_args = args
  )
}

ui_g_spider <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("spiderplot"))),
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
        label = div("Vertical Reference Line(s)",
                    tags$br(),
                    helpText("Enter numeric value(s) of vertical reference lines, separated by comma (eg. -2, 1)")),
        value = a$vref_line),
      textInput(
        ns("href_line"),
        label = div("Hortizontal Reference Line(s)",
                    tags$br(),
                    helpText("Enter numeric value(s) of horizontal reference lines, separated by comma (eg. -2, 1)")),
        value = a$href_line),
      tags$label("Plot Settings", class = "text-primary", style = "margin-top: 15px;"),
      plot_height_input(id = ns("spiderplot"), value = a$plot_height)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_spider <- function(input, output, session, datasets, dataname, label) {

  vals <- reactiveValues(spiderplot = NULL) # nolint

  callModule(plot_with_height,
             id = "spiderplot",
             plot_height = reactive(input$spiderplot),
             plot_id = session$ns("plot")
  )

  # initialize chunks
  init_chunks()

  # render plot
  output$plot <- renderPlot({

    # get datasets ---

    ADSL_FILTERED <- datasets$get_data("ADSL", reactive = TRUE, filtered = TRUE) # nolint
    ADTR_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) # nolint

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
    chunks_eval()
    validate(need(chunks_is_ok(), "Data could not be constructed."))

    # reference lines preprocessing - vertical
    if (!is.null(vref_line) || vref_line != "") {
      chunks_push(bquote(vref_line <- as.numeric(unlist(strsplit(.(vref_line), ",")))))
    } else {
      chunks_push(quote(vref_line <- NULL))
    }

    # validate vref_line
    validate(need(all(!is.na(vref_line)),
                  "Not all values entered for reference line(s) were numeric"))

    # reference lines preprocessing - horizontal
    if (!is.null(href_line) || href_line != "") {
      chunks_push(bquote(href_line <- as.numeric(unlist(strsplit(.(href_line), ",")))))
    } else {
      chunks_push(quote(href_line <- NULL))
    }

    # validate href_line
    validate(need(all(!is.na(href_line)), "Not all values entered for reference line(s) were numeric"))

    # label
    if (anno_txt_var) {
      chunks_push(quote(lbl <- list(txt_ann = as.factor(ANL_f$USUBJID))))
    } else {
      chunks_push(quote(lbl <- NULL))
    }

    chunks_push_new_line()

    # check
    chunks_eval()
    validate(need(chunks_is_ok(), "Data could not be constructed."))



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
      vref_line = quote(vref_line),
      href_line = quote(href_line),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = bquote(.(legend_on))
    ))

    chunks_eval()

  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Spider Plot",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })
}
