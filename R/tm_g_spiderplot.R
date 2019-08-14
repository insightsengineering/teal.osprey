
#' Spiderplot Teal Module
#'
#' Display spiderplot as a shiny module
#'
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param paramcd single selected endpoint filtered with \code{PARAMCD} variable
#' @param paramcd_choices vector with \code{paramcd} choices
#' @param x_var x-axis variables
#' @param x_var_choices vector with \code{x_var} choices
#' @param y_var y-axis variables
#' @param y_var_choices vector with \code{y_var} choices
#' @param marker_var variable dictates marker symbol
#' @param marker_var_choices vector with \code{marker_var} choices
#' @param line_colorby_var variable dictates line color
#' @param line_colorby_var_choices vector with \code{line_colorby_var} choices
#' @param vref_line vertical reference lines
#' @param href_line horizontal reference lines
#' @param anno_txt_var annotation text
#' @param legend_on boolean value for whether legend is displayed
#' @param xfacet_var variable for x facets
#' @param xfacet_var_choices vector with \code{xfacet_var} choices
#' @param yfacet_var variable for y facets
#' @param yfacet_var_choices vector with \code{yfacet_var} choices
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
#' \dontrun{
#' #Example spiderplot
#' library(dplyr)
#'
#' data("rADSL")
#' data("rADTR")
#'
#' ASL <- rADSL
#' ATR <- rADTR
#'
#' app <- teal::init(
#'   data = cdisc_data(ASL = ASL, ATR = ATR),
#'   modules = root_modules(
#'     tm_g_spiderplot(
#'        label = "Spiderplot",
#'        dataname = "ATR",
#'        paramcd = "SLDINV",
#'        paramcd_choices = "SLDINV",
#'        x_var = "ADY",
#'        x_var_choices = "ADY",
#'        y_var = "PCHG",
#'        y_var_choices = c("PCHG", "CHG", "AVAL"),
#'        marker_var = "SEX",
#'        marker_var_choices = c("SEX", "RACE", "USUBJID"),
#'        line_colorby_var = "SEX",
#'        line_colorby_var_choices = c("SEX","USUBJID", "RACE"),
#'        vref_line = "10, 37",
#'        href_line = "-20, 0",
#'        anno_txt_var = TRUE,
#'        legend_on = FALSE,
#'        xfacet_var = "SEX",
#'        xfacet_var_choices = c("SEX", "ARM"),
#'        yfacet_var = "ARM",
#'        yfacet_var_choices = c("ARM", "SEX"),
#'        plot_height = c(600, 200, 2000)
#'    )
#'   )
#' )
#'
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_spiderplot <- function(label,
                            dataname,
                            paramcd,
                            paramcd_choices = paramcd,
                            x_var,
                            x_var_choices = x_var,
                            y_var,
                            y_var_choices = y_var,
                            marker_var,
                            marker_var_choices = marker_var,
                            line_colorby_var,
                            line_colorby_var_choices = line_colorby_var_choices,
                            vref_line = NULL,
                            href_line = NULL,
                            anno_txt_var,
                            legend_on = FALSE,
                            xfacet_var = NULL,
                            xfacet_var_choices = xfacet_var,
                            yfacet_var = NULL,
                            yfacet_var_choices = yfacet_var,
                            plot_height,
                            pre_output = NULL,
                            post_output = NULL) {

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
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      div(
        style = "border-left: 3px solid #e3e3e3; padding-left: 0.6em; border-radius: 5px; margin-left: -0.6em;",
        optionalSelectInput(
          ns("paramcd"),
          "Parameter - from ATR",
          a$paramcd_choices,
          a$paramcd,
          multiple = FALSE),
        optionalSelectInput(
          ns("x_var"),
          "X-axis Variable",
          a$x_var_choices,
          a$x_var,
          multiple = FALSE),
        optionalSelectInput(
          ns("y_var"),
          "Y-axis Variable",
          a$y_var_choices,
          a$y_var,
          multiple = FALSE),
        optionalSelectInput(
          ns("line_colorby_var"),
          "Color By Variable (Line)",
          a$line_colorby_var_choices,
          a$line_colorby_var,
          multiple = FALSE),
        optionalSelectInput(
          ns("marker_var"),
          "Marker Symbol By Variable",
          a$marker_var_choices,
          a$marker_var,
          multiple = FALSE),
        optionalSelectInput(
          ns("xfacet_var"),
          "X-facet By Variable",
          a$xfacet_var_choices,
          a$xfacet_var,
          multiple = TRUE),
        optionalSelectInput(
          ns("yfacet_var"),
          "Y-facet By Variable",
          a$yfacet_var_choices,
          a$yfacet_var,
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
      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
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

  vals <- reactiveValues(spiderplot=NULL)

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

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ATR_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    atr_name <- paste0(dataname, "_FILTERED")
    assign(atr_name, ATR_FILTERED) # so that we can refer to the 'correct' data name


    # restart chunks & include current environment ---

    chunks_reset(envir = environment())


    # get inputs ---

    paramcd <- input$paramcd
    x_var <- input$x_var
    y_var <- input$y_var
    marker_var <- input$marker_var
    line_colorby_var <- input$line_colorby_var
    anno_txt_var <- input$anno_txt_var
    legend_on <- input$legend_on
    xfacet_var <- input$xfacet_var
    yfacet_var <- input$yfacet_var

    vref_line <- input$vref_line
    href_line <- input$href_line


    # define variables ---

    # if variable is not in ASL, then take from domain VADs
    varlist <- c(xfacet_var, yfacet_var, marker_var, line_colorby_var)
    varlist_from_asl <- varlist[varlist %in% names(ASL_FILTERED)]
    varlist_from_anl <- varlist[!varlist %in% names(ASL_FILTERED)]

    asl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_asl))
    atr_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", x_var, y_var, varlist_from_anl))



    # variables and inputs to chunks ---

    chunks_push(bquote({
      asl_vars <- .(asl_vars)
      atr_vars <- .(atr_vars)

      paramcd <- .(paramcd)
      x_var <- .(x_var)
      y_var <- .(y_var)
      marker_var <- .(marker_var)
      line_colorby_var <- .(line_colorby_var)
      vref_line <- .(vref_line)
      href_line <- .(href_line)
      anno_txt_var <- .(anno_txt_var)
      legend_on <- .(legend_on)
      xfacet_var <- .(xfacet_var)
      yfacet_var <- .(yfacet_var)
    }))

    chunks_push_new_line()


    # preprocessing of datasets to chunks ---

    # vars definition
    chunks_push(bquote({
      atr_vars <- atr_vars[atr_vars != "None"]
      atr_vars <- atr_vars[!is.null(atr_vars)]
    }))

    chunks_push_new_line()

    # merge
    chunks_push(bquote({
      ASL <- ASL_FILTERED[, asl_vars] %>% as.data.frame()
      ATR <- .(as.name(atr_name))[, atr_vars] %>% as.data.frame()

      ANL <- merge(ASL, ATR, by = c("USUBJID", "STUDYID"))
      ANL <- ANL %>% group_by(USUBJID, PARAMCD) %>% arrange(ANL[,x_var]) %>%
        as.data.frame()
    }))

    chunks_push_new_line()

    # format and filter
    chunks_push(quote({
      ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, '-', fixed = TRUE), tail, 1))

      ANL_f <- ANL %>% filter(PARAMCD == paramcd) %>% as.data.frame()
    }))

    chunks_push_new_line()

    # check
    chunks_eval()
    validate(need(chunks_is_ok(), "Data could not be constructed."))

    # reference lines preprocessing - vertical
    chunks_push(quote({
      {
        # If reference lines are requested
        if (!is.null(vref_line) || vref_line != "") {
          vref_line <- as.numeric(unlist(strsplit(vref_line, ",")))
        } else {
          vref_line <- NULL
        }
      }
    }))

    chunks_push_new_line()

    # validate vref_line
    chunks_eval()
    vl <- chunks_get_var("vref_line")
    validate(need(all(!is.na(vl)),
                  "Not all values entered for reference line(s) were numeric"))
    ANL_f <- chunks_get_var("ANL_f")
    validate(need(chunks_is_ok(), "Data could not be constructed."))

    # reference lines preprocessing - horizontal
    chunks_push(quote({
      {
        if (!is.null(href_line) || href_line != "") {
          href_line <- as.numeric(unlist(strsplit(href_line, ",")))
        } else {
          href_line <- NULL
        }
      }
    }))

    chunks_push_new_line()

    # validate href_line
    chunks_eval()
    hl <- chunks_get_var("href_line")
    validate(need(all(!is.na(hl)), "Not all values entered for reference line(s) were numeric"))

    # check
    validate(need(chunks_is_ok(), "Data could not be constructed."))

    # label
    chunks_push(quote({
      lbl <- NULL
      if(anno_txt_var){
        lbl <- list(txt_ann = as.factor(ANL_f$USUBJID))
      }
    }))

    chunks_push_new_line()

    # check
    chunks_eval()
    validate(need(chunks_is_ok(), "Data could not be constructed."))



    # plot code to chunks ---

    chunks_push(call(
      "g_spiderplot",
      marker_x = quote(ANL_f[,x_var]),
      marker_id = quote(ANL_f$USUBJID),
      marker_y = quote(ANL_f[,y_var]),
      line_colby = if (line_colorby_var != "None") {
        quote(ANL_f[,line_colorby_var])
      } else {
        NULL
      },
      marker_shape = if (marker_var != "None") {
        quote(ANL_f[,marker_var])
      } else {
        NULL
      },
      marker_size = 4,
      datalabel_txt = quote(lbl),
      facet_rows = if (!is.null(yfacet_var)) {
        quote(data.frame(ANL_f[, yfacet_var]))
      } else {
        NULL
      },
      facet_columns = if (!is.null(xfacet_var)) {
        quote(data.frame(ANL_f[, xfacet_var]))
      } else {
        NULL
      },
      vref_line = quote(vref_line),
      href_line = quote(href_line),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = quote(legend_on)
    ))

    chunks_eval()

  })



  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Swimlane plot",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })


}

