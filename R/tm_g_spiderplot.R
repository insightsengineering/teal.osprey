#' Spiderplot (Early Development) Teal Module
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
#' @param marker_colorby_var variable dictates marker color
#' @param marker_colorby_var_choices vector with \code{marker_colorby_var} choices
#' @param line_colorby_var variable dictates line color
#' @param line_colorby_var_choices vector with \code{line_colorby_var} choices
#' @param vref_line vertical reference lines
#' @param href_line horizontal reference lines
#' @param anno_txt_var annotation text
#' @param anno_disc_study marker annotation
#' @param legend_on boolean value for whether legend is displayed
#' @param xfacet_var variable for x facets
#' @param xfacet_var_choices vector with \code{xfacet_var} choices
#' @param yfacet_var variable for y facets
#' @param yfacet_var_choices vector with \code{yfacet_var} choices
#' @param plot_height range of plot height
#' @param code_data_processing string with data preprocessing before the teal
#'   app is initialized, default is NULL
#' @inheritParams teal::standard_layout
#' 
#' @return an \code{\link[teal]{module}} object
#' @export
#' 
#' @author Carolyn Zhang
#' 
#' @examples 
#' #Example spiderplot
#' library(random.cdisc.data)
#' library(plyr)
#' library(dplyr)
#' library(gridExtra)
#' library(ggplot2)
#' require(lemon)
#' 
#' #asl <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adsl.sas7bdat")
#' #aae <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adae.sas7bdat")
#'
#' ASL <- radam("ADSL", N=30)
#' ATR <- radam("ATR", N=30)
#' #dat <- left_join(ATR, ASL) %>% filter(PARAMCD == "SUMTGLES") %>% as.data.frame()
#' 
#' # test character vector as x axis labels
#' #ATR$TUDY <- as.character(ATR$TUDY)
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ATR = ATR),
#'   modules = root_modules(
#'     tm_g_spiderplot(
#'        label = "Spiderplot",
#'        dataname = "ATR",
#'        paramcd = "SUMTGLES",
#'        paramcd_choices = c("SUMTGLES", "LDIAM"),
#'        x_var = "TUDY",
#'        x_var_choices = c("None", "TUDY"),
#'        y_var = "PCHG",
#'        y_var_choices = c("None", "PCHG"),
#'        marker_var = "RACE",
#'        marker_var_choices = c("None", "RACE"),
#'        marker_colorby_var = "RACE",
#'        marker_colorby_var_choices = c("None", "RACE"),
#'        line_colorby_var = "USUBJID",
#'        line_colorby_var_choices = c("USUBJID", "RACE"),
#'        vref_line = c("10", "37"),
#'        href_line = c(-0.3, 1),
#'        anno_txt_var = TRUE,
#'        anno_disc_study = TRUE,
#'        legend_on = FALSE,
#'        xfacet_var = "SEX",
#'        xfacet_var_choices = c("None", "SEX"),
#'        yfacet_var = "ARM",
#'        yfacet_var_choices = c("None", "ARM"),
#'        plot_height = c(600, 200, 2000)
#'    )
#'   )
#' )
#'    
#' shinyApp(x$ui, x$server) 
#'   
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
                            marker_colorby_var,
                            marker_colorby_var_choices = marker_colorby_var,
                            line_colorby_var,
                            line_colorby_var_choices = line_colorby_var_choices,
                            vref_line,
                            href_line,
                            anno_txt_var,
                            anno_disc_study,
                            legend_on = FALSE,
                            xfacet_var,
                            xfacet_var_choices = xfacet_var,
                            yfacet_var,
                            yfacet_var_choices = y_facet_var,
                            plot_height,
                            pre_output = NULL, 
                            post_output = NULL, 
                            code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_spider,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    ui = ui_g_spider,
    ui_args = args
  )
  
}

ui_g_spider <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("plot_ui"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Dataset is:", tags$code(a$dataname)),
      optionalSelectInput(ns("paramcd"), "Parameter - from ATR", a$paramcd_choices, a$paramcd, multiple = FALSE),
      optionalSelectInput(ns("x_var"), "X-axis Variable", a$x_var_choices, a$x_var, multiple = FALSE),
      optionalSelectInput(ns("y_var"), "Y-axis Variable", a$y_var_choices, a$y_var, multiple = FALSE),
      optionalSelectInput(ns("line_colorby_var"), "Color By Variable (Line)", a$line_colorby_var_choices, a$line_colorby_var, multiple = FALSE),
      optionalSelectInput(ns("marker_colorby_var"), "Color By Variable (Marker)", a$marker_colorby_var_choices, a$marker_colorby_var, multiple = FALSE),
      optionalSelectInput(ns("marker_var"), "Marker Symbol By Variable", a$marker_var_choices, a$marker_var, multiple = FALSE),
      optionalSelectInput(ns("xfacet_var"), "X-facet By Variable", a$xfacet_var_choices, a$xfacet_var, multiple = FALSE),
      optionalSelectInput(ns("yfacet_var"), "Y-facet By Variable", a$yfacet_var_choices, a$yfacet_var, multiple = FALSE),
      checkboxInput(ns("anno_txt_var"), "Add subject ID label", value = a$anno_txt_var),
      checkboxInput(ns("anno_disc_study"), "Add annotation marker", value = a$anno_disc_study),
      checkboxInput(ns("legend_on"), "Add legend", value = a$legend_on),
      optionalSelectInput(ns("vref_line"), "X-reference line", a$vref_line, a$vref_line, multiple = TRUE),
      optionalSelectInput(ns("href_line"), "Y-reference line", a$href_line, a$href_line, multiple = TRUE),
      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_g_spider <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  vals <- reactiveValues(spiderplot=NULL)
  
  # dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("spiderplot"), height=plot_height)

  })
  
  chunks <- list(
    vars = "# Not Calculated",
    data = "#Not Calculated",
    p_spiderplot = "# Not Calculated"
  )
  
  output$spiderplot <- renderPlot({
    
    paramcd <- input$paramcd
    x_var <- input$x_var
    y_var <- input$y_var
    marker_var <- input$marker_var
    marker_colorby_var <- input$marker_colorby_var
    line_colorby_var <- input$line_colorby_var
    anno_txt_var <- input$anno_txt_var
    anno_disc_study <- input$anno_disc_study
    legend_on <- input$legend_on
    xfacet_var <- input$xfacet_var
    yfacet_var <- input$yfacet_var
    vref_line <- input$vref_line
    href_line <- input$href_line
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ATR_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    chunks$vars <<- bquote({
      paramcd <- .(paramcd)
      x_var <- .(x_var)
      y_var <- .(y_var)
      marker_var <- .(marker_var)
      marker_colorby_var <- .(marker_colorby_var)
      line_colorby_var <- .(line_colorby_var)
      vref_line <- .(vref_line)
      href_line <- .(href_line)
      anno_txt_var <- .(anno_txt_var)
      anno_disc_study <- .(anno_disc_study)
      legend_on <- .(legend_on)
      xfacet_var <- .(xfacet_var)
      yfacet_var <- .(yfacet_var)
    })
    
    chunks$data <<- bquote({
      ADAE <- merge(ASL_FILTERED, ATR_FILTERED, by = c("USUBJID", "STUDYID")) 
      ADAE <- ADAE %>% group_by(USUBJID, PARAM) %>% arrange(ADAE[,.(x_var)]) %>%
        as.data.frame()
      
      ADAE_f <- ADAE %>% filter(PARAMCD == .(paramcd)) %>% as.data.frame()
      
      if(is.numeric(ADAE_f[,.(x_var)])){
        vref_line <- as.numeric(.(vref_line))
      } 
      href_line <- as.numeric(.(href_line))
      
      lbl <- NULL
      if(!.(anno_txt_var) && .(anno_disc_study)){
        lbl <- list(two = as.factor(ADAE_f[,.(line_colorby_var)]), three = c('id-1', 'id-2', 'id-3'))
      }
      else if(.(anno_txt_var) && !.(anno_disc_study)){
        lbl <- list(one = as.factor(ADAE_f[,.(line_colorby_var)]))
      }
      else if(.(anno_txt_var) && .(anno_disc_study)){
        lbl <- list(one = as.factor(ADAE_f[,.(line_colorby_var)]), two = as.factor(ADAE_f[,.(line_colorby_var)]), three = c('id-1', 'id-2'))
      }
    }) 
    
    eval(chunks$data)
    
    chunks$p_spiderplot <<- call(
      "g_spiderplot",
      marker_x = bquote(data.frame(day = ADAE_f[,x_var], groupby = ADAE_f$USUBJID)),
      marker_y = bquote(ADAE_f[,y_var]),
      line_colby = bquote(if(line_colorby_var != "None"){ADAE_f[,line_colorby_var]}else{NULL}),
      marker_color = bquote(if(marker_colorby_var != "None"){ADAE_f[,marker_colorby_var]}else{NULL}),
      marker_shape = bquote(if(marker_var != "None"){ADAE_f[,marker_var]}else{NULL}),
      marker_size = 5,
      datalabel_txt = bquote(lbl),
      facet_rows = bquote(if(yfacet_var != "None"){ADAE_f[,yfacet_var]}else{NULL}),
      facet_columns = bquote(if(xfacet_var != "None")ADAE_f[,xfacet_var]else{NULL}),
      vref_line = bquote(vref_line),
      href_line = bquote(href_line),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = bquote(legend_on) 
    )
     vals$spiderplot <- eval(chunks$p_spiderplot)
     vals$spiderplot

  })
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "Spiderplot",
      datanames = dataname,
      datasets = datasets,
      code_data_processing
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$vars, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$data, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$p_spiderplot, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Spiderplot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # #export plot as a pdf
  # output$export_plot = downloadHandler(
  # filename = "spiderplot.pdf",
  # content = function(file) {
  #    pdf(file)
  #    print(vals$spiderplot) 
  #    dev.off()
  # })
  
}

