
#' Butterfly plot Teal Module
#' 
#' Display butterfly plot as a shiny module
#' 
#' @param label menu item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param filter_var variable name of data filter, default here is \code{NULL}
#' @param filter_var_choices vector with \code{filter_var} choices, default 
#' here is \code{NULL}
#' @param right_var dichotomization variable for right side
#' @param right_var_choices vector with dichotomization choices
#' @param left_var dichotomization variable for left side
#' @param left_var_choices vector with dichotomization choices
#' @param category_var category (y axis) variable
#' @param category_var_choices vector of category choices
#' @param color_by_var variable defines color blocks within each bar
#' @param color_by_var_choices vector of choices for color_block_by
#' @param count_by_var variable defines how x axis is calculated 
#' @param count_by_var_choices vector of choices for count_by_var
#' @param facet_var variable for row facets
#' @param facet_var_choices vector with \code{facet_var} choices
#' @param sort_by_var argument for order of class and term elements in table,
#'  default here is "count"
#' @param sort_by_var_choices vector with \code{sort_by_var} choices
#' @param legend_on boolean value for whether legend is displayed
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
#' #Example butterfly plot
#' library(dplyr)
#' 
#' data("rADSL")
#' data("rADAE")
#' ASL <- rADSL
#' AAE <- rADAE
#' AAE <- AAE %>% mutate(flag1 = ifelse(AETOXGR == 1, 1, 0)) %>% 
#'                mutate(flag2 = ifelse(AETOXGR == 2, 1, 0)) %>% 
#'                mutate(flag3 = ifelse(AETOXGR == 3, 1, 0)) 
#' AAE <- AAE %>% mutate(flag1_filt = rep("Y", nrow(AAE))) 
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, AAE = AAE),
#'   modules = root_modules(
#'     tm_g_butterfly(
#'        label = "Butterfly Plot",
#'        dataname = "AAE",
#'        filter_var = "NULL",
#'        filter_var_choices = c("NULL", "DTHFL", "flag1_filt"), 
#'        right_var = "SEX",
#'        right_var_choices = c("SEX", "ARM", "RACE", "flag1", "flag2", "flag3"),
#'        left_var = "SEX",
#'        left_var_choices = c("SEX", "ARM", "RACE", "flag1", "flag2", "flag3"),
#'        category_var = "AEBODSYS",
#'        category_var_choices = c("AEDECOD", "AEBODSYS"),
#'        color_by_var = "AETOXGR",
#'        color_by_var_choices = c("AETOXGR", "None"),
#'        count_by_var = "# of patients",
#'        count_by_var_choices = c("# of patients", "# of AEs"),
#'        facet_var = "None",
#'        facet_var_choices = c("RACE", "SEX", "ARM", "None"),
#'        sort_by_var = "count",
#'        sort_by_var_choices = c("count", "alphabetical"),
#'        legend_on = TRUE,
#'        plot_height = c(600, 200, 2000)
#'    )
#'   )
#' )
#'    
#' shinyApp(x$ui, x$server) 
#'   
#' 
tm_g_butterfly <- function(label, 
                           dataname, 
                           filter_var = NULL,
                           filter_var_choices = NULL,
                           right_var,
                           right_var_choices = dich_var,
                           left_var,
                           left_var_choices = dich_var,
                           category_var,
                           category_var_choices = category_var,
                           color_by_var,
                           color_by_var_choices = c(color_by_var, "None"),
                           count_by_var,
                           count_by_var_choices = c(count_by_var, "None"),
                           facet_var,
                           facet_var_choices = c(facet_var, "None"),  
                           sort_by_var,
                           sort_by_var_choices,
                           legend_on = TRUE,
                           plot_height,
                           pre_output = NULL, 
                           post_output = NULL, 
                           code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_butterfly,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    ui = ui_g_butterfly,
    ui_args = args
  )
  
}

ui_g_butterfly <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("plot_ui"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Dataset is:", tags$code(a$dataname)),
      optionalSelectInput(ns("filter_var"), "Preset Data Filters", a$filter_var_choices, a$filter_var, multiple = TRUE),
      optionalSelectInput(ns("right_ch"), "Right Dichotomization Variable", a$right_var_choices, a$right_var, multiple = FALSE),
      radioButtons(ns("right_v"), "Choose one", a$right_var),
      optionalSelectInput(ns("left_ch"), "Left Dichotomization Variable", a$left_var_choices, a$left_var, multiple = FALSE),
      radioButtons(ns("left_v"), "Choose one", a$left_var),
      optionalSelectInput(ns("category_var"), "Category Variable", a$category_var_choices, a$category_var, multiple = FALSE),
      radioButtons(ns("color_by_var"), "Color Block By Variable", a$color_by_var_choices, a$color_by_var),
      radioButtons(ns("count_by_var"), "Count By Variable", a$count_by_var_choices, a$count_by_var),
      optionalSelectInput(ns("facet_var"), "Facet By Variable", a$facet_var_choices, a$facet_var, multiple = FALSE),
      radioButtons(ns("sort_by_var"), "Sort By Variable", a$sort_by_var_choices, a$sort_by_var),
      checkboxInput(ns("legend_on"), "Add legend", value = a$legend_on),
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

srv_g_butterfly <- function(input, output, session, datasets, dataname, code_data_processing) {
  
  vals <- reactiveValues(butterfly=NULL)
  
  #dynamic options for dichotomization variable
  observe({
    right_ch <- input$right_ch
    left_ch <- input$left_ch
    
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    ANL_f  <- merge(ASL_FILTERED, AAE) %>% as.data.frame()
    
    options_r <- unique(ANL_f[, right_ch])
    options_l <- unique(ANL_f[, left_ch])
    
    updateRadioButtons(session, "right_v", choices = options_r)
    updateRadioButtons(session, "left_v", choices = options_l)
  })
  
  # dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("butterfly"), height=plot_height)
  })
  
  chunks <- list(
    vars = "# Not Calculated",
    data = "#Not Calculated",
    p_butterfly = "# Not Calculated"
  )
  
  output$butterfly <- renderPlot({
    
    right_v <- input$right_v
    left_v <- input$left_v
    right_ch <- input$right_ch
    left_ch <- input$left_ch
    category_var <- input$category_var
    color_by_var <- input$color_by_var
    count_by_var <- input$count_by_var
    legend_on <- input$legend_on
    facet_var <- input$facet_var
    sort_by_var <- input$sort_by_var
    filter_var <- input$filter_var

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    
    asl_vars <- unique(c("USUBJID", "STUDYID"))
    aae_vars <- unique(c("USUBJID", "STUDYID", "PARAMCD", category_var, color_by_var, 
                         count_by_var, facet_var, right_ch, left_ch)) 
    
    chunks$vars <<- bquote({
      right_v <- .(right_v)
      left_v <- .(left_v)
      right_ch <- .(right_ch)
      left_ch <- .(left_ch)
      category_var <- .(category_var)
      color_by_var <- .(color_by_var)
      count_by_var <- .(count_by_var)
      legend_on <- .(legend_on)
      facet_var <- .(facet_var)
      sort_by_var <- .(sort_by_var)
      asl_vars <- .(asl_vars)
      aae_vars <- .(aae_vars)
      filter_var <- .(filter_var)
    })
    
    chunks$data <<- bquote({
      aae_vars <- aae_vars[aae_vars %in% names(AAE_FILTERED)]
      ASL <- ASL_FILTERED[, asl_vars] %>% as.data.frame()
      
      if(!("NULL" %in% .(filter_var)) && !is.null(.(filter_var))){
        AAE <- quick_filter(.(filter_var), AAE_FILTERED) %>% droplevels()
      } 
      
      AAE <- AAE[, aae_vars] %>% as.data.frame() 

      ANL_f  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID")) %>%
        as.data.frame() 
      
      ANL_f <- na.omit(ANL_f)

      if(!is.null(right_v) && !is.null(left_v)){
        if(!(right_v %in% c(0, 1))){
          temp <- replace(as.character(ANL_f[, .(right_ch)]), as.character(ANL_f[, .(right_ch)]) != .(right_v), 0)
          temp <- replace(temp, temp == .(right_v), 1)
          right <- as.numeric(temp)
          right_name <- right_v
        } else{
          right <- ANL_f[, .(right_ch)]
          right_name <- right_ch
        }
        
        if(!(left_v %in% c(0, 1))){
          temp <- replace(as.character(ANL_f[, .(left_ch)]), as.character(ANL_f[, .(left_ch)]) != .(left_v), 0)
          temp <- replace(temp, temp == .(left_v), 1)
          left <- as.numeric(temp)
          left_name <- left_v
        } else{
          left <- ANL_f[, .(left_ch)]
          left_name <- left_ch
        }
        
      }

    })
    
    eval(chunks$data)
    
    if(!is.null(right_v) && !is.null(left_v)){
      chunks$p_butterfly <<- call(
        "g_butterfly",
        category = bquote(ANL_f[,category_var]),
        rightFlag = bquote(right),
        leftFlag = bquote(left),
        group_names = bquote(c(right_name, left_name)),
        block_count = bquote(count_by_var),
        block_color = bquote(if(color_by_var != "None"){ANL_f[,color_by_var]}else{NULL}),
        id = bquote(ANL_f$USUBJID),
        facet_rows = bquote(if(facet_var != "None"){ANL_f[,facet_var]}else{NULL}),
        x_label = bquote(count_by_var),
        y_label = "AE Derived Terms",
        legend_label = bquote(color_by_var),
        sort_by = bquote(sort_by_var),
        show_legend = bquote(legend_on) 
        )
    }
    
    vals$butterfly <- eval(chunks$p_butterfly)
    vals$butterfly
    
  })
  
  observeEvent(input$show_rcode, {
    
    header <- get_rcode_header(
      title = "butterfly",
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
      remove_enclosing_curly_braces(deparse(chunks$p_butterfly, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Butterfly Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # #export plot as a pdf
  # output$export_plot = downloadHandler(
  # filename = "butterfly.pdf",
  # content = function(file) {
  #    pdf(file)
  #    print(vals$butterfly) 
  #    dev.off()
  # })
  
}

