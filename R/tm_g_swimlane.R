#' Teal Module for Swimlane Plot
#'
#' This is teal module that generates a swimlane plot (bar plot with markers) for ADaM data
#'
#' @param label item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in the list passed
#'     to the \code{data} argument of \code{\link[teal]{init}}.
#' @param bar_var subject-level numeric variable from dataset to plot as the bar length
#' @param bar_var_choices vector with variable names that can be used as \code{bar_var}
#' @param bar_color_var color by variable (subject-level)
#' @param bar_color_var_choices vector with variable names that can be used as \code{bar_color_var}
#' @param sort_var sort by variable (subject-level)
#' @param sort_var_choices vector with variable names that can be used as \code{sort_var}
#' @param marker_pos_var numeric variable for marker position (Note: make sure that marker position has the 
#'      same relative start day as bar length variable \code{bar_var})
#' @param marker_pos_var_choices vector with variable names that can be used as \code{marker_pos_var}
#' @param marker_shape_var marker shape variable
#' @param marker_shape_var_choices vector with variable names that can be used as \code{marker_shape_var}
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name)
#' @param marker_color_var marker color variable
#' @param marker_color_var_choices vector with variable names that can be used as \code{marker_color_var}
#' @param marker_color_opt aesthetic values to map color values (named vector to map color values to each name)
#' @param vref_line vertical reference lines
#' @param anno_txt_var character vector with variable names that are selected as annotation
#' @param anno_txt_var_choices vector with variable names that can be selected as \code{anno_txt_var}
#' @param plot_height plot height
#' @inheritParams teal::standard_layout
#' @param code_data_processing string with data preprocessing before the teal app is initialized
#'
#' @import grid
#'
#' @return a \code{\link[teal]{module}} object
#'
#' @export
#'
#' @template author_qit3
#'
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#' 
#' data("rADSL")
#' data("rADRS")
#' 
#' ASL <- rADSL
#' ARS <- rADRS
#'
#' ARS <- ARS %>% filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#'   mutate(AVALC = DCSREAS,
#'          ADY = EOSDY) %>%
#'   rbind (ARS %>% filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#'   arrange(USUBJID)
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_g_swimlane(
#'        label = "Swimlane Plot",
#'        dataname = 'ARS',
#'        bar_var = "TRTDURD",
#'        bar_var_choices = c("TRTDURD", "EOSDY"),
#'        bar_color_var = "EOSSTT",
#'        bar_color_var_choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1", "SEX"),
#'        sort_var = "ACTARMCD",
#'        sort_var_choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD"),
#'        marker_pos_var = "ADY",
#'        marker_pos_var_choices = c("None", "ADY"),
#'        marker_shape_var = "AVALC",
#'        marker_shape_var_choices = c("None", "AVALC", "AVISIT"),
#'        marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
#'        marker_color_var = "AVALC",
#'        marker_color_var_choices = c("None", "AVALC", "AVISIT"),
#'        marker_color_opt = c("CR" = "green", "PR" = "blue", "SD" = "goldenrod", "PD" = "red", "Death" = "black"),
#'        vref_line = "30, 60",
#'        anno_txt_var = c("ACTARM", "SEX"),
#'        anno_txt_var_choices = c("ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1", "SEX", "RACE","COUNTRY","DCSREAS", "DCSREASP")
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }

tm_g_swimlane <- function(label,
                          dataname,
                          bar_var,
                          bar_var_choices = bar_var,
                          bar_color_var = NULL,
                          bar_color_var_choices = bar_color_var,
                          sort_var = NULL,
                          sort_var_choices = sort_var,
                          marker_pos_var = NULL,
                          marker_pos_var_choices = marker_pos_var,
                          marker_shape_var = NULL,
                          marker_shape_var_choices = marker_shape_var,
                          marker_shape_opt = NULL,
                          marker_color_var = NULL,
                          marker_color_var_choices = marker_color_var,
                          marker_color_opt = NULL,
                          anno_txt_var = NULL,
                          anno_txt_var_choices = anno_txt_var,
                          vref_line = NULL,
                          plot_height = c(1200, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL,
                          code_data_processing = NULL
){

  args <- as.list(environment())

  module(
    label = label,
    ui = ui_g_swimlane,
    ui_args = args,
    server = srv_g_swimlane,
    server_args = list(dataname = dataname,
                       marker_shape_opt = marker_shape_opt,
                       marker_color_opt = marker_color_opt,
                       code_data_processing = code_data_processing),
    filters = dataname
  )
}


ui_g_swimlane <- function(id, ...){

  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code(a$dataname)),
      optionalSelectInput(ns("bar_var"), "Bar Length", choices = a$bar_var_choices,
                          selected = a$bar_var, multiple = FALSE),
      optionalSelectInput(ns("bar_color_var"), "Bar Color", choices = a$bar_color_var_choices,
                          selected = a$bar_color_var, multiple = FALSE),
      optionalSelectInput(ns("marker_pos_var"), "Marker Position", choices = a$marker_pos_var_choices,
                          selected = a$marker_pos_var, multiple = FALSE),
      optionalSelectInput(ns("marker_shape_var"), "Marker Shape", choices = a$marker_shape_var_choices,
                          selected = a$marker_shape_var, multiple = FALSE),
      optionalSelectInput(ns("marker_color_var"), "Marker Color", choices = a$marker_color_var_choices,
                          selected = a$marker_color_var, multiple = FALSE),
      optionalSelectInput(ns("sort_var"), "Sort by", choices = a$sort_var_choices,
                          selected = a$sort_var, multiple = FALSE,
                          label_help = helpText("from ", tags$code("ASL"))),
      optionalSelectInput(ns("anno_txt_var"), "Annotation Variables", choices = a$anno_txt_var_choices,
                          selected = a$anno_txt_var, multiple = TRUE,
                          label_help = helpText("from ", tags$code("ASL"))),
      textInput(ns("vref_line"), 
                label = div("Vertical Reference Line(s)", tags$br(), 
                            helpText("Enter numeric value(s) of reference lines, separated by comma (eg. 100, 200)")), 
                value = a$vref_line),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_swimlane <- function(input, output, session, datasets, dataname,
                           marker_shape_opt,
                           marker_color_opt,
                           code_data_processing) {

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("swimlaneplot"), height = plot_height)
  })

  chunks <- list(
    vars = "# No Calculated",
    data = "# No Calculated",
    g_swimlaneplot = "# No Calculated"
  )

  # create plot
  output$swimlaneplot <- renderPlot({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ANL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    bar_var <- input$bar_var
    bar_color_var <- if (input$bar_color_var == "None") NULL else input$bar_color_var
    sort_var <- if (input$sort_var == "None") NULL else input$sort_var
    marker_pos_var <- if (input$marker_pos_var == "None") NULL else input$marker_pos_var
    marker_shape_var <- if (input$marker_shape_var == "None") NULL else input$marker_shape_var
    # marker_shape_opt <- input$marker_shape_opt
    marker_color_var <- if (input$marker_color_var == "None") NULL else input$marker_color_var
    # marker_color_opt <- input$marker_color_opt
    anno_txt_var <- input$anno_txt_var
    vref_line <- input$vref_line

    if (length(anno_txt_var) == 0) anno_txt_var <- NULL

    for (i in seq_along(chunks)) chunks[[i]] <<- "# Not calculated"

    # validate input values
    validate_standard_inputs(
      ASL = ASL_FILTERED,
      aslvars = c("USUBJID", "STUDYID", bar_var, bar_color_var, sort_var, anno_txt_var),
      ANL = ANL_FILTERED,
      anlvars = unique(c("USUBJID", "STUDYID", "ADY", marker_pos_var, marker_shape_var, marker_color_var)),
      min_nrow = 3
    )

    # any other validations

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    asl_vars <- unique(c("USUBJID", "STUDYID", bar_var, bar_color_var, sort_var, anno_txt_var))
    anl_vars <- unique(c("USUBJID", "STUDYID", "ADY", marker_pos_var, marker_shape_var, marker_color_var))

    chunks$vars <<- bquote({
      bar_var <- .(bar_var)
      bar_color_var <- .(bar_color_var)
      sort_var <- .(sort_var)
      marker_pos_var <- .(marker_pos_var)
      marker_shape_var <- .(marker_shape_var)
      # marker_shape_opt <- .(marker_shape_opt)
      marker_color_var <- .(marker_color_var)
      # marker_color_opt <- .(marker_color_opt)
      anno_txt_var <- .(anno_txt_var)
      vref_line <- .(vref_line)
    })

    chunks$data <<- bquote({
      ASL_p <- ASL_FILTERED
      ANL_p <- .(as.name(anl_name))
      
      ASL <- ASL_p[, .(asl_vars)]
      ANL <- merge(
        x = ASL_p[, .(asl_vars)],
        y = ANL_p[, .(anl_vars)],
        all.x = FALSE, all.y = FALSE,
        by = c("USUBJID", "STUDYID")
      )
      
      #If reference lines are requested
      {
        if (vref_line != "" || is.null(vref_line)) {
          vref_line <- unlist(strsplit(.(vref_line), ","))
          vref_line <- as.numeric(vref_line)
          validate(need(all(!is.na(vref_line)), "Not all values entered for reference line(s) were numeric"))
        } else{
          vref_line <- NULL
        }
      }
      
    })
    
    eval(chunks$data)

    chunks$g_swimlaneplot <<- bquote({
      grid.newpage()

      p <- g_swimlane(bar_id = ASL[["USUBJID"]],
                      bar_length = ASL[[.(bar_var)]],
                      sort_by = if (length(sort_var) > 0) ASL[[.(sort_var)]] else NULL, 
                      col_by = if (length(bar_color_var) > 0) ASL[[.(bar_color_var)]] else NULL, 
                      marker_id = ANL[["USUBJID"]],
                      marker_pos = if (length(marker_pos_var) > 0) ANL[[.(marker_pos_var)]] else NULL,
                      marker_shape = if (length(marker_shape_var) > 0) ANL[[.(marker_shape_var)]] else NULL, 
                      marker_shape_opt <- .(marker_shape_opt),
                      marker_color = if (length(marker_color_var) > 0) ANL[[.(marker_color_var)]] else NULL, 
                      marker_color_opt <- .(marker_color_opt),
                      anno_txt = if (length(anno_txt_var) > 0) ASL[.(anno_txt_var)] else data.frame(),
                      yref_line = .(vref_line),
                      ytick_at = waiver(),
                      ylab = "Time from First Treatment (Day)",
                      title = "Swimlane Plot")
      p

    })

    eval(chunks$g_swimlaneplot)

  })


  observeEvent(input$show_rcode, {

    header <- get_rcode_header_osprey(
      title = "Swimlane Plot",
      datanames = if (is.null(code_data_processing)) dataname else datasets$datanames(),
      datasets = datasets,
      code_data_processing
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$vars)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$data)),
      "",
      deparse(chunks$g_swimlaneplot)
    ), collapse = "\n")

    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Swimlane Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })

}
