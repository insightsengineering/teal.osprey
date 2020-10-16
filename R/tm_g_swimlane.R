#' Teal Module for Swimlane Plot
#'
#' This is teal module that generates a swimlane plot (bar plot with markers) for ADaM data
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param dataname analysis data used for plotting, needs to be available in the list passed to the \code{data}
#' argument of \code{\link[teal]{init}}. If no markers are to be plotted in the module, "ADSL" should be
#' the input. If markers are to be plotted, data name for the marker data should be the input
#' @param bar_var (\code{\link[teal]{choices_selected}}) subject-level numeric variable from dataset
#' to plot as the bar length
#' @param bar_color_var (\code{\link[teal]{choices_selected}}) color by variable (subject-level)
#' @param sort_var (\code{choices_selected}) sort by variable (subject-level)
#' @param marker_pos_var (\code{\link[teal]{choices_selected}}) variable for marker position from marker data
#' (Note: make sure that marker position has the same relative start day as bar length variable \code{bar_var})
#' @param marker_shape_var (\code{\link[teal]{choices_selected}}) marker shape variable from marker data
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name).
#' If not \code{NULL}, please make sure this contains all possible values for \code{marker_shape_var} values,
#' otherwise shape will be assigned by \code{ggplot} default
#' @param marker_color_var marker color variable from marker data
#' @param marker_color_opt aesthetic values to map color values (named vector to map color values to each name).
#' If not \code{NULL}, please make sure this contains all possible values for \code{marker_color_var} values,
#' otherwise color will be assigned by \code{ggplot} default
#' @param vref_line vertical reference lines
#' @param anno_txt_var character vector with subject-level variable names that are selected as annotation
#'
#' @return a \code{\link[teal]{module}} object
#'
#' @export
#'
#' @template author_qit3
#'
#' @examples
#'
#' #Example using stream (ADaM) dataset
#' library(dplyr)
#'
#' ADSL <- rADSL
#' ADRS <- rADRS
#'
#' ADRS <- ADRS %>%
#'   dplyr::filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#'   mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
#'   base::rbind(ADRS %>% dplyr::filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#'   arrange(USUBJID)
#'
#' x <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = 'ADSL <- rADSL
#'     ADRS <- rADRS
#'     ADRS <- ADRS %>% dplyr::filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#'     mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
#'     rbind (ADRS %>% dplyr::filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#'     arrange(USUBJID)', check = FALSE),
#'   modules = root_modules(
#'     tm_g_swimlane(
#'       label = "Swimlane Plot",
#'       dataname = "ADRS",
#'       bar_var = choices_selected(selected = "TRTDURD", choices = c("TRTDURD", "EOSDY")),
#'       bar_color_var = choices_selected(
#'         selected = "EOSSTT",
#'         choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1", "SEX")
#'       ),
#'       sort_var = choices_selected(
#'         selected = "ACTARMCD",
#'         choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD")
#'       ),
#'       marker_pos_var = choices_selected(selected = "ADY", choices = c("None", "ADY")),
#'       marker_shape_var = choices_selected(selected = "AVALC", c("None", "AVALC", "AVISIT")),
#'       marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
#'       marker_color_var = choices_selected(selected = "AVALC",
#'                                           choices = c("None", "AVALC", "AVISIT")),
#'       marker_color_opt = c("CR" = "green", "PR" = "blue", "SD" = "goldenrod",
#'                            "PD" = "red", "Death" = "black"),
#'       vref_line = c(30, 60),
#'       anno_txt_var = choices_selected(
#'         selected = c("ACTARM", "SEX"),
#'         choices = c("ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1",
#'                     "SEX", "RACE", "COUNTRY", "DCSREAS", "DCSREASP")
#'       )
#'     )
#'   )
#'   )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
#'
tm_g_swimlane <- function(label,
                          dataname,
                          bar_var,
                          bar_color_var = NULL,
                          sort_var = NULL,
                          marker_pos_var = NULL,
                          marker_shape_var = NULL,
                          marker_shape_opt = NULL,
                          marker_color_var = NULL,
                          marker_color_opt = NULL,
                          anno_txt_var = NULL,
                          vref_line = NULL,
                          plot_height = c(1200L, 400L, 5000L),
                          plot_width = NULL,
                          pre_output = NULL,
                          post_output = NULL) {
  args <- as.list(environment())

  stopifnot(is_character_single(label))
  stopifnot(is_character_single(dataname))
  stopifnot(is.choices_selected(bar_var))
  stopifnot(is.choices_selected(bar_color_var))
  stopifnot(is.choices_selected(marker_pos_var))
  stopifnot(is.choices_selected(marker_shape_var))
  stopifnot(is_numeric_vector(marker_shape_opt))
  stopifnot(is.choices_selected(marker_color_var))
  stopifnot(is_character_vector(marker_color_opt))
  stopifnot(is.choices_selected(anno_txt_var))
  stopifnot(is_numeric_vector(vref_line))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  module(
    label = label,
    ui = ui_g_swimlane,
    ui_args = args,
    server = srv_g_swimlane,
    server_args = list(
      dataname = dataname,
      marker_pos_var,
      marker_shape_var,
      marker_shape_opt = marker_shape_opt,
      marker_color_var,
      marker_color_opt = marker_color_opt,
      label = label,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    filters = dataname
  )
}


ui_g_swimlane <- function(id, ...) {
  a <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("swimlaneplot"), height = a$plot_height, width = a$plot_width)
      ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      div(
        style = "border-left: 3px solid #e3e3e3; padding-left: 0.6em; border-radius: 5px; margin-left: -0.6em;",
        optionalSelectInput(ns("bar_var"), "Bar Length",
                            choices = a$bar_var$choices,
                            selected = a$bar_var$selected, multiple = FALSE,
                            label_help = helpText("from ", tags$code("ADSL"))
        ),
        optionalSelectInput(ns("bar_color_var"), "Bar Color",
                            choices = a$bar_color_var$choices,
                            selected = a$bar_color_var$selected, multiple = FALSE,
                            label_help = helpText("from ", tags$code("ADSL"))
        )
      ),
      optionalSelectInput(ns("sort_var"), "Sort by",
                          choices = a$sort_var$choices,
                          selected = a$sort_var$selected, multiple = FALSE,
                          label_help = helpText("from ", tags$code("ADSL"))
      ),
      div(
        style = "border-left: 3px solid #e3e3e3; padding-left: 0.6em; border-radius: 5px; margin-left: -0.6em;",
        if (a$dataname == "ADSL") {
          NULL
        } else if (is.null(a$marker_pos_var)) {
          NULL
        } else {
          optionalSelectInput(ns("marker_pos_var"), "Marker Position",
                              choices = a$marker_pos_var$choices,
                              selected = a$marker_pos_var$selected, multiple = FALSE,
                              label_help = helpText("from ", tags$code(a$dataname))
          )
        },
        uiOutput(ns("marker_shape_sel")),
        uiOutput(ns("marker_color_sel"))
      ),
      optionalSelectInput(ns("anno_txt_var"), "Annotation Variables",
                          choices = a$anno_txt_var$choices,
                          selected = a$anno_txt_var$selected, multiple = TRUE,
                          label_help = helpText("from ", tags$code("ADSL"))
      ),
      textInput(ns("vref_line"),
                label = div(
                  "Vertical Reference Line(s)", tags$br(),
                  helpText("Enter numeric value(s) of reference lines, separated by comma (eg. 100, 200)")
                ),
                value = paste(a$vref_line, collapse = ", ")
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_swimlane <- function(input, output, session, datasets, dataname,
                           marker_pos_var,
                           marker_shape_var,
                           marker_shape_opt,
                           marker_color_var,
                           marker_color_opt,
                           label,
                           plot_height,
                           plot_width) {

  # use teal.devel code chunks
  init_chunks()

  observe({
    # if marker position is "None", then hide options for marker shape and color
    output$marker_shape_sel <- renderUI({
      if (dataname == "ADSL" | is.null(marker_shape_var)) {
        NULL
      } else if (is.null(input$marker_pos_var)) {
        NULL
      } else {
        ns <- session$ns
        if (input$marker_pos_var == "None") {
          NULL
        } else {
          optionalSelectInput(ns("marker_shape_var"), "Marker Shape",
                              choices = marker_shape_var$choices,
                              selected = marker_shape_var$selected, multiple = FALSE,
                              label_help = helpText("from ", tags$code(dataname))
          )
        }
      }
    })
    output$marker_color_sel <- renderUI({
      if (dataname == "ADSL" | is.null(marker_color_var)) {
        NULL
      } else if (is.null(input$marker_pos_var)) {
        NULL
      } else {
        ns <- session$ns
        if (input$marker_pos_var == "None") {
          NULL
        } else {
          optionalSelectInput(ns("marker_color_var"), "Marker Color",
                              choices = marker_color_var$choices,
                              selected = marker_color_var$selected, multiple = FALSE,
                              label_help = helpText("from ", tags$code(dataname))
          )
        }
      }
    })
  })

  # create plot
  plot_r <- reactive({

    # DATA GETTERS
    validate(need("ADSL" %in% datasets$datanames(), "ADSL needs to be defined in datasets"))
    validate(need(
      (length(datasets$datanames()) == 1 && dataname == "ADSL") ||
        (length(datasets$datanames()) >= 2 && dataname != "ADSL"),
      "Please either add just 'ADSL' as dataname when just ADSL is available
      In case 2 datasets are available ADSL is not supposed to be the dataname."
    ))

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    if (dataname != "ADSL") {
      ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
      anl_name <- paste0(dataname, "_FILTERED")
      assign(anl_name, ANL_FILTERED)
    }

    # Restart the chunks for showing code
    chunks_reset(envir = environment())

    # VARIABLE GETTERS
    # lookup bar variables
    bar_var <- input$bar_var
    bar_color_var <- if (input$bar_color_var == "None" | input$bar_color_var == "") NULL else input$bar_color_var
    sort_var <- if (input$sort_var == "None" | input$sort_var == "") NULL else input$sort_var

    # Check if marker inputs can be used
    if (dataname == "ADSL" || is.null(input$marker_pos_var)) {
      marker_pos_var <- NULL
      marker_shape_var <- NULL
      marker_color_var <- NULL
    } else {
      marker_pos_var <- if (input$marker_pos_var == "None") {
        NULL
      } else {
        input$marker_pos_var
      }
      marker_shape_var <- if (!is.null(marker_shape_var) && is.null(input$marker_shape_var)) {
        return(NULL)
      } else if (input$marker_shape_var == "None") {
        NULL
      } else {
        input$marker_shape_var
      }
      marker_color_var <- if (!is.null(marker_color_var) && is.null(input$marker_color_var)) {
        return(NULL)
      } else if (input$marker_color_var == "None") {
        NULL
      } else {
        input$marker_color_var
      }
    }

    anno_txt_var <- input$anno_txt_var
    vref_line <- input$vref_line

    if (length(anno_txt_var) == 0) anno_txt_var <- NULL

    # If reference lines are requested
    if (vref_line != "" || is.null(vref_line)) {
      vref_line <- unlist(strsplit(vref_line, ","))
      vref_line <- as.numeric(vref_line)
      validate(need(all(!is.na(vref_line)), "Not all values entered for reference line(s) were numeric"))
    } else {
      vref_line <- NULL
    }

    # validate input values
    if (dataname == "ADSL") {
      validate_has_data(ADSL_FILTERED, min_nrow = 3)
      validate_has_variable(ADSL_FILTERED, c("USUBJID", "STUDYID", bar_var, bar_color_var, sort_var, anno_txt_var))
    } else {
      validate_has_data(ADSL_FILTERED, min_nrow = 3)
      validate_has_variable(ADSL_FILTERED, c("USUBJID", "STUDYID", bar_var, bar_color_var, sort_var, anno_txt_var))

      validate_has_data(ANL_FILTERED, min_nrow = 3)
      validate_has_variable(ANL_FILTERED,
                            unique(c("USUBJID", "STUDYID", marker_pos_var, marker_shape_var, marker_color_var)))
    }

    # DATA / VARIABLE VALIDATIONS

    adsl_vars <- unique(c("USUBJID", "STUDYID", bar_var, bar_color_var, sort_var, anno_txt_var))

    if (dataname != "ADSL") {
      anl_vars <- unique(c("USUBJID", "STUDYID", marker_pos_var, marker_shape_var, marker_color_var)) # nolint
      validate(need(
        !any(c(marker_pos_var, marker_shape_var, marker_color_var) %in% adsl_vars),
        "marker-related variables need to come from marker data"
      ))
    }

    # WRITE VARIABLES TO CHUNKS

    chunks_push(bquote({
      bar_var <- .(bar_var)
      bar_color_var <- .(bar_color_var)
      sort_var <- .(sort_var)
      marker_pos_var <- .(marker_pos_var)
      marker_shape_var <- .(marker_shape_var)
      marker_color_var <- .(marker_color_var)
      anno_txt_var <- .(anno_txt_var)
    }))
    chunks_push_new_line()

    # WRITE DATA SELECTION TO CHUNKS

    if (dataname == "ADSL") {
      chunks_push(bquote({
        ADSL_p <- ADSL_FILTERED # nolint
        ADSL <- ADSL_p[, .(adsl_vars)] # nolint
        # only take last part of USUBJID
        ADSL$USUBJID <- unlist(lapply(strsplit(ADSL$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
      }))
    } else {
      anl_name <- paste0(dataname, "_FILTERED")
      chunks_push(bquote({
        ADSL_p <- ADSL_FILTERED # nolint
        ANL_p <- .(as.name(anl_name)) # nolint

        ADSL <- ADSL_p[, .(adsl_vars)] # nolint
        ANL <- merge( # nolint
          x = ADSL,
          y = ANL_p[, .(anl_vars)],
          all.x = FALSE, all.y = FALSE,
          by = c("USUBJID", "STUDYID")
        )
        # only take last part of USUBJID
        ADSL$USUBJID <- unlist(lapply(strsplit(ADSL$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
        ANL$USUBJID <- unlist(lapply(strsplit(ANL$USUBJID, "-", fixed = TRUE), tail, 1)) # nolint
      }))
    }
    chunks_push_new_line() # empty line for pretty code
    chunks_safe_eval()

    # WRITE PLOTTING CODE TO CHUNKS

    ADSL <- chunks_get_var("ADSL") # nolint
    ANL <- chunks_get_var("ANL") # nolint
    if (dataname == "ADSL") {
      chunks_push(call(
        "g_swimlane",
        bar_id = bquote(ADSL[["USUBJID"]]),
        bar_length = bquote(ADSL[[bar_var]]),
        sort_by = if (length(sort_var) > 0) bquote(ADSL[[sort_var]]) else NULL,
        col_by = if (length(bar_color_var) > 0) bquote(ADSL[[bar_color_var]]) else NULL,
        marker_id = NULL,
        marker_pos = NULL,
        marker_shape = NULL,
        marker_shape_opt = NULL,
        marker_color = NULL,
        marker_color_opt = NULL,
        anno_txt = if (length(anno_txt_var) > 0) bquote(ADSL[, anno_txt_var]) else data.frame(),
        yref_line = bquote(.(vref_line)),
        ytick_at = bquote(waiver()),
        ylab = "Time from First Treatment (Day)",
        title = "Swimlane Plot"
      ))
    } else {
      chunks_push(call(
        "g_swimlane",
        bar_id = bquote(ADSL[["USUBJID"]]),
        bar_length = bquote(ADSL[[bar_var]]),
        sort_by = if (length(sort_var) > 0) {
          bquote(ADSL[[sort_var]])
        } else {
          NULL
        },
        col_by = if (length(bar_color_var) > 0) {
          bquote(ADSL[[bar_color_var]])
        } else {
          NULL
        },
        marker_id = bquote(ANL[["USUBJID"]]),
        marker_pos = if (length(marker_pos_var) > 0) {
          bquote(ANL[[marker_pos_var]])
        } else {
          NULL
        },
        marker_shape = if (length(marker_shape_var) > 0) {
          bquote(ANL[[marker_shape_var]])
        } else {
          NULL
        },
        marker_shape_opt = if (length(marker_shape_var) == 0) {
          NULL
        } else if (length(marker_shape_var) > 0 &
                   all(unique(ANL[[marker_shape_var]]) %in% names(marker_shape_opt)) == T) {
          bquote(.(marker_shape_opt))
        } else {
          NULL
        },
        marker_color = if (length(marker_color_var) > 0) {
          bquote(ANL[[marker_color_var]])
        } else {
          NULL
        },
        marker_color_opt = if (length(marker_color_var) == 0) {
          NULL
        } else if (length(marker_color_var) > 0 &
                   all(unique(ANL[[marker_color_var]]) %in% names(marker_color_opt)) == T) {
          bquote(.(marker_color_opt))
        } else {
          NULL
        },
        anno_txt = if (length(anno_txt_var) > 0) {
          bquote(ADSL[, anno_txt_var])
        } else {
          data.frame()
        },
        yref_line = bquote(.(vref_line)),
        ytick_at = bquote(waiver()),
        ylab = "Time from First Treatment (Day)",
        title = "Swimlane Plot"
      ))
    }

    chunks_safe_eval()
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(plot_with_settings_srv,
             id = "swimlaneplot",
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
