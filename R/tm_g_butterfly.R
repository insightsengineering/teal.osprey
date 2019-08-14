
#' Butterfly plot Teal Module
#'
#' Display butterfly plot as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_t_ae
#' @param filter_var (\code{choices_selected}) variable name of data filter, please see details regarding
#'   expected values, default is \code{NULL}. \code{choices}
#'   vector with \code{filter_var} choices, default is
#'   \code{NULL}
#' @param right_var (\code{choices_selected}) dichotomization variable for right side
#' @param left_var (\code{choices_selected}) dichotomization variable for left side
#' @param category_var (\code{choices_selected}) category (y axis) variable
#' @param color_by_var (\code{choices_selected}) variable defines color blocks within each bar
#' @param count_by_var (\code{choices_selected}) variable defines how x axis is calculated
#' @param facet_var (\code{choices_selected}) variable for row facets
#' @param sort_by_var (\code{choices_selected}) argument for order of class and term elements in table,
#'   default here is "count"
#' @param legend_on (\code{boolean}) value for whether legend is displayed
#' @param plot_height (\code{numeric}) range of plot height - 3 values
#'
#' @details \code{filter_var} option is designed to work in conjuction with
#'   filtering function provided by \code{teal} (encoding panel on the right
#'   hand side of the shiny app). It can be used as quick access to pre-defined
#'   subsets of the domain datasets (not subject-level dataset) to be used for
#'   analysis, denoted by an value of "Y". Each variable within the
#'   \code{filter_var_choices} is expected to contain values of either "Y" or
#'   "N". If multiple variables are selected as \code{filter_var}, only
#'   observations with "Y" value in each and every selected variables will be
#'   used for subsequent analysis. Flag variables (from ADaM datasets) can be
#'   used directly as filter.
#' @details
#'
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
#' #Example butterfly plot
#' library(dplyr)
#'
#' ASL <- mutate(rADSL, DOSE = paste(sample(1:3, nrow(rADSL), replace = T), "UG"), USUBJID = SUBJID)
#' AAE <- mutate(
#'   rADAE,
#'   flag1 = ifelse(AETOXGR == 1, 1, 0),
#'   flag2 = ifelse(AETOXGR == 2, 1, 0),
#'   flag3 = ifelse(AETOXGR == 3, 1, 0),
#'   flag1_filt = rep("Y", nrow(AAE)),
#'   USUBJID = SUBJID
#' )
#'
#' app <- init(
#'   data = cdisc_data(ASL = ASL, AAE = AAE, code = paste0(c(
#'      'ASL <- mutate(rADSL, DOSE = paste(sample(1:3, nrow(rADSL), replace = T), "UG"),
#'         USUBJID = SUBJID)',
#'      'AAE <- mutate(
#'       rADAE,
#'       flag1 = ifelse(AETOXGR == 1, 1, 0),
#'       flag2 = ifelse(AETOXGR == 2, 1, 0),
#'       flag3 = ifelse(AETOXGR == 3, 1, 0),
#'       flag1_filt = rep("Y", nrow(AAE)),
#'       USUBJID = SUBJID
#'      )'), collapse = ";")
#'   ),
#'   modules = root_modules(
#'     tm_g_butterfly(
#'        label = "Butterfly Plot",
#'        dataname = "AAE",
#'        right_var = choices_selected(selected = "SEX", choices = c("DOSE", "SEX", "ARM",
#'          "RACE", "flag1", "flag2","flag3")),
#'        left_var = choices_selected(selected = "RACE", choices = c("DOSE", "SEX", "ARM",
#'           "RACE", "flag1", "flag2", "flag3")),
#'        category_var = choices_selected(selected = "AEBODSYS", choices = c("AEDECOD", "AEBODSYS")),
#'        color_by_var = choices_selected(selected = "AETOXGR", choices = c("AETOXGR", "None")),
#'        count_by_var = choices_selected(selected = "# of patients",
#'          choices = c("# of patients", "# of AEs")),
#'        facet_var = choices_selected(selected = NULL, choices = c("RACE", "SEX", "ARM")),
#'        sort_by_var = choices_selected(selected = "count", choices = c("count", "alphabetical")),
#'        legend_on = TRUE,
#'        plot_height = c(600, 200, 2000)
#'    )
#'   )
#' )
#'
#' shinyApp(app$ui, app$server)
#'
#' }
#'
tm_g_butterfly <- function(label,
                           dataname,
                           filter_var = NULL,
                           right_var,
                           left_var,
                           category_var,
                           color_by_var,
                           count_by_var,
                           facet_var = NULL,
                           sort_by_var = choices_selected(selected = "count", choices = c("count", "alphabetical")),
                           legend_on = TRUE,
                           plot_height = c(600, 200, 2000),
                           pre_output = NULL,
                           post_output = NULL) {

  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.choices_selected(filter_var) || is.null(filter_var))
  stopifnot(is.choices_selected(right_var))
  stopifnot(is.choices_selected(left_var))
  stopifnot(is.choices_selected(category_var))
  stopifnot(is.choices_selected(color_by_var))
  stopifnot(is.choices_selected(count_by_var))
  stopifnot(is.choices_selected(facet_var) || is.null(facet_var))
  stopifnot(is.choices_selected(sort_by_var))
  stopifnot(is.logical.single(legend_on))
  stopifnot(is.numeric.vector(plot_height))

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_butterfly,
    server_args = list(dataname = dataname),
    ui = ui_g_butterfly,
    ui_args = args
  )

}

ui_g_butterfly <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("butterflyplot"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Dataset is:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("filter_var"),
        label = div("Preset Data Filters",
        tags$br(),
        helpText("Observations with value of 'Y' for selected variable(s) will be used for analysis")),
        choices = a$filter_var$choices,
        selected = a$filter_var$selected, multiple = TRUE),
      optionalSelectInput(
        ns("right_ch"),
        "Right Dichotomization Variable",
        a$right_var$choices,
        a$right_var$selected,
        multiple = FALSE),
      checkboxGroupInput(
        ns("right_v"),
        "Choose one",
        a$right_var$choices,
        a$right_var$selected),
      optionalSelectInput(
        ns("left_ch"),
        "Left Dichotomization Variable",
        a$left_var$choices,
        a$left_var$selected,
        multiple = FALSE),
      checkboxGroupInput(
        ns("left_v"),
        "Choose one",
        a$left_var$choices,
        a$left_var$selected),
      optionalSelectInput(
        ns("category_var"),
        "Category Variable",
        a$category_var$choices,
        a$category_var$selected,
        multiple = FALSE),
      radioButtons(
        ns("color_by_var"),
        "Color Block By Variable",
        a$color_by_var$choices,
        a$color_by_var$selected),
      radioButtons(
        ns("count_by_var"),
        "Count By Variable",
        a$count_by_var$choices,
        a$count_by_var$selected),
      optionalSelectInput(
        ns("facet_var"),
        "Facet By Variable",
        a$facet_var$choices,
        a$facet_var$selected,
        multiple = TRUE),
      radioButtons(
        ns("sort_by_var"),
        "Sort By Variable",
        a$sort_by_var$choices,
        a$sort_by_var$selected),
      checkboxInput(
        ns("legend_on"),
        "Add legend",
        value = a$legend_on),
      tags$label(
        "Plot Settings",
        class="text-primary",
        style="margin-top: 15px;"),
      plot_height_input(id = ns("butterflyplot"), value = a$plot_height)
    ),
    forms = tags$div(
      actionButton(
        ns("show_rcode"),
        "Show R Code",
        width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_butterfly <- function(input, output, session, datasets, dataname) {

  init_chunks()

  #dynamic options for dichotomization variable
  observe({
    right_ch <- input$right_ch
    left_ch <- input$left_ch

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    ASL_df <- ASL_FILTERED %>% as.data.frame()
    AAE_df <- AAE_FILTERED %>% as.data.frame()

    options_r <- if (right_ch %in% names(ASL_df)) unique(ASL_df[, right_ch]) else unique(AAE_df[, right_ch])
    options_l <- if (left_ch %in% names(ASL_df)) unique(ASL_df[, left_ch]) else unique(AAE_df[, left_ch])

    updateCheckboxGroupInput(session, "right_v", choices = options_r, selected = options_r[1])
    updateCheckboxGroupInput(session, "left_v", choices = options_l, selected = options_l[1])
  })

  output$plot <- renderPlot({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AAE_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

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

    #if variable is not in ASL, then take from domain VADs
    varlist <- c(category_var, color_by_var, facet_var, filter_var, right_ch, left_ch)
    varlist_from_asl <- varlist[varlist %in% names(ASL_FILTERED)]
    varlist_from_anl <- varlist[!varlist %in% names(ASL_FILTERED)]

    asl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_asl))
    aae_vars <- unique(c("USUBJID", "STUDYID", varlist_from_anl))

    aae_name <- paste0(dataname, "_FILTERED")
    assign(aae_name, AAE_FILTERED) # so that we can refer to the 'correct' data name

    chunks_reset(envir = environment())

    chunks_push(bquote({

      # aae_vars <- aae_vars[aae_vars %in% names(AAE_FILTERED)]
      # aae_vars <- aae_vars[!is.null(aae_vars)]

      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame()
      AAE <- .(as.name(aae_name))[, .(aae_vars)] %>% as.data.frame()
    }))


    if(!("NULL" %in% filter_var) && !is.null(filter_var)){
        chunks_push(bquote(
      AAE <- quick_filter(.(filter_var), AAE) %>% droplevels() %>% as.data.frame()
                ))
    }

    chunks_push_new_line()

    chunks_push(bquote({
      ANL_f  <- left_join(ASL, AAE, by = c("USUBJID", "STUDYID")) %>%
        as.data.frame()
      ANL_f <- na.omit(ANL_f)
    }))

    chunks_push_new_line()
    chunks_push_new_line()


    if(!is.null(right_v) && !is.null(left_v)){
      if(!(all(right_v %in% c(0, 1)))){
        chunks_push(bquote({
          right <- as.character(ANL_f[, .(right_ch)])
          right <- replace(right, !(right %in% .(input$right_v)), 0)
          right <- replace(right, right %in% .(input$right_v), 1)
          right_name <- paste(right_v, collapse = " - ")
        }))
      } else{
        chunks_push(bquote({
          right <- ANL_f[, .(right_ch)]
          right_name <- right_ch
        }))
      }

      chunks_push_new_line()

      if(!(all(left_v %in% c(0, 1)))){
        chunks_push(bquote({
          left <- as.character(ANL_f[, .(left_ch)])
          left <- replace(left, !(left %in% .(input$left_v)), 0)
          left <- replace(left, left %in% .(input$left_v), 1)
          left_name <- paste(left_v, collapse = " - ")
        }))
      } else{
        chunks_push(bquote({
          left <- ANL_f[, .(left_ch)]
          left_name <- left_ch
        }))
      }

    }
    chunks_push_new_line()

    chunks_eval()

    validate(need(chunks_is_ok(), "Data could not be processed."))

    if(!is.null(right_v) && !is.null(left_v)){
      chunks_push(call(
        "g_butterfly",
        category = bquote(ANL_f[, .(category_var)]),
        rightFlag = bquote(right),
        leftFlag = bquote(left),
        group_names = bquote(c(right_name, left_name)),
        block_count = count_by_var,
        block_color = if(color_by_var != "None"){
          bquote(ANL_f[, .(color_by_var)])
        } else {
          NULL
        },
        id = bquote(ANL_f$USUBJID),
        facet_rows = if(!is.null(facet_var)){
          bquote(ANL_f[,facet_var])
        } else {
          NULL
        },
        x_label = count_by_var,
        y_label = category_var,
        legend_label = color_by_var,
        sort_by = sort_by_var,
        show_legend = legend_on
      ))
    }

    chunks_eval()

  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height,
      id = "butterflyplot",
      plot_height = reactive(input$butterflyplot),
      plot_id = session$ns("plot")
  )

  observeEvent(input$show_rcode, {
    show_rcode_modal(
        title = "Butterfly plot",
        rcode = get_rcode(
            datasets = datasets,
            title = "Butterfly plot"
        )
    )
  })
}

