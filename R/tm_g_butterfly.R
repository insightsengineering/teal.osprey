
#' Butterfly plot Teal Module
#'
#' Display butterfly plot as a shiny module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
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
#'
#' @details \code{filter_var} option is designed to work in conjunction with
#'   filtering function provided by \code{teal} (encoding panel on the right
#'   hand side of the shiny app). It can be used as quick access to predefined
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
#' #Example using stream (ADaM) dataset
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' set.seed(23)
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#' ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
#' ADAE <- mutate(
#'   ADAE,
#'   flag1 = ifelse(AETOXGR == 1, 1, 0),
#'   flag2 = ifelse(AETOXGR == 2, 1, 0),
#'   flag3 = ifelse(AETOXGR == 3, 1, 0),
#'   flag1_filt = rep("Y", n())
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = "ADSL <- radsl(cached = TRUE)
#'               set.seed(23)
#'               ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), 'UG'))"),
#'     cdisc_dataset("ADAE", ADAE,
#'       code = "ADAE <- radae(cached = TRUE)
#'               ADAE <- mutate(ADAE,
#'               flag1 = ifelse(AETOXGR == 1, 1, 0),
#'               flag2 = ifelse(AETOXGR == 2, 1, 0),
#'               flag3 = ifelse(AETOXGR == 3, 1, 0),
#'               flag1_filt = rep('Y', n()))"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_butterfly(
#'       label = "Butterfly Plot",
#'       dataname = "ADAE",
#'       right_var = choices_selected(selected = "SEX",
#'         choices = c("DOSE", "SEX", "ARM","RACE", "flag1", "flag2","flag3")),
#'       left_var = choices_selected(selected = "RACE",
#'         choices = c("DOSE", "SEX", "ARM", "RACE", "flag1", "flag2", "flag3")),
#'       category_var = choices_selected(selected = "AEBODSYS", choices = c("AEDECOD", "AEBODSYS")),
#'       color_by_var = choices_selected(selected = "AETOXGR", choices = c("AETOXGR", "None")),
#'       count_by_var = choices_selected(selected = "# of patients",
#'                                       choices = c("# of patients", "# of AEs")),
#'       facet_var = choices_selected(selected = NULL, choices = c("RACE", "SEX", "ARM")),
#'       sort_by_var = choices_selected(selected = "count", choices = c("count", "alphabetical")),
#'       legend_on = TRUE,
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#'   )
#'
#' \dontrun{
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
                           plot_height = c(600L, 200L, 2000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL) {

  stopifnot(is_character_single(label))
  stopifnot(is_character_single(dataname))
  stopifnot(is.choices_selected(filter_var) || is.null(filter_var))
  stopifnot(is.choices_selected(right_var))
  stopifnot(is.choices_selected(left_var))
  stopifnot(is.choices_selected(category_var))
  stopifnot(is.choices_selected(color_by_var))
  stopifnot(is.choices_selected(count_by_var))
  stopifnot(is.choices_selected(facet_var) || is.null(facet_var))
  stopifnot(is.choices_selected(sort_by_var))
  stopifnot(is_logical_single(legend_on))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_butterfly,
    server_args = list(dataname = dataname, plot_height = plot_height, plot_width = plot_width),
    ui = ui_g_butterfly,
    ui_args = args
  )

}

ui_g_butterfly <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("butterflyplot"), height = a$plot_height, width = a$plot_width)
      ),
    encoding =  div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Dataset is:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("filter_var"),
        label =
          "Preset Data Filters Observations with value of 'Y' for selected variable(s) will be used for analysis",
        choices = a$filter_var$choices,
        selected = a$filter_var$selected, multiple = TRUE),
      optionalSelectInput(
        ns("right_var"),
        "Right Dichotomization Variable",
        a$right_var$choices,
        a$right_var$selected,
        multiple = FALSE),
      checkboxGroupInput(
        ns("right_val"),
        "Choose one",
        a$right_var$choices,
        a$right_var$selected),
      optionalSelectInput(
        ns("left_var"),
        "Left Dichotomization Variable",
        a$left_var$choices,
        a$left_var$selected,
        multiple = FALSE),
      checkboxGroupInput(
        ns("left_val"),
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
        value = a$legend_on)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_butterfly <- function(input, output, session, datasets, dataname, plot_height, plot_width) {

  init_chunks()

  #dynamic options for dichotomization variable
  observe({

    req(!is.null(input$right_var) && !is.null(input$left_var))

    right_var <- input$right_var
    left_var <- input$left_var

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ADAE_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    ADSL_df <- ADSL_FILTERED %>% as.data.frame() # nolint
    ADAE_df <- ADAE_FILTERED %>% as.data.frame() # nolint

    options_r <- if (right_var %in% names(ADSL_df)) unique(ADSL_df[, right_var]) else unique(ADAE_df[, right_var])
    options_l <- if (left_var %in% names(ADSL_df)) unique(ADSL_df[, left_var]) else unique(ADAE_df[, left_var])

    updateCheckboxGroupInput(session, "right_val", choices = options_r, selected = options_r[1])
    updateCheckboxGroupInput(session, "left_val", choices = options_l, selected = options_l[1])
  })

  plot_r <- reactive({
    validate(need(input$category_var, "Please select a category variable."))

    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
    ADAE_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint

    right_var <- isolate(input$right_var)
    left_var <- isolate(input$left_var)
    right_val <- input$right_val
    left_val <- input$left_val
    category_var <- input$category_var
    color_by_var <- input$color_by_var
    count_by_var <- input$count_by_var
    legend_on <- input$legend_on
    facet_var <- input$facet_var
    sort_by_var <- input$sort_by_var
    filter_var <- input$filter_var

    #if variable is not in ADSL, then take from domain VADs

    varlist <- c(category_var, color_by_var, facet_var, filter_var, right_var, left_var)
    varlist_from_adsl <- intersect(varlist, names(ADSL_FILTERED))
    varlist_from_anl <- intersect(varlist, setdiff(names(ADAE_FILTERED), names(ADSL_FILTERED)))

    adsl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_adsl)) # nolint
    adae_vars <- unique(c("USUBJID", "STUDYID", varlist_from_anl)) # nolint

    adae_name <- paste0(dataname, "_FILTERED")
    assign(adae_name, ADAE_FILTERED) # so that we can refer to the 'correct' data name

    chunks_reset(envir = environment())

    chunks_push(bquote({
      ADSL <- ADSL_FILTERED[, .(adsl_vars)] %>% as.data.frame() # nolint
      ADAE <- .(as.name(adae_name))[, .(adae_vars)] %>% as.data.frame() # nolint
    }))

    chunks_push_new_line()
    if (!("NULL" %in% filter_var) && !is.null(filter_var)) {
      chunks_push(bquote(
        ADAE <- quick_filter(.(filter_var), ADAE) %>% # nolint
          droplevels() %>%
          as.data.frame()
      ))
    }
    chunks_push_new_line()

    chunks_push(bquote({
      ANL_f  <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID")) %>% as.data.frame() # nolint
      ANL_f <- na.omit(ANL_f) # nolint
    }))

    chunks_push_new_line()
    chunks_push_new_line()

    if (!is.null(right_val) && !is.null(right_val)) {
      chunks_push(bquote({
        right <- ANL_f[, .(right_var)] %in% .(right_val)
        right_name <- paste(.(right_val), collapse = " - ")
        left <- ANL_f[, .(left_var)] %in% .(left_val)
        left_name <- paste(.(left_val), collapse = " - ")
      }))
    }

    chunks_push_new_line()
    chunks_safe_eval()

    if (!is.null(right_val) && !is.null(left_val)) {
      chunks_push(call(
        "g_butterfly",
        category = bquote(ANL_f[, .(category_var)]),
        right_flag = bquote(right),
        left_flag = bquote(left),
        group_names = bquote(c(right_name, left_name)),
        block_count = count_by_var,
        block_color = if (color_by_var != "None") {
          bquote(ANL_f[, .(color_by_var)])
        } else {
          NULL
        },
        id = bquote(ANL_f$USUBJID),
        facet_rows = if (!is.null(facet_var)) {
          bquote(ANL_f[, .(facet_var)])
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

    chunks_safe_eval()

  })

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "butterflyplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width)

  callModule(
    module = get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "Butterfly plot"
  )
}
