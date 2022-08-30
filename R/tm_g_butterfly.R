
#' Butterfly plot Teal Module
#'
#' Display butterfly plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams argument_convention
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
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#'
#' # Example using stream (ADaM) dataset
#' library(dplyr)
#' library(scda)
#' library(nestcolor)
#'
#' set.seed(23)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
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
#'       code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl
#'               set.seed(23)
#'               ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), 'UG'))"
#'     ),
#'     cdisc_dataset("ADAE", ADAE,
#'       code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae
#'               ADAE <- mutate(ADAE,
#'               flag1 = ifelse(AETOXGR == 1, 1, 0),
#'               flag2 = ifelse(AETOXGR == 2, 1, 0),
#'               flag3 = ifelse(AETOXGR == 3, 1, 0),
#'               flag1_filt = rep('Y', n()))"
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_butterfly(
#'       label = "Butterfly Plot",
#'       dataname = "ADAE",
#'       right_var = choices_selected(
#'         selected = "SEX",
#'         choices = c("DOSE", "SEX", "ARM", "RACE", "flag1", "flag2", "flag3")
#'       ),
#'       left_var = choices_selected(
#'         selected = "RACE",
#'         choices = c("DOSE", "SEX", "ARM", "RACE", "flag1", "flag2", "flag3")
#'       ),
#'       category_var = choices_selected(selected = "AEBODSYS", choices = c("AEDECOD", "AEBODSYS")),
#'       color_by_var = choices_selected(selected = "AETOXGR", choices = c("AETOXGR", "None")),
#'       count_by_var = choices_selected(
#'         selected = "# of patients",
#'         choices = c("# of patients", "# of AEs")
#'       ),
#'       facet_var = choices_selected(selected = NULL, choices = c("RACE", "SEX", "ARM")),
#'       sort_by_var = choices_selected(selected = "count", choices = c("count", "alphabetical")),
#'       legend_on = TRUE,
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
                           sort_by_var = teal.widgets::choices_selected(
                             selected = "count", choices = c("count", "alphabetical")
                           ),
                           legend_on = TRUE,
                           plot_height = c(600L, 200L, 2000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL) {
  logger::log_info("Initializing tm_g_butterfly")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_class(filter_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(right_var, classes = "choices_selected")
  checkmate::assert_class(left_var, classes = "choices_selected")
  checkmate::assert_class(category_var, classes = "choices_selected")
  checkmate::assert_class(color_by_var, classes = "choices_selected")
  checkmate::assert_class(count_by_var, classes = "choices_selected")
  checkmate::assert_class(facet_var, classes = "choices_selected", null.ok = TRUE)
  checkmate::assert_class(sort_by_var, classes = "choices_selected")
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

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_butterfly,
    server_args = list(dataname = dataname, label = label, plot_height = plot_height, plot_width = plot_width),
    ui = ui_g_butterfly,
    ui_args = args
  )
}

ui_g_butterfly <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("butterflyplot"))
    ),
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Dataset is:", tags$code(a$dataname)),
      if (!is.null(a$filter_var)) {
        teal.widgets::optionalSelectInput(
          ns("filter_var"),
          label =
            "Preset Data Filters Observations with value of 'Y' for selected variable(s) will be used for analysis",
          choices = a$filter_var$choices,
          selected = a$filter_var$selected,
          multiple = TRUE
        )
      },
      teal.widgets::optionalSelectInput(
        ns("right_var"),
        "Right Dichotomization Variable",
        a$right_var$choices,
        a$right_var$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("right_val"),
        "Choose Up To 2:",
        multiple = TRUE,
        options = list(
          `max-options` = 2L,
          `max-options-text` = "no more than 2",
          `actions-box` = FALSE
        )
      ),
      teal.widgets::optionalSelectInput(
        ns("left_var"),
        "Left Dichotomization Variable",
        a$left_var$choices,
        a$left_var$selected,
        multiple = FALSE
      ),
      teal.widgets::optionalSelectInput(
        ns("left_val"),
        "Choose Up To 2:",
        multiple = TRUE,
        options = list(
          `max-options` = 2L,
          `max-options-text` = "no more than 2",
          `actions-box` = FALSE
        )
      ),
      teal.widgets::optionalSelectInput(
        ns("category_var"),
        "Category Variable",
        a$category_var$choices,
        a$category_var$selected,
        multiple = FALSE
      ),
      radioButtons(
        ns("color_by_var"),
        "Color Block By Variable",
        a$color_by_var$choices,
        a$color_by_var$selected
      ),
      radioButtons(
        ns("count_by_var"),
        "Count By Variable",
        a$count_by_var$choices,
        a$count_by_var$selected
      ),
      if (!is.null(a$facet_var)) {
        teal.widgets::optionalSelectInput(
          ns("facet_var"),
          "Facet By Variable",
          a$facet_var$choices,
          a$facet_var$selected,
          multiple = TRUE
        )
      },
      radioButtons(
        ns("sort_by_var"),
        "Sort By Variable",
        a$sort_by_var$choices,
        a$sort_by_var$selected
      ),
      checkboxInput(
        ns("legend_on"),
        "Add legend",
        value = a$legend_on
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_butterfly <- function(id, datasets, reporter, dataname, label, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("category_var", shinyvalidate::sv_required(message = "Please select category variable."))
    iv$add_rule("right_var", shinyvalidate::sv_required(message = "Please select Right Dichotomization Variable."))
    iv$add_rule("left_var", shinyvalidate::sv_required(message = "Please select Left Dichotomization Variable."))
    iv$enable()

    teal.code::init_chunks()

    options <- reactiveValues(r = NULL, l = NULL)
    vars <- reactiveValues(r = NULL, l = NULL)

    reactive_data <- reactive({
      ADSL <- datasets$get_data("ADSL", filtered = FALSE) # nolint
      ANL <- datasets$get_data(dataname, filtered = FALSE) # nolint

      ADSL_df <- ADSL %>% as.data.frame() # nolint
      ANL_df <- ANL %>% as.data.frame() # nolint

      list(ADSL_df = ADSL_df, ANL_df = ANL_df)
    })

    # dynamic options for dichotomization variable
    observeEvent(input$right_var,
      handlerExpr = {
        right_var <- input$right_var
        right_val <- isolate(input$right_val)
        current_r_var <- isolate(vars$r)
        if (is.null(right_var)) {
          teal.widgets::updateOptionalSelectInput(
            session,
            "right_val",
            choices = character(0),
            selected = character(0)
          )
        } else {
          data <- reactive_data()
          options$r <- if (right_var %in% names(data$ADSL_df)) {
            sort(unique(data$ADSL_df[, right_var]))
          } else {
            sort(unique(data$ANL_df[, right_var]))
          }

          selected <- if (length(right_val) > 0) {
            left_over <- right_val[right_val %in% options$r]
            if (length(left_over) > 0 && !is.null(current_r_var) && current_r_var == right_var) {
              left_over
            } else {
              options$r[1]
            }
          } else {
            options$r[1]
          }
          teal.widgets::updateOptionalSelectInput(
            session, "right_val",
            choices = as.character(options$r), selected = selected, label = "Choose Up To 2:"
          )
        }
        vars$r <- right_var
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$left_var,
      handlerExpr = {
        left_var <- input$left_var
        left_val <- isolate(input$left_val)
        current_l_var <- isolate(vars$l)
        if (is.null(left_var)) {
          teal.widgets::updateOptionalSelectInput(
            session, "left_val",
            choices = character(0), selected = character(0)
          )
        } else {
          data <- reactive_data()
          options$l <- if (left_var %in% names(data$ADSL_df)) {
            sort(unique(data$ADSL_df[, left_var]))
          } else {
            sort(unique(data$ANL_df[, left_var]))
          }

          selected <- if (length(left_val) > 0) {
            left_over <- left_val[left_val %in% options$l]
            if (length(left_over) > 0 && !is.null(current_l_var) && current_l_var == left_var) {
              left_over
            } else {
              options$l[1]
            }
          } else {
            options$l[1]
          }

          teal.widgets::updateOptionalSelectInput(
            session, "left_val",
            choices = as.character(options$l), selected = selected, label = "Choose Up To 2:"
          )
        }
        vars$l <- left_var
      },
      ignoreNULL = FALSE
    )



    plot_r <- reactive({
      validate(need(iv$is_valid(), "Misspecification error: please observe red flags in the interface."))
      ADSL <- datasets$get_data("ADSL", filtered = TRUE) # nolint
      ANL <- datasets$get_data(dataname, filtered = TRUE) # nolint

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

      validate(
        need(nrow(ADSL) > 0, "ADSL Data has no rows"),
        need(nrow(ANL) > 0, "ADAE Data has no rows")
      )
      validate(
        need(length(right_val) > 0, "No values of 'Right Dichotomization Variable' are checked"),
        need(length(left_val) > 0, "No values of 'Left Dichotomization Variable' are checked")
      )
      validate(need(
        any(c(ADSL[[right_var]] %in% right_val, ADSL[[left_var]] %in% left_val)),
        "ADSL Data contains no rows with either of the selected left or right dichotomization values (filtered out?)"
      ))

      # if variable is not in ADSL, then take from domain VADs
      varlist <- c(category_var, color_by_var, facet_var, filter_var, right_var, left_var)
      varlist_from_adsl <- intersect(varlist, names(ADSL))
      varlist_from_anl <- intersect(varlist, setdiff(names(ANL), names(ADSL)))

      adsl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_adsl)) # nolint
      anl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_anl)) # nolint

      anl_name <- dataname
      assign(anl_name, ANL) # so that we can refer to the 'correct' data name

      teal.code::chunks_reset(envir = environment())

      teal.code::chunks_push(
        id = "datasets call",
        expression = bquote({
          ADSL <- ADSL[, .(adsl_vars)] %>% as.data.frame() # nolint
          ANL <- .(as.name(anl_name))[, .(anl_vars)] %>% as.data.frame() # nolint
        })
      )

      teal.code::chunks_push_new_line()
      if (!("NULL" %in% filter_var) && !is.null(filter_var)) {
        teal.code::chunks_push(
          id = "data filter call",
          expression = bquote(
            ANL <- quick_filter(.(filter_var), ANL) %>% # nolint
              droplevels() %>%
              as.data.frame()
          )
        )
      }
      teal.code::chunks_push_new_line()

      teal.code::chunks_push(
        id = "ANL_f call",
        expression = bquote({
          ANL_f <- left_join(ADSL, ANL, by = c("USUBJID", "STUDYID")) %>% as.data.frame() # nolint
          ANL_f <- na.omit(ANL_f) # nolint
        })
      )

      teal.code::chunks_push_new_line()
      teal.code::chunks_push_new_line()

      if (!is.null(right_val) && !is.null(right_val)) {
        teal.code::chunks_push(
          id = "right/left call",
          expression = bquote({
            right <- ANL_f[, .(right_var)] %in% .(right_val)
            right_name <- paste(.(right_val), collapse = " - ")
            left <- ANL_f[, .(left_var)] %in% .(left_val)
            left_name <- paste(.(left_val), collapse = " - ")
          })
        )
      }

      teal.code::chunks_push_new_line()
      teal.code::chunks_safe_eval()

      if (!is.null(right_val) && !is.null(left_val)) {
        teal.code::chunks_push(
          id = "g_butterfly call",
          expression = bquote({
            osprey::g_butterfly(
              category = ANL_f[, .(category_var)],
              right_flag = right,
              left_flag = left,
              group_names = c(right_name, left_name),
              block_count = .(count_by_var),
              block_color = .(if (color_by_var != "None") {
                bquote(ANL_f[, .(color_by_var)])
              } else {
                NULL
              }),
              id = ANL_f$USUBJID,
              facet_rows = .(if (!is.null(facet_var)) {
                bquote(ANL_f[, .(facet_var)])
              } else {
                NULL
              }),
              x_label = .(count_by_var),
              y_label = .(category_var),
              legend_label = .(color_by_var),
              sort_by = .(sort_by_var),
              show_legend = .(legend_on)
            )
          })
        )
      }

      teal.code::chunks_safe_eval()
    })

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "butterflyplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = unique(c(
        dataname,
        vapply(X = dataname, FUN.VALUE = character(1), function(x) {
          if (inherits(datasets, "CDISCFilteredData")) datasets$get_parentname(x)
        })
      ))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Butterfly")
        card$append_text("Butterfly Plot", "header2")
        card$append_fs(datasets$get_filter_state())
        if (!is.null(input$filter_var) || !is.null(input$facet_var) || !is.null(input$sort_by_var)) {
          card$append_text("Selected Options", "header3")
        }
        if (!is.null(input$filter_var)) {
          card$append_text(paste0("Preset Data Filters: ", paste(input$filter_var, collapse = ", "), "."))
        }
        if (!is.null(input$facet_var)) {
          card$append_text(paste0("Faceted by: ", paste(input$facet_var, collapse = ", "), "."))
        }
        if (!is.null(input$sort_by_var)) {
          card$append_text(paste0("Sorted by: ", paste(input$sort_by_var, collapse = ", "), "."))
        }
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 2L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
