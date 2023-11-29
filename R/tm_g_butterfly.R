#' Butterfly plot Teal Module
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#'   used for subsequent analysis. Flag variables (from `ADaM` datasets) can be
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
#' # Example using stream (ADaM) dataset
#' data <- cdisc_data() |>
#'   within({
#'     library(dplyr)
#'     library(nestcolor)
#'     set.seed(23)
#'     ADSL <- rADSL
#'     ADAE <- rADAE
#'     ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
#'     ADAE <- mutate(
#'       ADAE,
#'       flag1 = ifelse(AETOXGR == 1, 1, 0),
#'       flag2 = ifelse(AETOXGR == 2, 1, 0),
#'       flag3 = ifelse(AETOXGR == 3, 1, 0),
#'       flag1_filt = rep("Y", n())
#'     )
#'   })
#'
#' datanames(data) <- c("ADSL", "ADAE")
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_butterfly(
#'       label = "Butterfly Plot",
#'       dataname = "ADAE",
#'       right_var = choices_selected(
#'         selected = "SEX",
#'         choices = c("SEX", "ARM", "RACE")
#'       ),
#'       left_var = choices_selected(
#'         selected = "RACE",
#'         choices = c("SEX", "ARM", "RACE")
#'       ),
#'       category_var = choices_selected(
#'         selected = "AEBODSYS",
#'         choices = c("AEDECOD", "AEBODSYS")
#'       ),
#'       color_by_var = choices_selected(
#'         selected = "AETOXGR",
#'         choices = c("AETOXGR", "None")
#'       ),
#'       count_by_var = choices_selected(
#'         selected = "# of patients",
#'         choices = c("# of patients", "# of AEs")
#'       ),
#'       facet_var = choices_selected(
#'         selected = NULL,
#'         choices = c("RACE", "SEX", "ARM")
#'       ),
#'       sort_by_var = choices_selected(
#'         selected = "count",
#'         choices = c("count", "alphabetical")
#'       ),
#'       legend_on = TRUE,
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
                           sort_by_var = teal.transform::choices_selected(
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
    datanames = c("ADSL", dataname),
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_butterfly <- function(id, data, filter_panel_api, reporter, dataname, label, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    iv <- reactive({
      ADSL <- data()[["ADSL"]] # nolint
      ANL <- data()[[dataname]] # nolint

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("category_var", shinyvalidate::sv_required(
        message = "Category Variable is required"
      ))
      iv$add_rule("right_var", shinyvalidate::sv_required(
        message = "Right Dichotomization Variable is required"
      ))
      iv$add_rule("left_var", shinyvalidate::sv_required(
        message = "Left Dichotomization Variable is required"
      ))
      iv$add_rule("right_var", ~ if (!is.factor(ANL[[.]])) {
        "Right Dichotomization Variable must be a factor variable, contact developer"
      })
      iv$add_rule("left_var", ~ if (!is.factor(ANL[[.]])) {
        "Left Dichotomization Variable must be a factor variable, contact developer"
      })
      iv$add_rule("right_val", shinyvalidate::sv_required(
        message = "At least one value of Right Dichotomization Variable must be selected"
      ))
      iv$add_rule("left_val", shinyvalidate::sv_required(
        message = "At least one value of Left Dichotomization Variable must be selected"
      ))
      iv$enable()
      iv
    })

    options <- reactiveValues(r = NULL, l = NULL)
    vars <- reactiveValues(r = NULL, l = NULL)

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
          options$r <- if (right_var %in% names(data()[["ADSL"]])) {
            levels(data()[["ADSL"]][[right_var]])
          } else {
            levels(data()[[dataname]][[right_var]])
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
          options$l <- if (left_var %in% names(data()[["ADSL"]])) {
            levels(data()[["ADSL"]][[left_var]])
          } else {
            levels(data()[[dataname]][[left_var]])
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

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        ADSL <- data()[["ADSL"]] # nolint
        ANL <- data()[[dataname]] # nolint

        teal::validate_has_data(ADSL, min_nrow = 0, msg = sprintf("%s Data is empty", "ADSL"))
        teal::validate_has_data(ANL, min_nrow = 0, msg = sprintf("%s Data is empty", dataname))

        teal::validate_inputs(iv())

        validate(
          need(
            all(input$right_val %in% ADSL[[input$right_var]]) &&
              all(input$left_val %in% ADSL[[input$left_var]]),
            "No observations for selected dichotomization values (filtered out?)"
          )
        )

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

        # if variable is not in ADSL, then take from domain VADs
        varlist <- c(category_var, color_by_var, facet_var, filter_var, right_var, left_var)
        varlist_from_adsl <- intersect(varlist, names(ADSL))
        varlist_from_anl <- intersect(varlist, setdiff(names(ANL), names(ADSL)))

        adsl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_adsl)) # nolint
        anl_vars <- unique(c("USUBJID", "STUDYID", varlist_from_anl)) # nolint

        q1 <- teal.code::eval_code(
          data(),
          code = bquote({
            ADSL <- ADSL[, .(adsl_vars)] %>% as.data.frame() # nolint
            ANL <- .(as.name(dataname))[, .(anl_vars)] %>% as.data.frame() # nolint
          })
        )

        if (!("NULL" %in% filter_var) && !is.null(filter_var)) {
          q1 <- teal.code::eval_code(
            q1,
            code = bquote(
              ANL <- quick_filter(.(filter_var), ANL) %>% # nolint
                droplevels() %>%
                as.data.frame()
            )
          )
        }

        q1 <- teal.code::eval_code(
          q1,
          code = bquote({
            ANL_f <- left_join(ADSL, ANL, by = c("USUBJID", "STUDYID")) %>% as.data.frame() # nolint
            ANL_f <- na.omit(ANL_f) # nolint
          })
        )

        if (!is.null(right_val) && !is.null(right_val)) {
          q1 <- teal.code::eval_code(
            q1,
            code = bquote({
              right <- ANL_f[, .(right_var)] %in% .(right_val)
              right_name <- paste(.(right_val), collapse = " - ")
              left <- ANL_f[, .(left_var)] %in% .(left_val)
              left_name <- paste(.(left_val), collapse = " - ")
            })
          )
        }

        if (!is.null(right_val) && !is.null(left_val)) {
          q1 <- teal.code::eval_code(
            q1,
            code = bquote(
              plot <- osprey::g_butterfly(
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
            )
          )
        }

        teal.code::eval_code(q1, quote(plot))
      })
    )

    plot_r <- reactive(output_q()[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "butterflyplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      title = paste("R code for", label),
      verbatim_content = reactive(teal.code::get_code(output_q()))
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Butterfly Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
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
        card$append_src(teal.code::get_code(output_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
  })
}
