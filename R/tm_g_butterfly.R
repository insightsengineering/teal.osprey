#' Butterfly plot Teal Module
#'
#' @description
#'
#' Display butterfly plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param filter_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with variable name of data filter, please see details regarding expected values,
#'   default is `NULL`.
#' @param right_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with dichotomization variable for the right side.
#' @param left_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with dichotomization variable for the left side.
#' @param category_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with category (y-axis) variable.
#' @param color_by_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with variable that defines color blocks within each bar.
#' @param count_by_var Either a ([`teal.picks::values()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with variable that defines how the x axis is calculated.
#' @param facet_var Either a ([`teal.picks::variables()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with variable for row facets.
#' @param sort_by_var Either a ([`teal.picks::values()`]) object or a
#'   ([`variables()`]) object.
#'   `choices_selected` is being deprecated as an argument type and will be removed in the future.
#'   Object with argument for order of class and term elements in table,
#'   default here is `"count"`.
#' @param legend_on (`boolean`) value for whether legend is displayed
#'
#' @details `filter_var` option is designed to work in conjunction with
#'   filtering function provided by `teal` (encoding panel on the right
#'   hand side of the shiny app). It can be used as quick access to predefined
#'   subsets of the domain datasets (not subject-level dataset) to be used for
#'   analysis, denoted by an value of "Y". Each variable within the
#'   `filter_var_choices` is expected to contain values of either "Y" or
#'   "N". If multiple variables are selected as `filter_var`, only
#'   observations with "Y" value in each and every selected variables will be
#'   used for subsequent analysis. Flag variables (from `ADaM` datasets) can be
#'   used directly as filter.
#'
#' @inherit argument_convention return
#' @inheritSection teal::example_module Reporting
#'
#' @template author_zhanc107
#' @template author_liaoc10
#'
#' @examples
#' data <- teal_data() %>%
#'   eval_code("set.seed(23) # @linksto ADSL") %>%
#'   within({
#'     library(nestcolor)
#'     library(dplyr)
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
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_butterfly(
#'       label = "Butterfly Plot",
#'       dataname = "ADAE",
#'       right_var = variables(
#'         choices = c("SEX", "ARM", "RACE"),
#'         selected = "SEX"
#'       ),
#'       left_var = variables(
#'         choices = c("SEX", "ARM", "RACE"),
#'         selected = "RACE"
#'       ),
#'       category_var = variables(
#'         choices = c("AEDECOD", "AEBODSYS"),
#'         selected = "AEBODSYS"
#'       ),
#'       color_by_var = variables(
#'         choices = c("AETOXGR"),
#'         selected = "AETOXGR",
#'         "allow-clear" = TRUE,
#'         fixed = FALSE
#'       ),
#'       count_by_var = values(
#'         choices = c("# of patients", "# of AEs"),
#'         selected = "# of patients"
#'       ),
#'       facet_var = variables(
#'         choices = c("RACE", "SEX", "ARM"),
#'         selected = NULL
#'       ),
#'       sort_by_var = values(
#'         choices = c("count", "alphabetical"),
#'         selected = "count"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
tm_g_butterfly <- function(label,
                           dataname,
                           filter_var = NULL,
                           right_var,
                           left_var,
                           category_var,
                           color_by_var,
                           count_by_var,
                           facet_var = NULL,
                           sort_by_var = teal.picks::values(
                             choices = c("count", "alphabetical"),
                             selected = "count"
                           ),
                           legend_on = TRUE,
                           plot_height = c(600L, 200L, 2000L),
                           plot_width = NULL,
                           pre_output = NULL,
                           post_output = NULL,
                           transformators = list()) {
  message("Initializing tm_g_butterfly")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)

  right_var <- migrate_choices_selected_to_variables(right_var, multiple = FALSE)
  left_var <- migrate_choices_selected_to_variables(left_var, multiple = FALSE)
  category_var <- migrate_choices_selected_to_variables(category_var, multiple = FALSE)
  color_by_var <- migrate_choices_selected_to_variables(color_by_var, multiple = FALSE)
  filter_var <- migrate_choices_selected_to_variables(filter_var, null.ok = TRUE)
  facet_var <- migrate_choices_selected_to_variables(facet_var, null.ok = TRUE)
  count_by_var <- migrate_choices_selected_to_values(count_by_var)
  sort_by_var <- migrate_choices_selected_to_values(sort_by_var)

  right_var <- create_picks_helper(teal.picks::datasets(dataname), right_var)
  left_var <- create_picks_helper(teal.picks::datasets(dataname), left_var)
  category_var <- create_picks_helper(teal.picks::datasets(dataname), category_var)
  color_by_var <- create_picks_helper(teal.picks::datasets(dataname), color_by_var)
  if (!is.null(filter_var)) {
    filter_var <- create_picks_helper(teal.picks::datasets(dataname), filter_var)
  }
  if (!is.null(facet_var)) {
    facet_var <- create_picks_helper(teal.picks::datasets(dataname), facet_var)
  }

  right_var <- force_pick_selection(right_var, "right_var")
  left_var <- force_pick_selection(left_var, "left_var")
  category_var <- force_pick_selection(category_var, "category_var")
  color_by_var <- force_pick_selection(color_by_var, "color_by_var")

  checkmate::assert_class(count_by_var, "pick")
  checkmate::assert_class(sort_by_var, "pick")
  if (!is.null(filter_var)) checkmate::assert_class(filter_var, "picks")
  if (!is.null(facet_var)) checkmate::assert_class(facet_var, "picks")

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
    server_args = args[names(args) %in% names(formals(srv_g_butterfly))],
    ui = ui_g_butterfly,
    ui_args = args[names(args) %in% names(formals(ui_g_butterfly))],
    transformators = transformators
  )
}

ui_g_butterfly <- function(id,
                           filter_var,
                           right_var,
                           left_var,
                           category_var,
                           color_by_var,
                           count_by_var,
                           facet_var,
                           sort_by_var,
                           legend_on,
                           pre_output,
                           post_output) { # nolint: object_name_linter.
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("butterflyplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      if (!is.null(filter_var)) {
        tags$div(
          tags$strong("Preset Data Filters"),
          teal.picks::picks_ui(id = ns("filter_var"), picks = filter_var)
        )
      },
      tags$div(
        tags$strong("Right Dichotomization Variable"),
        teal.picks::picks_ui(id = ns("right_var"), picks = right_var)
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
      tags$div(
        tags$strong("Left Dichotomization Variable"),
        teal.picks::picks_ui(id = ns("left_var"), picks = left_var)
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
      tags$div(
        tags$strong("Category Variable"),
        teal.picks::picks_ui(id = ns("category_var"), picks = category_var)
      ),
      tags$div(
        tags$strong("Color Block By Variable"),
        teal.picks::picks_ui(id = ns("color_by_var"), picks = color_by_var)
      ),
      radioButtons(
        ns("count_by_var"),
        "Count By Variable",
        get_choices(count_by_var$choices),
        count_by_var$selected
      ),
      if (!is.null(facet_var)) {
        tags$div(
          tags$strong("Facet By Variable"),
          teal.picks::picks_ui(id = ns("facet_var"), picks = facet_var)
        )
      },
      radioButtons(
        ns("sort_by_var"),
        "Sort By Variable",
        get_choices(sort_by_var$choices),
        sort_by_var$selected
      ),
      checkboxInput(
        ns("legend_on"),
        "Add legend",
        value = legend_on
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# nolint start: object_name_linter.
srv_g_butterfly <- function(
  # nolint end: object_name_linter.
  id,
  data,
  right_var,
  left_var,
  category_var,
  color_by_var,
  count_by_var,
  sort_by_var,
  filter_var,
  facet_var,
  plot_height,
  plot_width
) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.osprey")

    # Build picks list (exclude NULL optional picks)
    picks_list <- list(
      right_var = right_var,
      left_var = left_var,
      category_var = category_var,
      color_by_var = color_by_var
    )
    if (!is.null(filter_var)) picks_list$filter_var <- filter_var
    if (!is.null(facet_var)) picks_list$facet_var <- facet_var

    # Initialize picks selectors
    selectors <- teal.picks::picks_srv(
      picks = picks_list,
      data = data
    )

    # Merge datasets based on picks selections
    merged <- teal.picks::merge_srv(
      "merge",
      data = data,
      selectors = selectors,
      output_name = "ANL"
    )

    options <- reactiveValues(r = NULL, l = NULL)
    vars <- reactiveValues(r = NULL, l = NULL)

    # dynamic options for right dichotomization variable values
    observeEvent(merged$variables()$right_var,
      handlerExpr = {
        right_var_name <- merged$variables()$right_var
        right_val <- isolate(input$right_val)
        current_r_var <- isolate(vars$r)
        if (is.null(right_var_name)) {
          teal.widgets::updateOptionalSelectInput(
            session,
            "right_val",
            choices = character(0),
            selected = character(0)
          )
        } else {
          options$r <- levels(merged$data()[["ANL"]][[right_var_name]])
          selected <- if (length(right_val) > 0) {
            left_over <- right_val[right_val %in% options$r]
            if (length(left_over) > 0 && !is.null(current_r_var) && current_r_var == right_var_name) {
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
        vars$r <- right_var_name
      },
      ignoreNULL = FALSE
    )

    # dynamic options for left dichotomization variable values
    observeEvent(merged$variables()$left_var,
      handlerExpr = {
        left_var_name <- merged$variables()$left_var
        left_val <- isolate(input$left_val)
        current_l_var <- isolate(vars$l)
        if (is.null(left_var_name)) {
          teal.widgets::updateOptionalSelectInput(
            session, "left_val",
            choices = character(0), selected = character(0)
          )
        } else {
          options$l <- levels(merged$data()[["ANL"]][[left_var_name]])
          selected <- if (length(left_val) > 0) {
            left_over <- left_val[left_val %in% options$l]
            if (length(left_over) > 0 && !is.null(current_l_var) && current_l_var == left_var_name) {
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
        vars$l <- left_var_name
      },
      ignoreNULL = FALSE
    )

    output_q <- shiny::debounce(
      millis = 200,
      r = reactive({
        qenv <- merged$data()

        teal.reporter::teal_card(qenv) <-
          c(
            teal.reporter::teal_card(qenv),
            teal.reporter::teal_card("## Module's output(s)")
          )
        qenv <- teal.code::eval_code(qenv, "library(dplyr)")

        ANL <- qenv[["ANL"]]

        right_var_name <- merged$variables()$right_var
        left_var_name <- merged$variables()$left_var
        category_var_name <- merged$variables()$category_var
        color_by_var_name <- merged$variables()$color_by_var
        count_by_var_name <- input$count_by_var
        sort_by_var_name <- input$sort_by_var
        filter_var_name <- merged$variables()$filter_var
        facet_var_name <- merged$variables()$facet_var

        teal::validate_has_data(ANL, min_nrow = 1, msg = "ANL Data is empty")

        teal::validate_input(
          "right_var",
          condition = length(right_var_name) > 0,
          message = "Right Dichotomization Variable is required."
        )
        teal::validate_input(
          "left_var",
          condition = length(left_var_name) > 0,
          message = "Left Dichotomization Variable is required."
        )
        teal::validate_input(
          "category_var",
          condition = length(category_var_name) > 0,
          message = "Category Variable is required."
        )
        teal::validate_input(
          "right_var",
          condition = length(ANL[[right_var_name]]) > 0 && is.factor(ANL[[right_var_name]]),
          message = "Right Dichotomization Variable must be a factor variable, contact developer."
        )
        teal::validate_input(
          "left_var",
          condition = length(ANL[[left_var_name]]) > 0 && is.factor(ANL[[left_var_name]]),
          message = "Left Dichotomization Variable must be a factor variable, contact developer."
        )

        right_val <- input$right_val
        left_val <- input$left_val
        legend_on <- input$legend_on

        teal::validate_input(
          "right_val",
          condition = length(right_val) > 0,
          message = "At least one value of Right Dichotomization Variable must be selected."
        )
        teal::validate_input(
          "left_val",
          condition = length(left_val) > 0,
          message = "At least one value of Left Dichotomization Variable must be selected."
        )

        teal::validate_input(
          c("right_val", "left_val"),
          condition = all(right_val %in% ANL[[right_var_name]]) &&
            all(left_val %in% ANL[[left_var_name]]),
          message = "No observations for selected dichotomization values (filtered out?)"
        )

        q1 <- teal.code::eval_code(
          qenv,
          code = bquote({
            right <- ANL[[.(right_var_name)]] %in% .(right_val)
            right_name <- paste(.(right_val), collapse = " - ")
            left <- ANL[[.(left_var_name)]] %in% .(left_val)
            left_name <- paste(.(left_val), collapse = " - ")
          })
        )

        # This is redundant, only added to avoid NOTE in R CMD check
        right <- q1[["right"]]
        right_name <- q1[["right_name"]]
        left <- q1[["left"]]
        left_name <- q1[["left_name"]]

        teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Plot")

        if (!is.null(filter_var_name) || !is.null(facet_var_name) || !is.null(sort_by_var_name)) {
          teal.reporter::teal_card(q1) <- c(teal.reporter::teal_card(q1), "### Selected Options")
        }
        if (!is.null(filter_var_name)) {
          teal.reporter::teal_card(q1) <- c(
            teal.reporter::teal_card(q1),
            sprintf("Preset Data Filters: %s.", paste(filter_var_name, collapse = ", "))
          )
        }
        if (!is.null(facet_var_name)) {
          teal.reporter::teal_card(q1) <- c(
            teal.reporter::teal_card(q1),
            sprintf("Faceted by: %s.", paste(facet_var_name, collapse = ", "))
          )
        }
        if (!is.null(sort_by_var_name)) {
          teal.reporter::teal_card(q1) <- c(
            teal.reporter::teal_card(q1),
            sprintf("Sorted by: %s.", paste(sort_by_var_name, collapse = ", "))
          )
        }

        q1 <- within(
          q1,
          {
            plot <- osprey::g_butterfly(
              category = ANL[[category_var_name]],
              right_flag = right,
              left_flag = left,
              group_names = c(right_name, left_name),
              block_count = count_by_var_name,
              block_color = if (!is.null(color_by_var_name)) {
                ANL[[color_by_var_name]]
              } else {
                NULL
              },
              id = ANL$USUBJID,
              facet_rows = if (!is.null(facet_var_name)) {
                ANL[[facet_var_name]]
              } else {
                NULL
              },
              x_label = count_by_var_name,
              y_label = category_var_name,
              legend_label = if (!is.null(color_by_var_name)) {
                color_by_var_name
              } else {
                ""
              },
              sort_by = sort_by_var_name,
              show_legend = legend_on
            )
          },
          category_var_name = category_var_name,
          color_by_var_name = color_by_var_name,
          count_by_var_name = count_by_var_name,
          facet_var_name = facet_var_name,
          sort_by_var_name = sort_by_var_name,
          legend_on = legend_on
        )

        q1
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

    set_chunk_dims(pws, output_q)
  })
}
