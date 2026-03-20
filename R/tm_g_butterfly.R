#' Butterfly plot Teal Module
#'
#' @description
#'
#' Display butterfly plot as a shiny module
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @inheritParams argument_convention
#' @param filter_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with variable name of data filter, please see details regarding
#'   expected values, default is `NULL`
#' @param right_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with dichotomization variable for right side
#' @param left_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with dichotomization variable for left side
#' @param category_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with category (y axis) variable
#' @param color_by_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with variable that defines color blocks within each bar
#' @param count_by_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::values()]`)
#'   object with variable that defines how the x axis is calculated
#' @param facet_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::variables()]`)
#'   object with variable for row facets
#' @param sort_by_var Either a ([`teal.transform::choices_selected`])
#'   `choices_selected` object or a (`[teal.picks::values()]`)
#'   object with argument for order of class and term elements in table,
#'   default here is `"count"`
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
#' @export
tm_g_butterfly <- function(label,
                           dataname,
                           filter_var,
                           right_var,
                           left_var,
                           category_var,
                           color_by_var,
                           count_by_var,
                           facet_var,
                           sort_by_var,
                           legend_on,
                           plot_height,
                           plot_width,
                           pre_output,
                           post_output,
                           transformators = list()) {
  UseMethod("tm_g_butterfly", right_var)
}

#' @rdname tm_g_butterfly
#' @examples
#' # Example using stream (ADaM) dataset
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
#' @export
tm_g_butterfly.default <- function(label,
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
                                   post_output = NULL,
                                   transformators = list()) {
  message("Initializing tm_g_butterfly")
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

  right_var <- teal.picks::as.picks(right_var)
  left_var <- teal.picks::as.picks(left_var)
  category_var <- teal.picks::as.picks(category_var)
  color_by_var <- teal.picks::as.picks(color_by_var)
  if (!is.null(filter_var)) {
    filter_var <- teal.picks::as.picks(filter_var)
  }
  if (!is.null(facet_var)) {
    facet_var <- teal.picks::as.picks(facet_var)
  }

  count_by_var <- teal.picks::values(choices = count_by_var$choices, selected = count_by_var$selected)
  sort_by_var <- teal.picks::values(choices = sort_by_var$choices, selected = sort_by_var$selected)

  tm_g_butterfly.pick(
    label = label,
    dataname = dataname,
    filter_var = filter_var,
    right_var = right_var,
    left_var = left_var,
    category_var = category_var,
    color_by_var = color_by_var,
    count_by_var = count_by_var,
    facet_var = facet_var,
    sort_by_var = sort_by_var,
    legend_on = legend_on,
    plot_height = plot_height,
    plot_width = plot_width,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators
  )
}
