# Butterfly plot Teal Module

Display butterfly plot as a shiny module

## Usage

``` r
tm_g_butterfly(
  label,
  dataname,
  filter_var = NULL,
  right_var = teal.picks::variables(is.factor),
  left_var = teal.picks::variables(is.factor),
  category_var = teal.picks::variables(teal.picks::is_categorical()),
  color_by_var = teal.picks::variables(dplyr::starts_with("AETO")),
  count_by_var = teal.picks::values(choices = c("# of patients", "# of AEs"), selected =
    "# of patients"),
  facet_var = NULL,
  sort_by_var = teal.picks::values(choices = c("count", "alphabetical"), selected =
    "count"),
  legend_on = TRUE,
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- dataname:

  (`character(1)`)\
  analysis data used in the teal module, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).

- filter_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with variable name of data
  filter, please see details regarding expected values, default is
  `NULL`.

- right_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with dichotomization
  variable for the right side.

- left_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with dichotomization
  variable for the left side.

- category_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with category (y-axis)
  variable.

- color_by_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with variable that defines
  color blocks within each bar.

- count_by_var:

  Either a
  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with variable that defines
  how the x axis is calculated.

- facet_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with variable for row
  facets.

- sort_by_var:

  Either a
  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with argument for order of
  class and term elements in table, default here is `"count"`.

- legend_on:

  (`boolean`) value for whether legend is displayed

- plot_height:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional, with text placed after the output to put the
  output into context. For example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of `teal_transform_module`)
  optional, decorators for the module `plot` output.

## Value

the
[`teal::module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
object.

## Details

`filter_var` option is designed to work in conjunction with filtering
function provided by `teal` (encoding panel on the right hand side of
the shiny app). It can be used as quick access to predefined subsets of
the domain datasets (not subject-level dataset) to be used for analysis,
denoted by an value of "Y". Each variable within the
`filter_var_choices` is expected to contain values of either "Y" or "N".
If multiple variables are selected as `filter_var`, only observations
with "Y" value in each and every selected variables will be used for
subsequent analysis. Flag variables (from `ADaM` datasets) can be used
directly as filter.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`grob`, `gtable`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_butterfly(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...), # applied to the `plot` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/latest-tag/articles/decorate-module-output.html).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Author

Carolyn Zhang (zhanc107) <carolyn.zhang@duke.edu>

Chendi Liao (liaoc10) <chendi.liao@roche.com>

## Examples

``` r
data <- teal_data() %>%
  eval_code("set.seed(23) # @linksto ADSL") %>%
  within({
    library(nestcolor)
    library(dplyr)
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE
    ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
    ADAE <- mutate(
      ADAE,
      flag1 = ifelse(AETOXGR == 1, 1, 0),
      flag2 = ifelse(AETOXGR == 2, 1, 0),
      flag3 = ifelse(AETOXGR == 3, 1, 0),
      flag1_filt = rep("Y", n())
    )
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = variables(
        choices = c("SEX", "ARM", "RACE"),
        selected = "SEX"
      ),
      left_var = variables(
        choices = c("SEX", "ARM", "RACE"),
        selected = "RACE"
      ),
      category_var = variables(
        choices = c("AEDECOD", "AEBODSYS"),
        selected = "AEBODSYS"
      ),
      color_by_var = variables(
        choices = c("AETOXGR"),
        selected = "AETOXGR",
        "allow-clear" = TRUE,
        fixed = FALSE
      ),
      count_by_var = values(
        choices = c("# of patients", "# of AEs"),
        selected = "# of patients"
      ),
      facet_var = variables(
        choices = c("RACE", "SEX", "ARM"),
        selected = NULL
      ),
      sort_by_var = values(
        choices = c("count", "alphabetical"),
        selected = "count"
      )
    )
  )
)
#> Initializing tm_g_butterfly
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
