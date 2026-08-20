# Teal Module for Waterfall Plot

This is teal module that generates a waterfall plot for `ADaM` data

## Usage

``` r
tm_g_waterfall(
  label,
  parentname = "ADSL",
  dataname_tr = "ADTR",
  dataname_rs = "ADRS",
  bar_paramcd = teal.picks::values(choices = teal.picks::is_categorical(), multiple =
    FALSE),
  bar_var = teal.picks::variables(choices = is.numeric, multiple = FALSE),
  bar_color_var = teal.picks::variables(choices = teal.picks::is_categorical(max.len =
    20), selected = NULL),
  bar_color_opt = NULL,
  sort_var = teal.picks::variables(selected = NULL),
  add_label_var_sl = teal.picks::variables(selected = NULL),
  add_label_paramcd_rs = teal.picks::values(selected = NULL, multiple = FALSE),
  anno_txt_var_sl = teal.picks::variables(selected = NULL, multiple = TRUE),
  anno_txt_paramcd_rs = teal.picks::values(selected = NULL),
  facet_var = teal.picks::variables(selected = NULL),
  ytick_at = 20,
  href_line = NULL,
  gap_point_val = NULL,
  show_value = TRUE,
  plot_height = c(1200L, 400L, 5000L),
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

- parentname:

  (`character(1)`)\
  analysis data used for several variables in the teal module, needs to
  be available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  The default is `"ADSL"`

- dataname_tr:

  (`character(1)`) tumor burden analysis data used in teal module to
  plot as bar height, needs to be available in the list passed to the
  `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- dataname_rs:

  (`character(1)`) response analysis data used in teal module to label
  response parameters, needs to be available in the list passed to the
  `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- bar_paramcd:

  Either a
  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Parameter in tumor burden data that
  will be plotted as bar height.

- bar_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Numeric variable from dataset to
  plot the bar height, e.g., `PCHG`.

- bar_color_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Color by variable (subject level).
  Defaults to no selection.

- bar_color_opt:

  aesthetic values to map color values (named vector to map color values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `bar_color_var` values, otherwise color will be
  assigned by `ggplot` default, please note that `NULL` needs to be
  specified in this case

- sort_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Sort by variable (subject level).
  Defaults to no selection.

- add_label_var_sl:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Add label to bars (subject level).

- add_label_paramcd_rs:

  Either a
  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Add label to bars (response
  dataset). At least one of `add_label_var_sl` and
  `add_label_paramcd_rs` needs to not be selected.

- anno_txt_var_sl:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Subject level variables to be
  displayed in the annotation table, default is no selection.

- anno_txt_paramcd_rs:

  Either a
  ([`teal.picks::values()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Analysis dataset variables to be
  displayed in the annotation table, default is no selection.

- facet_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Facet by variable (subject level).
  Defaults to no selection.

- ytick_at:

  (`numeric(1)`) bar height axis interval, default is 20

- href_line:

  (`numeric`) numeric vector to plot horizontal reference lines, default
  is `NULL`

- gap_point_val:

  (`numeric(1)`) singular numeric value for adding bar break when some
  bars are significantly higher than others, default is `NULL`

- show_value:

  (`logical(1)`) boolean of whether value of bar height is shown,
  default is `TRUE`

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

Ting Qi (qit3) <qit3@gene.com>

houx14 <houx14@gene.com>

## Examples

``` r
data <- within(teal_data(), {
  library(nestcolor)
  ADSL <- teal.data::rADSL
  ADRS <- teal.data::rADRS
  ADTR <- teal.data::rADTR
  ADSL$SEX <- factor(ADSL$SEX, levels = unique(ADSL$SEX))
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_waterfall(
      label = "Waterfall",
      dataname_tr = "ADTR",
      dataname_rs = "ADRS",
      bar_paramcd = values(c("SLDINV"), "SLDINV", multiple = FALSE),
      bar_var = variables(c("PCHG", "AVAL"), "PCHG"),
      bar_color_var = variables(c("ARMCD", "SEX"), "ARMCD"),
      bar_color_opt = NULL,
      sort_var = variables(c("ARMCD", "SEX"), NULL),
      add_label_var_sl = variables(c("SEX", "EOSDY"), NULL),
      add_label_paramcd_rs = values(c("BESRSPI", "OBJRSPI"), NULL, multiple = FALSE),
      anno_txt_var_sl = variables(c("SEX", "ARMCD", "BMK1", "BMK2"), NULL, multiple = TRUE),
      anno_txt_paramcd_rs = values(c("BESRSPI", "OBJRSPI"), NULL),
      facet_var = variables(c("SEX", "ARMCD", "STRATA1", "STRATA2"), NULL),
      href_line = "-30, 20"
    )
  )
)
#> Initializing tm_g_waterfall
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
