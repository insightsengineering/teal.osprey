# Spider plot Teal Module

Display spider plot as a shiny module

## Usage

``` r
tm_g_spiderplot(
  label,
  dataname,
  paramcd = teal.picks::variables(choices = dplyr::starts_with("PARAMCD"), selected = 1L),
  x_var = teal.picks::variables(choices = dplyr::where(is.numeric), selected = 1L),
  y_var = teal.picks::variables(choices = teal.picks::is_categorical(), selected = 1L),
  marker_var = teal.picks::variables(choices = teal.picks::is_categorical(), selected =
    1L),
  line_colorby_var = teal.picks::variables(choices = teal.picks::is_categorical(),
    selected = 1L),
  xfacet_var = NULL,
  yfacet_var = NULL,
  vref_line = NULL,
  href_line = NULL,
  anno_txt_var = TRUE,
  legend_on = FALSE,
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

- paramcd:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object.\
  `choices_selected` is being deprecated as an argument type and will be
  removed in the future. Variable value designating the studied
  parameter.

- x_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For x-axis variables.

- y_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For y-axis variables.

- marker_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For marker symbol.

- line_colorby_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For line color.

- xfacet_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For x facets.

- yfacet_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For y facets.

- vref_line:

  vertical reference lines

- href_line:

  horizontal reference lines

- anno_txt_var:

  annotation text

- legend_on:

  boolean value for whether legend is displayed

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

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_spiderplot(
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
data <- within(teal_data(), {
  library(nestcolor)
  ADSL <- teal.data::rADSL
  ADTR <- teal.data::rADTR
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_spiderplot(
      label = "Spider plot (picks)",
      dataname = "ADTR",
      paramcd = variables(
        choices = "PARAMCD",
        selected = "PARAMCD"
      ),
      x_var = variables(
        choices = dplyr::where(is.numeric),
        selected = 1L
      ),
      y_var = variables(
        choices = c("PCHG", "CHG", "AVAL"),
        selected = "PCHG"
      ),
      marker_var = variables(
        choices = c("SEX", "RACE", "USUBJID"),
        selected = "SEX"
      ),
      line_colorby_var = variables(
        choices = c("SEX", "USUBJID", "RACE"),
        selected = "SEX"
      ),
      xfacet_var = variables(
        choices = c("SEX", "ARM"),
        selected = "SEX"
      ),
      yfacet_var = variables(
        choices = c("SEX", "ARM"),
        selected = "ARM"
      ),
      vref_line = "10, 37",
      href_line = "-20, 0"
    )
  )
)
#> Initializing tm_g_spiderplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
