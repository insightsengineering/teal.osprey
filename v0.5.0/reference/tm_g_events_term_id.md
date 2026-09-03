# Events by Term Plot Teal Module

Display Events by Term plot as a shiny module

## Usage

``` r
tm_g_events_term_id(
  label,
  dataname,
  parentname,
  term_var = teal.picks::variables(dplyr::starts_with("AE")),
  arm_var = teal.picks::variables(dplyr::starts_with("ACTARM")),
  fontsize = c(5, 3, 7),
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
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

- parentname:

  (`character(1)`)\
  analysis data used for several variables in the teal module, needs to
  be available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  The default is `"ADSL"`

- term_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Object with all available choices
  and pre-selected option names that can be used to specify the term for
  events.

- arm_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object.\
  `choices_selected` is being deprecated as an argument type and will be
  removed in the future. Object with all available choices and the
  pre-selected option for variable names that can be used as `arm_var`.
  Column `arm_var` in the `dataname` has to be a factor.

- fontsize:

  (`numeric(1)` or `numeric(3)`)\
  Defines initial possible range of font-size. `fontsize` is set for
  [`teal.widgets::optionalSliderInputValMinMax()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html)
  which controls font-size in the output plot.

- plot_height:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

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

- `plot` (`grob`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_events_term_id(
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

Liming Li (lil128) <liming.li@roche.com>

Molly He (hey59) <hey59@gene.com>

## Examples

``` r
data <- within(teal_data(), {
  ADSL <- teal.data::rADSL
  ADAE <- teal.data::rADAE
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parentname = "ADSL",
      term_var = variables(
        choices = c(
          "AEDECOD", "AETERM",
          "AEHLT", "AELLT", "AEBODSYS"
        ),
        selected = "AEDECOD"
      ),
      arm_var = variables(
        choices = c("ACTARM", "ACTARMCD"),
        selected = "ACTARMCD"
      ),
      plot_height = c(600, 200, 2000)
    )
  )
)
#> Initializing tm_g_events_term_id
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
