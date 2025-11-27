# Events by Term Plot Teal Module

Display Events by Term plot as a shiny module

## Usage

``` r
tm_g_events_term_id(
  label,
  dataname,
  term_var,
  arm_var,
  fontsize = c(5, 3, 7),
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- dataname:

  (`character(1)`)  
  analysis data used in the teal module, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).

- term_var:

  [teal.transform::choices_selected](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  object with all available choices and pre-selected option names that
  can be used to specify the term for events

- arm_var:

  (`choices_selected`)  
  object with all available choices and the pre-selected option for
  variable names that can be used as `arm_var`. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details. Column `arm_var` in the `dataname` has to be a factor.

- fontsize:

  (`numeric(1)` or `numeric(3)`)  
  Defines initial possible range of font-size. `fontsize` is set for
  [`teal.widgets::optionalSliderInputValMinMax()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html)
  which controls font-size in the output plot.

- plot_height:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

## Value

the
[`teal::module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
object.

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
data <- teal_data() %>%
  within({
    ADSL <- rADSL
    ADAE <- rADAE
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      term_var = choices_selected(
        selected = "AEDECOD",
        choices = c(
          "AEDECOD", "AETERM",
          "AEHLT", "AELLT", "AEBODSYS"
        )
      ),
      arm_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("ACTARM", "ACTARMCD")
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
