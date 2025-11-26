# teal module for the `AE` by subgroups

Display the `AE` by subgroups plot as a teal module

## Usage

``` r
tm_g_ae_sub(
  label,
  dataname,
  arm_var,
  group_var,
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  fontsize = c(5, 3, 7),
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

- arm_var:

  (`choices_selected`)  
  object with all available choices and the pre-selected option for
  variable names that can be used as `arm_var`. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details. Column `arm_var` in the `dataname` has to be a factor.

- group_var:

  (`choices_selected`) subgroups variables. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details.

- plot_height:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- fontsize:

  (`numeric(1)` or `numeric(3)`)  
  Defines initial possible range of font-size. `fontsize` is set for
  [`teal.widgets::optionalSliderInputValMinMax()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html)
  which controls font-size in the output plot.

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

Liming Li (Lil128) <liming.li@roche.com>

Molly He (hey59) <hey59@gene.com>

## Examples

``` r
# Example using stream (ADaM) dataset
data <- teal_data() %>%
  within({
    ADSL <- rADSL
    ADAE <- rADAE
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_ae_sub(
      label = "AE by Subgroup",
      dataname = "ADAE",
      arm_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("ACTARM", "ACTARMCD")
      ),
      group_var = choices_selected(
        selected = c("SEX", "REGION1", "RACE"),
        choices = c("SEX", "REGION1", "RACE")
      ),
      plot_height = c(600, 200, 2000)
    )
  )
)
#> Initializing tm_g_ae_sub
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
