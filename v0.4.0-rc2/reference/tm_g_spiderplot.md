# Spider plot Teal Module

Display spider plot as a shiny module

## Usage

``` r
tm_g_spiderplot(
  label,
  dataname,
  paramcd,
  x_var,
  y_var,
  marker_var,
  line_colorby_var,
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

- paramcd:

  (`character(1)` or `choices_selected`)  
  variable value designating the studied parameter. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details.

- x_var:

  x-axis variables

- y_var:

  y-axis variables

- marker_var:

  variable dictates marker symbol

- line_colorby_var:

  variable dictates line color

- xfacet_var:

  variable for x facets

- yfacet_var:

  variable for y facets

- vref_line:

  vertical reference lines

- href_line:

  horizontal reference lines

- anno_txt_var:

  annotation text

- legend_on:

  boolean value for whether legend is displayed

- plot_height:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)  
  vector to indicate default value, minimum and maximum values.

- pre_output:

  (`shiny.tag`) optional,  
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

Carolyn Zhang (zhanc107) <carolyn.zhang@duke.edu>

Chendi Liao (liaoc10) <chendi.liao@roche.com>

## Examples

``` r
# Example using stream (ADaM) dataset
data <- teal_data() %>%
  within({
    library(nestcolor)
    ADSL <- rADSL
    ADTR <- rADTR
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_spiderplot(
      label = "Spider plot",
      dataname = "ADTR",
      paramcd = choices_selected(
        choices = "SLDINV",
        selected = "SLDINV"
      ),
      x_var = choices_selected(
        choices = "ADY",
        selected = "ADY"
      ),
      y_var = choices_selected(
        choices = c("PCHG", "CHG", "AVAL"),
        selected = "PCHG"
      ),
      marker_var = choices_selected(
        choices = c("SEX", "RACE", "USUBJID"),
        selected = "SEX"
      ),
      line_colorby_var = choices_selected(
        choices = c("SEX", "USUBJID", "RACE"),
        selected = "SEX"
      ),
      xfacet_var = choices_selected(
        choices = c("SEX", "ARM"),
        selected = "SEX"
      ),
      yfacet_var = choices_selected(
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
