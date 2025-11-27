# Teal Module for Waterfall Plot

This is teal module that generates a waterfall plot for `ADaM` data

## Usage

``` r
tm_g_waterfall(
  label,
  dataname_tr = "ADTR",
  dataname_rs = "ADRS",
  bar_paramcd,
  bar_var,
  bar_color_var,
  bar_color_opt = NULL,
  sort_var,
  add_label_var_sl,
  add_label_paramcd_rs,
  anno_txt_var_sl,
  anno_txt_paramcd_rs,
  facet_var,
  ytick_at = 20,
  href_line = NULL,
  gap_point_val = NULL,
  show_value = TRUE,
  plot_height = c(1200L, 400L, 5000L),
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

- dataname_tr:

  tumor burden analysis data used in teal module to plot as bar height,
  needs to be available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- dataname_rs:

  response analysis data used in teal module to label response
  parameters, needs to be available in the list passed to the `data`
  argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- bar_paramcd:

  `choices_selected` parameter in tumor burden data that will be plotted
  as bar height

- bar_var:

  `choices_selected` numeric variable from dataset to plot the bar
  height, e.g., `PCHG`

- bar_color_var:

  `choices_selected` color by variable (subject level), `None`
  corresponds to `NULL`

- bar_color_opt:

  aesthetic values to map color values (named vector to map color values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `bar_color_var` values, otherwise color will be
  assigned by `ggplot` default, please note that `NULL` needs to be
  specified in this case

- sort_var:

  `choices_selected` sort by variable (subject level), `None`
  corresponds to `NULL`

- add_label_var_sl:

  `choices_selected` add label to bars (subject level), `None`
  corresponds to `NULL`

- add_label_paramcd_rs:

  `choices_selected` add label to bars (response dataset), `None`
  corresponds to `NULL`. At least one of `add_label_var_sl` and
  `add_label_paramcd_rs` needs to be `NULL`

- anno_txt_var_sl:

  `choices_selected` subject level variables to be displayed in the
  annotation table, default is `NULL`

- anno_txt_paramcd_rs:

  `choices_selected` analysis dataset variables to be displayed in the
  annotation table, default is `NULL`

- facet_var:

  `choices_selected` facet by variable (subject level), `None`
  corresponds to `NULL`

- ytick_at:

  bar height axis interval, default is 20

- href_line:

  numeric vector to plot horizontal reference lines, default is `NULL`

- gap_point_val:

  singular numeric value for adding bar break when some bars are
  significantly higher than others, default is `NULL`

- show_value:

  boolean of whether value of bar height is shown, default is `TRUE`

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

Ting Qi (qit3) <qit3@gene.com>

houx14 <houx14@gene.com>

## Examples

``` r
data <- teal_data() %>%
  within({
    library(nestcolor)
    ADSL <- rADSL
    ADRS <- rADRS
    ADTR <- rADTR
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
      bar_paramcd = choices_selected(c("SLDINV"), "SLDINV"),
      bar_var = choices_selected(c("PCHG", "AVAL"), "PCHG"),
      bar_color_var = choices_selected(c("ARMCD", "SEX"), "ARMCD"),
      bar_color_opt = NULL,
      sort_var = choices_selected(c("ARMCD", "SEX"), NULL),
      add_label_var_sl = choices_selected(c("SEX", "EOSDY"), NULL),
      add_label_paramcd_rs = choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
      anno_txt_var_sl = choices_selected(c("SEX", "ARMCD", "BMK1", "BMK2"), NULL),
      anno_txt_paramcd_rs = choices_selected(c("BESRSPI", "OBJRSPI"), NULL),
      facet_var = choices_selected(c("SEX", "ARMCD", "STRATA1", "STRATA2"), NULL),
      href_line = "-30, 20"
    )
  )
)
#> Initializing tm_g_waterfall
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
