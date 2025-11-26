# Teal Module for `Swimlane` Plot

This is teal module that generates a `swimlane` plot (bar plot with
markers) for `ADaM` data

## Usage

``` r
tm_g_swimlane(
  label,
  dataname,
  bar_var,
  bar_color_var = NULL,
  sort_var = NULL,
  marker_pos_var = NULL,
  marker_shape_var = NULL,
  marker_shape_opt = NULL,
  marker_color_var = NULL,
  marker_color_opt = NULL,
  anno_txt_var = NULL,
  vref_line = NULL,
  plot_height = c(1200L, 400L, 5000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  x_label = "Time from First Treatment (Day)",
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- dataname:

  analysis data used for plotting, needs to be available in the list
  passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  If no markers are to be plotted in the module, `"ADSL"` should be the
  input. If markers are to be plotted, data name for the marker data
  should be the input

- bar_var:

  [teal.transform::choices_selected](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  subject-level numeric variable from dataset to plot as the bar length

- bar_color_var:

  [teal.transform::choices_selected](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  color by variable (subject-level)

- sort_var:

  `choices_selected` sort by variable (subject-level)

- marker_pos_var:

  [teal.transform::choices_selected](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  variable for marker position from marker data (Note: make sure that
  marker position has the same relative start day as bar length variable
  `bar_var`

- marker_shape_var:

  [teal.transform::choices_selected](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  marker shape variable from marker data

- marker_shape_opt:

  aesthetic values to map shape values (named vector to map shape values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `marker_shape_var` values, otherwise shape will be
  assigned by `ggplot` default

- marker_color_var:

  marker color variable from marker data

- marker_color_opt:

  aesthetic values to map color values (named vector to map color values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `marker_color_var` values, otherwise color will be
  assigned by `ggplot` default

- anno_txt_var:

  character vector with subject-level variable names that are selected
  as annotation

- vref_line:

  vertical reference lines

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

- x_label:

  the label of the x axis

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

## Examples

``` r
# Example using stream (ADaM) dataset
data <- teal_data() %>%
  within({
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL %>%
      mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1) %>%
      filter(STRATA1 == "A" & ARMCD == "ARM A")
    ADRS <- rADRS %>%
      filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
      mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
      rbind(rADRS %>% filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
      arrange(USUBJID)
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_swimlane(
      label = "Swimlane Plot",
      dataname = "ADRS",
      bar_var = choices_selected(
        selected = "TRTDURD",
        choices = c("TRTDURD", "EOSDY")
      ),
      bar_color_var = choices_selected(
        selected = "EOSSTT",
        choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "SEX")
      ),
      sort_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD")
      ),
      marker_pos_var = choices_selected(
        selected = "ADY",
        choices = c("ADY")
      ),
      marker_shape_var = choices_selected(
        selected = "AVALC",
        c("AVALC", "AVISIT")
      ),
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
      marker_color_var = choices_selected(
        selected = "AVALC",
        choices = c("AVALC", "AVISIT")
      ),
      marker_color_opt = c(
        "CR" = "green", "PR" = "blue", "SD" = "goldenrod",
        "PD" = "red", "Death" = "black"
      ),
      vref_line = c(30, 60),
      anno_txt_var = choices_selected(
        selected = c("ACTARM", "SEX"),
        choices = c(
          "ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1",
          "SEX", "RACE", "COUNTRY", "DCSREAS", "DCSREASP"
        )
      )
    )
  )
)
#> Initializing tm_g_swimlane
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
