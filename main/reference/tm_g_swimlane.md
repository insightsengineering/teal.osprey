# Teal Module for `Swimlane` Plot

This is a teal module that generates a `swimlane` plot (bar plot with
markers) for `ADaM` data

## Usage

``` r
tm_g_swimlane(
  label,
  dataname,
  bar_var = teal.picks::variables(choices = is.numeric, selected = 1L),
  parentname = "ADSL",
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
  analysis data used for markers. Use `"ADSL"` when no markers are
  plotted.

- bar_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Subject-level numeric variable for
  bar length (from `parentname`).

- parentname:

  (`character(1)`)\
  analysis data used for several variables in the teal module, needs to
  be available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  The default is `"ADSL"`

- bar_color_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Subject-level color variable from
  `parentname`.

- sort_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Subject-level sort variable from
  `parentname`.

- marker_pos_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Marker position variable from
  `dataname`).

- marker_shape_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Marker shape variable from
  `dataname`.

- marker_shape_opt:

  (`numeric`)\
  Named vector mapping shape values to ggplot shapes.

- marker_color_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Marker color variable from
  `dataname`.

- marker_color_opt:

  (`character`)\
  Named vector mapping color values to colors.

- anno_txt_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Subject-level annotation variables
  from `parentname` (multiple selection allowed).

- vref_line:

  (`numeric`)\
  Vertical reference lines.

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

- x_label:

  (`character`)\
  Label of the x axis.

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

    tm_g_swimlane(
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

## Examples

``` r
data <- within(teal_data(), {
  library(nestcolor)
  library(dplyr)
  ADSL <- teal.data::rADSL %>%
    mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1) %>%
    filter(STRATA1 == "A" & ARMCD == "ARM A")
  ADRS <- teal.data::rADRS %>%
    filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
    mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
    rbind(teal.data::rADRS %>% filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
    arrange(USUBJID)
})

join_keys(data) <- default_cdisc_join_keys[names(data)]


app <- init(
  data = data,
  modules = modules(
    tm_g_swimlane(
      label = "Swimlane Plot",
      dataname = "ADRS",
      bar_var = variables(
        choices = c("TRTDURD", "EOSDY"),
        selected = "TRTDURD"
      ),
      bar_color_var = variables(
        choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "SEX"),
        selected = "EOSSTT"
      ),
      sort_var = variables(
        choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD"),
        selected = "ACTARMCD"
      ),
      marker_pos_var = variables(
        choices = c("ADY"),
        selected = "ADY"
      ),
      marker_shape_var = variables(
        selected = "AVALC",
        c("AVALC", "AVISIT")
      ),
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
      marker_color_var = variables(
        selected = "AVALC",
        choices = c("AVALC", "AVISIT")
      ),
      marker_color_opt = c(
        "CR" = "green", "PR" = "blue", "SD" = "goldenrod",
        "PD" = "red", "Death" = "black"
      ),
      vref_line = c(30, 60),
      anno_txt_var = variables(
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
