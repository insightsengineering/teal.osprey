# Teal module for the `AE` overview

Display the `AE` overview plot as a shiny module

## Usage

``` r
tm_g_ae_oview(
  label,
  dataname,
  arm_var,
  flag_var_anl,
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

- arm_var:

  (`choices_selected`)  
  object with all available choices and the pre-selected option for
  variable names that can be used as `arm_var`. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details. Column `arm_var` in the `dataname` has to be a factor.

- flag_var_anl:

  ([`teal.transform::choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  `choices_selected` object with variables used to count adverse event
  sub-groups (e.g. Serious events, Related events, etc.)

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

## Examples

``` r
data <- teal_data() %>%
  within({
    library(dplyr)
    ADSL <- rADSL
    ADAE <- rADAE
    .add_event_flags <- function(dat) {
      dat <- dat %>%
        mutate(
          TMPFL_SER = AESER == "Y",
          TMPFL_REL = AEREL == "Y",
          TMPFL_GR5 = AETOXGR == "5",
          AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
          AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
        )
      labels <- c(
        "Serious AE", "Related AE", "Grade 5 AE",
        "AE related to A: Drug X", "AE related to B: Placebo"
      )
      cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
      for (i in seq_along(labels)) {
        attr(dat[[cols[i]]], "label") <- labels[i]
      }
      dat
    }
    ADAE <- .add_event_flags(ADAE)
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = choices_selected(
        selected = "ACTARM",
        choices = c("ACTARM", "ACTARMCD")
      ),
      flag_var_anl = choices_selected(
        selected = "AEREL1",
        choices = variable_choices(
          ADAE,
          c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
        ),
      ),
      plot_height = c(600, 200, 2000)
    )
  )
)
#> Initializing tm_g_ae_oview
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
