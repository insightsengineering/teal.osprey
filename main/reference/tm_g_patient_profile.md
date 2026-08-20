# Patient Profile plot teal module

Display patient profile plot as a shiny module

## Usage

``` r
tm_g_patient_profile(
  label = "Patient Profile Plot",
  patient_id = teal.picks::variables(choices = dplyr::starts_with("USUBJ"), selected =
    1L),
  sl_dataname,
  ex_dataname = NA,
  ae_dataname = NA,
  rs_dataname = NA,
  cm_dataname = NA,
  lb_dataname = NA,
  sl_start_date,
  ex_var = NULL,
  ae_var = NULL,
  ae_line_col_var = NULL,
  ae_line_col_opt = NULL,
  rs_var = NULL,
  cm_var = NULL,
  lb_var = NULL,
  x_limit = "-28, 365",
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

- patient_id:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Describing the unique subject ID
  selection.

- sl_dataname:

  (`character`) subject level dataset name, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- ex_dataname, ae_dataname, rs_dataname, cm_dataname, lb_dataname:

  (`character(1)`) names of exposure, adverse events, response,
  concomitant medications, and labs datasets, respectively; must be
  available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)\
  set to NA (default) to omit from analysis

- sl_start_date:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the study start date variable,
  usually set to treatment start date or randomization date.

- ex_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the exposure variable to plot
  as each line. Leave unspecified or set to `NULL` if exposure data is
  not available.

- ae_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the adverse event variable to
  plot as each line. Leave unspecified or set to `NULL` if adverse
  events data is not available.

- ae_line_col_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For coloring `AE` lines. Leave
  unspecified or set to `NULL` if adverse events data is not available.

- ae_line_col_opt:

  aesthetic values to map color values (named vector to map color values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `ae_line_col_var` values.\
  leave unspecified or set to `NULL` if adverse events data is not
  available

- rs_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the response variable to plot
  as each line. Leave unspecified or set to `NULL` if response data is
  not available.

- cm_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the concomitant medication
  variable to plot as each line. Leave unspecified or set to `NULL` if
  concomitant medications data is not available.

- lb_var:

  Either a
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, a full
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object, or a
  [`variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. For the lab variable to plot as
  each line. Leave unspecified or set to `NULL` if labs data is not
  available.

- x_limit:

  a single `character` string with two numbers separated by a comma
  indicating the x-axis limit, default is "-28, 365"

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

## Details

As the patient profile module plots different domains in one plot, the
study day (x-axis) is derived for consistency based the start date of
user's choice in the app (for example, `ADSL.RANDDT` or `ADSL.TRTSDT`):

- In `ADAE`, `ADEX`, and `ADCM`, it would be study day based on `ASTDT`
  and/or `AENDT` in reference to the start date

- In `ADRS` and `ADLB`, it would be study day based on `ADT` in
  reference to the start date

For every variable domain defined (i.e `ae_var`) please set its
corresponding analysis dataset (i.e `ae_dataset`)

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`grob`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_patient_profile(
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

Xuefeng Hou (houx14) <houx14@gene.com>

Tina Cho (chot) <tina.cho@roche.com>

Molly He (hey59) <hey59@gene.com>

Ting Qi (qit3) <qit3@gene.com>

## Examples

``` r
data <- within(teal_data(), {
  library(nestcolor)
  library(dplyr)
  ADSL <- teal.data::rADSL
  ADAE <- teal.data::rADAE %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
  ADCM <- teal.data::rADCM %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
  # The step below is to pre-process ADCM to legacy standard
  ADCM <- ADCM %>%
    select(-starts_with("ATC")) %>%
    unique()
  ADRS <- teal.data::rADRS %>% mutate(ADT = as.Date(ADTM))
  ADEX <- teal.data::rADEX %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
  ADLB <- teal.data::rADLB %>% mutate(ADT = as.Date(ADTM), LBSTRESN = as.numeric(LBSTRESC))
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = variables(
        choices = "USUBJID",
        selected = "USUBJID"
      ),
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      ae_dataname = "ADAE",
      rs_dataname = "ADRS",
      cm_dataname = "ADCM",
      lb_dataname = "ADLB",
      sl_start_date = variables(
        selected = "TRTSDTM",
        choices = c("TRTSDTM", "RANDDT")
      ),
      ex_var = variables(
        selected = "PARCAT2",
        choices = "PARCAT2"
      ),
      ae_var = variables(
        selected = "AEDECOD",
        choices = c("AEDECOD", "AESOC")
      ),
      ae_line_col_var = variables(
        selected = "AESER",
        choices = c("AESER", "AEREL")
      ),
      ae_line_col_opt = c("Y" = "red", "N" = "blue"),
      rs_var = variables(
        selected = "PARAMCD",
        choices = "PARAMCD"
      ),
      cm_var = variables(
        selected = "CMDECOD",
        choices = c("CMDECOD", "CMCAT")
      ),
      lb_var = variables(
        selected = "LBTESTCD",
        choices = c("LBTESTCD", "LBCAT")
      ),
      x_limit = "-28, 750",
      plot_height = c(1200, 400, 5000)
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
