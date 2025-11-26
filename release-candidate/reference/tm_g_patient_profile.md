# Patient Profile plot teal module

Display patient profile plot as a shiny module

## Usage

``` r
tm_g_patient_profile(
  label = "Patient Profile Plot",
  patient_id,
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
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- patient_id:

  (`choices_seleced`) unique subject ID variable

- sl_dataname:

  (`character`) subject level dataset name, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- ex_dataname, ae_dataname, rs_dataname, cm_dataname, lb_dataname:

  (`character(1)`) names of exposure, adverse events, response,
  concomitant medications, and labs datasets, respectively; must be
  available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)  
  set to NA (default) to omit from analysis

- sl_start_date:

  `choices_selected` study start date variable, usually set to treatment
  start date or randomization date

- ex_var:

  `choices_selected` exposure variable to plot as each line  
  leave unspecified or set to `NULL` if exposure data is not available

- ae_var:

  `choices_selected` adverse event variable to plot as each line  
  leave unspecified or set to `NULL` if adverse events data is not
  available

- ae_line_col_var:

  `choices_selected` variable for coloring `AE` lines  
  leave unspecified or set to `NULL` if adverse events data is not
  available

- ae_line_col_opt:

  aesthetic values to map color values (named vector to map color values
  to each name). If not `NULL`, please make sure this contains all
  possible values for `ae_line_col_var` values.  
  leave unspecified or set to `NULL` if adverse events data is not
  available

- rs_var:

  `choices_selected` response variable to plot as each line  
  leave unspecified or set to `NULL` if response data is not available

- cm_var:

  `choices_selected` concomitant medication variable to plot as each
  line  
  leave unspecified or set to `NULL` if concomitant medications data is
  not available

- lb_var:

  `choices_selected` lab variable to plot as each line  
  leave unspecified or set to `NULL` if labs data is not available

- x_limit:

  a single `character` string with two numbers separated by a comma
  indicating the x-axis limit, default is "-28, 365"

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

## Details

As the patient profile module plots different domains in one plot, the
study day (x-axis) is derived for consistency based the start date of
user's choice in the app (for example, `ADSL.RANDDT` or `ADSL.TRTSDT`):

- In `ADAE`, `ADEX`, and `ADCM`, it would be study day based on `ASTDT`
  and/or `AENDT` in reference to the start date

- In `ADRS` and `ADLB`, it would be study day based on `ADT` in
  reference to the start date

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
data <- teal_data() %>%
  within({
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL
    ADAE <- rADAE %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADCM <- rADCM %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    # The step below is to pre-process ADCM to legacy standard
    ADCM <- ADCM %>%
      select(-starts_with("ATC")) %>%
      unique()
    ADRS <- rADRS %>% mutate(ADT = as.Date(ADTM))
    ADEX <- rADEX %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADLB <- rADLB %>% mutate(ADT = as.Date(ADTM), LBSTRESN = as.numeric(LBSTRESC))
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = choices_selected(
        choices = unique(ADSL$USUBJID),
        selected = unique(ADSL$USUBJID)[1]
      ),
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      ae_dataname = "ADAE",
      rs_dataname = "ADRS",
      cm_dataname = "ADCM",
      lb_dataname = "ADLB",
      sl_start_date = choices_selected(
        selected = "TRTSDTM",
        choices = c("TRTSDTM", "RANDDT")
      ),
      ex_var = choices_selected(
        selected = "PARCAT2",
        choices = "PARCAT2"
      ),
      ae_var = choices_selected(
        selected = "AEDECOD",
        choices = c("AEDECOD", "AESOC")
      ),
      ae_line_col_var = choices_selected(
        selected = "AESER",
        choices = c("AESER", "AEREL")
      ),
      ae_line_col_opt = c("Y" = "red", "N" = "blue"),
      rs_var = choices_selected(
        selected = "PARAMCD",
        choices = "PARAMCD"
      ),
      cm_var = choices_selected(
        selected = "CMDECOD",
        choices = c("CMDECOD", "CMCAT")
      ),
      lb_var = choices_selected(
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
