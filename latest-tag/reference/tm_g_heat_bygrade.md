# Teal module for the heatmap by grade

Display the heatmap by grade as a shiny module

## Usage

``` r
tm_g_heat_bygrade(
  label,
  sl_dataname,
  ex_dataname,
  ae_dataname,
  cm_dataname = NA,
  id_var,
  visit_var,
  ongo_var,
  anno_var,
  heat_var,
  conmed_var = NULL,
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

- sl_dataname:

  (`character`) subject level dataset name, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)

- ex_dataname:

  (`character`) exposures dataset name, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)  

- ae_dataname:

  (`character`) adverse events dataset name, needs to be available in
  the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)  

- cm_dataname:

  (`character`) concomitant medications dataset name, needs to be
  available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)  
  specify to `NA` if no concomitant medications data is available

- id_var:

  (`choices_seleced`) unique subject ID variable

- visit_var:

  (`choices_seleced`) analysis visit variable

- ongo_var:

  (`choices_seleced`) study ongoing status variable. This variable is a
  derived logical variable. Usually it can be derived from `EOSSTT`.

- anno_var:

  (`choices_seleced`) annotation variable

- heat_var:

  (`choices_seleced`) heatmap variable

- conmed_var:

  (`choices_seleced`) concomitant medications variable, specify to `NA`
  if no concomitant medications data is available

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
    library(nestcolor)
    ADSL <- rADSL %>% slice(1:30)
    ADEX <- rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
    ADAE <- rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
    ADCM <- rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
    # This preprocess is only to force legacy standard on ADCM
    ADCM <- ADCM %>%
      select(-starts_with("ATC")) %>%
      unique()
    # function to derive AVISIT from ADEX
    .add_visit <- function(data_need_visit) {
      visit_dates <- ADEX %>%
        filter(PARAMCD == "DOSE") %>%
        distinct(USUBJID, AVISIT, ASTDTM) %>%
        group_by(USUBJID) %>%
        arrange(ASTDTM) %>%
        mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
        rename(this_vis = ASTDTM)
      data_visit <- data_need_visit %>%
        select(USUBJID, ASTDTM) %>%
        left_join(visit_dates, by = "USUBJID") %>%
        filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
        left_join(data_need_visit) %>%
        distinct()
      return(data_visit)
    }
    # derive AVISIT for ADAE and ADCM
    ADAE <- .add_visit(ADAE)
    ADCM <- .add_visit(ADCM)
    # derive ongoing status variable for ADEX
    ADEX <- ADEX %>%
      filter(PARCAT1 == "INDIVIDUAL") %>%
      mutate(ongo_status = (EOSSTT == "ONGOING"))
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

ADCM <- data[["ADCM"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_heat_bygrade(
      label = "Heatmap by grade",
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      ae_dataname = "ADAE",
      cm_dataname = "ADCM",
      id_var = choices_selected(
        selected = "USUBJID",
        choices = c("USUBJID", "SUBJID")
      ),
      visit_var = choices_selected(
        selected = "AVISIT",
        choices = c("AVISIT")
      ),
      ongo_var = choices_selected(
        selected = "ongo_status",
        choices = c("ongo_status")
      ),
      anno_var = choices_selected(
        selected = c("SEX", "COUNTRY"),
        choices = c("SEX", "COUNTRY", "USUBJID")
      ),
      heat_var = choices_selected(
        selected = "AETOXGR",
        choices = c("AETOXGR")
      ),
      conmed_var = choices_selected(
        selected = "CMDECOD",
        choices = c("CMDECOD")
      ),
      plot_height = c(600, 200, 2000)
    )
  )
)
#> Initializing tm_g_heat_bygrade
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
