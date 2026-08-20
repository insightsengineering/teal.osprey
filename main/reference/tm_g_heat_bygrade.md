# Teal module for the heatmap by grade

Display the heatmap by grade as a shiny module

## Usage

``` r
tm_g_heat_bygrade(
  label,
  sl_dataname,
  ex_dataname,
  ae_dataname,
  id_var = teal.picks::variables(choices = teal.picks::is_categorical(), selected = 1L),
  visit_var = teal.picks::variables(choices = dplyr::starts_with("AVISIT"), selected =
    1L),
  ongo_var = teal.picks::variables(choices = dplyr::starts_with("ongo"), selected = 1L),
  anno_var = teal.picks::variables(choices = teal.picks::is_categorical(min.len = 2),
    selected = 1L, multiple = TRUE),
  heat_var = teal.picks::variables(choices = dplyr::starts_with("AET0"), selected = 1L),
  cm_dataname = NULL,
  conmed_var = NULL,
  fontsize = c(5, 3, 7),
  plot_height = c(600L, 200L, 2000L),
  plot_width = NULL,
  transformators = list(),
  decorators = list()
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
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)\

- ae_dataname:

  (`character`) adverse events dataset name, needs to be available in
  the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)\
  specify to `NA` if no concomitant medications data is available

- id_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Unique subject ID variable.

- visit_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Analysis visit variable.

- ongo_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Study ongoing status variable. This
  variable is a derived logical variable. Usually it can be derived from
  `EOSSTT`.

- anno_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Annotation variable.

- heat_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Heatmap variable.

- cm_dataname:

  (`character`) concomitant medications dataset name,

- conmed_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object. `choices_selected()` is being deprecated as an argument type
  and will be removed in the future. Concomitant medications variable,
  specify to `NA` if no concomitant medications data is available

- fontsize:

  (`numeric(1)` or `numeric(3)`)\
  Defines initial possible range of font-size. `fontsize` is set for
  [`teal.widgets::optionalSliderInputValMinMax()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html)
  which controls font-size in the output plot.

- plot_height:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

- plot_width:

  (`numeric(3)`)\
  vector to indicate default value, minimum and maximum values.

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

\`data“ object is only used for checks

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`grob`, `gtable`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_heat_bygrade(
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

## Examples

``` r

data <- within(teal_data(), {
  library(dplyr)
  library(nestcolor)
  ADSL <- teal.data::rADSL %>% slice(1:30)
  ADEX <- teal.data::rADEX %>% filter(USUBJID %in% ADSL$USUBJID)
  ADAE <- teal.data::rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
  ADCM <- teal.data::rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
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

app <- init(
  data = data,
  modules = modules(
    tm_g_heat_bygrade(
      label = "Heatmap by grade",
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      ae_dataname = "ADAE",
      cm_dataname = "ADCM",
      id_var = variables(
        choices = is_categorical(min.len = 2),
        selected = 1L
      ),
      visit_var = variables(
        choices = dplyr::starts_with("AVISIT"),
        selected = 1L
      ),
      ongo_var = variables(
        choices = dplyr::starts_with("ongo"),
        selected = 1L
      ),
      anno_var = variables(
        choices = is_categorical(min.len = 2),
        selected = c("SEX", "COUNTRY"),
        multiple = TRUE
      ),
      heat_var = variables(
        choices = dplyr::starts_with("AETO"),
        selected = 1L
      ),
      conmed_var = variables(
        choices = dplyr::starts_with("CMDECOD"),
        selected = 1L
      )
    )
  )
)
#> Initializing tm_g_heat_bygrade
#> Warning: variables(choices = is_categorical(min.len = 2), selected = c("SEX", "COUNTRY"), multiple = TRUE)
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
