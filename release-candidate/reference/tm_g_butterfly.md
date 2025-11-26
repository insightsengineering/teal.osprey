# Butterfly plot Teal Module

Display butterfly plot as a shiny module

## Usage

``` r
tm_g_butterfly(
  label,
  dataname,
  filter_var = NULL,
  right_var,
  left_var,
  category_var,
  color_by_var,
  count_by_var,
  facet_var = NULL,
  sort_by_var = teal.transform::choices_selected(selected = "count", choices = c("count",
    "alphabetical")),
  legend_on = TRUE,
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

- filter_var:

  (`choices_selected`) variable name of data filter, please see details
  regarding expected values, default is`NULL`.`choices` vector with
  `filter_var` choices, default is `NULL`

- right_var:

  (`choices_selected`) dichotomization variable for right side

- left_var:

  (`choices_selected`) dichotomization variable for left side

- category_var:

  (`choices_selected`) category (y axis) variable

- color_by_var:

  (`choices_selected`) variable defines color blocks within each bar

- count_by_var:

  (`choices_selected`) variable defines how x axis is calculated

- facet_var:

  (`choices_selected`) variable for row facets

- sort_by_var:

  (`choices_selected`) argument for order of class and term elements in
  table, default here is "count"

- legend_on:

  (`boolean`) value for whether legend is displayed

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

`filter_var` option is designed to work in conjunction with filtering
function provided by `teal` (encoding panel on the right hand side of
the shiny app). It can be used as quick access to predefined subsets of
the domain datasets (not subject-level dataset) to be used for analysis,
denoted by an value of "Y". Each variable within the
`filter_var_choices` is expected to contain values of either "Y" or "N".
If multiple variables are selected as `filter_var`, only observations
with "Y" value in each and every selected variables will be used for
subsequent analysis. Flag variables (from `ADaM` datasets) can be used
directly as filter.

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
  eval_code("set.seed(23) # @linksto ADSL") %>%
  within({
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL
    ADAE <- rADAE
    ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
    ADAE <- mutate(
      ADAE,
      flag1 = ifelse(AETOXGR == 1, 1, 0),
      flag2 = ifelse(AETOXGR == 2, 1, 0),
      flag3 = ifelse(AETOXGR == 3, 1, 0),
      flag1_filt = rep("Y", n())
    )
  })

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = choices_selected(
        selected = "SEX",
        choices = c("SEX", "ARM", "RACE")
      ),
      left_var = choices_selected(
        selected = "RACE",
        choices = c("SEX", "ARM", "RACE")
      ),
      category_var = choices_selected(
        selected = "AEBODSYS",
        choices = c("AEDECOD", "AEBODSYS")
      ),
      color_by_var = choices_selected(
        selected = "AETOXGR",
        choices = c("AETOXGR", "None")
      ),
      count_by_var = choices_selected(
        selected = "# of patients",
        choices = c("# of patients", "# of AEs")
      ),
      facet_var = choices_selected(
        selected = NULL,
        choices = c("RACE", "SEX", "ARM")
      ),
      sort_by_var = choices_selected(
        selected = "count",
        choices = c("count", "alphabetical")
      ),
      legend_on = TRUE,
      plot_height = c(600, 200, 2000)
    )
  )
)
#> Initializing tm_g_butterfly
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
