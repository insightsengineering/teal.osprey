# Helper UI function to decorate plot output UI

This is used in
[`tm_g_ae_oview()`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_ae_oview.md)
and
[`tm_g_events_term_id()`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_events_term_id.md).

## Usage

``` r
ui_g_decorate(
  id,
  titles = "Titles",
  footnotes = "footnotes",
  fontsize = c(5, 4, 11)
)
```

## Arguments

- id:

  (`character`) id of this module. set to `NULL` if you want to make it
  identical to the module who called it.

- titles:

  (`character`) default titles

- footnotes:

  (`character`) default footnotes

- fontsize:

  (`numeric(1)` or `numeric(3)`)  
  Defines initial possible range of font-size. `fontsize` is set for
  [`teal.widgets::optionalSliderInputValMinMax()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html)
  which controls font-size in the output plot.
