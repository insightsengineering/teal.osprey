# Helper server function to decorate plot output

This is used in
[`tm_g_ae_oview()`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_ae_oview.md)
and
[`tm_g_events_term_id()`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_events_term_id.md).

## Usage

``` r
srv_g_decorate(
  id,
  plot_id = "out",
  plt = reactive(NULL),
  plot_height,
  plot_width
)
```

## Arguments

- id:

  (`character`) id of the module

- plot_id:

  (`character`) id for plot output

- plt:

  (`reactive`) a reactive object of graph object

- plot_height:

  (`numeric`) optional vector of length three with `c(value, min, max)`.
  Specifies the height of the main plot.

- plot_width:

  (`numeric`) optional vector of length three with `c(value, min, max)`.
  Specifies the width of the main plot and renders a slider on the plot
  to interactively adjust the plot width.
