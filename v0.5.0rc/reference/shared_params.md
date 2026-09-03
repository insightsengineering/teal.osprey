# Shared Parameters

Contains arguments that are shared between multiple functions in the
package to avoid repetition using `inheritParams`.

## Arguments

- plot_height:

  (`numeric`) optional vector of length three with `c(value, min, max)`.
  Specifies the height of the main plot.

- plot_width:

  (`numeric`) optional vector of length three with `c(value, min, max)`.
  Specifies the width of the main plot and renders a slider on the plot
  to interactively adjust the plot width.

- label:

  (`character`) module label in the teal app. Please note that this
  module is developed based on `ADaM` data structure and `ADaM`
  variables.
