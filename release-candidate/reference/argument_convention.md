# Standard Arguments

The documentation to this function lists all the arguments in teal
modules that are used repeatedly to express an analysis.

## Arguments

- label:

  (`character(1)`)  
  menu item label of the module in the teal app.

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

- paramcd:

  (`character(1)` or `choices_selected`)  
  variable value designating the studied parameter. See
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  for details.

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

## Value

the
[`teal::module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
object.

## Details

Although this function just returns `NULL` it has two uses, for the teal
module users it provides a documentation of arguments that are commonly
and consistently used in the framework. For the developer it adds a
single reference point to import the `roxygen` argument description
with: `@inheritParams argument_convention`
