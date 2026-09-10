# Standard Arguments

The documentation to this function lists all the arguments in teal
modules that are used repeatedly to express an analysis.

## Arguments

- label:

  (`character(1)`)\
  menu item label of the module in the teal app.

- dataname:

  (`character(1)`)\
  analysis data used in the teal module, needs to be available in the
  list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).

- parentname:

  (`character(1)`)\
  analysis data used for several variables in the teal module, needs to
  be available in the list passed to the `data` argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  The default is `"ADSL"`

- arm_var:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object.\
  `choices_selected` is being deprecated as an argument type and will be
  removed in the future. Object with all available choices and the
  pre-selected option for variable names that can be used as `arm_var`.
  Column `arm_var` in the `dataname` has to be a factor.

- paramcd:

  Either a
  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html))
  object or a
  ([`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object.\
  `choices_selected` is being deprecated as an argument type and will be
  removed in the future. Variable value designating the studied
  parameter.

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

  (`list` of `teal_transform_module`) optional, input data transforms
  applied after filtering (UI in the filter sidebar under **Transform
  Data**). See
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of `teal_transform_module`)
  optional, decorators for the module `plot` output.

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
