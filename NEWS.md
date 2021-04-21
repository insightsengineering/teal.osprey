# teal.osprey 0.1.9
* Added new `teal` module `tm_g_ae_sub` for an AE by subgroups plot.
* Improved reactivity dependencies across modules.

# teal.osprey 0.1.8

* Added validation message if the dataset does not contain subjects from both the control and treatment arms in `tm_g_events_term_id`.
* Improved validation message if subject has been filtered out of dataset when used with `tm_g_patient_profile`.
* Fixed modules due to changes to `plot_with_settings` in latest `teal.devel` release.

# teal.osprey 0.1.7

### Enhancements
* Added additional validation for horizontal and vertical reference lines in `tm_g_spider_plot`.

### Bug Fixes
* Fixed `tm_g_patient_profile` when adverse event line color variable is not selected.
* Fixed the issue of unknown date format and established minimum number of observation threshold in `tm_g_patient_profile`.

### Miscellaneous
* Retired the following `teal` modules due to duplication in `teal.modules.clinical`:
  - Teal module for AE overview summary table - can be replaced with `teal.modules.clinical::tm_t_events_summary`.
  - Teal module for AE summary table by preferred terms - can be replaced with `teal.modules.clinical::tm_t_events`.
  - Teal module for AE summary table by highest NCI-CTCAE grade - can be replaced with `teal.modules.clinical::tm_t_events_by_grade`.
  - Teal module for Disposition table - can be replaced with `teal.modules.clinical::tm_t_summary`.
* Moved `code` argument to `cdisc_dataset` (from `cdisc_data`) in examples.

# teal.osprey 0.1.6

* All graph modules now accept a `plot_width` argument which specifies the plot width and renders a slider to adjust the width interactively in the module.
* Replaced `plot_with_height` with `plot_with_settings` module throughout package.
* Show R Code now produces code generating only the datasets needed for the current module, not all datasets included in the data argument of `teal::init`.
* Evaluate chunks with `chunks_safe_eval` rather than `chunks_eval`.

# teal.osprey 0.1.5

*  Added new `teal` modules:
  - Teal module for events by term summary plot.
  - Teal module for AE overview summary plot.
  - Teal module for patient profile plot

# teal.osprey 0.1.4

* Minor changes in tests.

# teal.osprey 0.1.3
* Technical release with updated dependencies.
 
# teal.osprey 0.1.2
* New waterfall module.
* Refactored due to recent changes in `utils.nest`.

# teal.osprey 0.1.1
* Refactoring package according to the NEST standards:
  - Included test.nest tests.
  - Fixed outdated modules according to current teal functionality.
  - Fixed documentation examples and example_app.R.
  - Simplified dependencies.

# teal.osprey 0.1.0

* First versioned release of teal.osprey package, which include the following seven new functions to create `teal` modules for analysis functions in `osprey` v0.1.0:
  - Teal module for AE overview summary table.
  - Teal module for AE summary table by preferred terms.
  - Teal module for AE summary table by highest NCI-CTCAE grade.
  - Teal module for Disposition table.
  - Teal module for AE butterfly plot.
  - Teal module for Swimlane plot.
  - Teal module for Spider plot.
