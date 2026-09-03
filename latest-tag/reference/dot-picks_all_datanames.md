# Create a reactive that sets plot dimensions on a `teal_card`

This is a convenience function that creates a reactive expression that
automatically sets the `dev.width` and `dev.height` attributes on the
last chunk outputs of a `teal_card` based on plot dimensions from a plot
widget.

## Usage

``` r
.picks_all_datanames(pick_slots)
```

## Arguments

- pick_slots:

  (`list`) named list of `picks` objects (NULL entries ignored).

## Value

A reactive expression that returns the `teal_card` with updated
dimensions

Collect unique datanames from a list of picks objects (internal).
