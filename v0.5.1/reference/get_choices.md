# Get Choices

This function returns choices based on the class of the input. If the
input is of class `delayed_data`, it returns the `subset` of the input.
If `subset` is NULL and the input contains `var_label` and
`var_choices`, it throws an error prompting to resolve delayed inputs.
Otherwise, it returns the input as is.

## Usage

``` r
get_choices(choices)
```

## Arguments

- choices:

  An object that contains choices.

## Value

A vector of choices.
