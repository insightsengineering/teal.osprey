# takes input_string, splits by "," and returns a numeric vector with NAs where the split-strings are not numeric. e.g. as_numeric_from_comma_separated_string("4 ,hello,5,, 3") is c(4, NA, 5, NA, 3). If input argument is NULL or just whitespace then NULL is returned

takes input_string, splits by "," and returns a numeric vector with NAs
where the split-strings are not numeric. e.g.
as_numeric_from_comma_separated_string("4 ,hello,5,, 3") is c(4, NA, 5,
NA, 3). If input argument is NULL or just whitespace then NULL is
returned

## Usage

``` r
as_numeric_from_comma_sep_str(input_string)
```

## Arguments

- input_string:

  string to be split into numeric vector
