# Build a correlation results table

Build a correlation results table

## Usage

``` r
tbl_correlation(
  x,
  locale = "en",
  digits = 2L,
  p_value_digits = 3L,
  missing = "NA"
)
```

## Arguments

- x:

  An `analysis_result`.

- locale:

  Output locale, `en` or `ru`.

- digits:

  Digits for estimates and confidence limits.

- p_value_digits:

  Digits for p values.

- missing:

  Missing-value text.

## Value

A backend-independent `bq_table`.
