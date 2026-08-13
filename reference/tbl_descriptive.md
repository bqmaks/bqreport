# Build a backend-independent descriptive table

`tbl_descriptive()` applies registered display templates to numerical
descriptive results. It does not modify or round values stored in the
`analysis_result`; formatting exists only in the returned table model.

## Usage

``` r
tbl_descriptive(
  x,
  overall_label = NULL,
  missing = "NA",
  digits = 2L,
  percent_digits = 1L,
  p_value_digits = 3L,
  locale = "en"
)
```

## Arguments

- x:

  An `analysis_result` containing descriptive tasks.

- overall_label:

  Display label for the overall population.

- missing:

  Text used for unavailable statistics.

- digits:

  Default number of decimal places when variable metadata do not provide
  `digits`.

- percent_digits:

  Decimal places for `{p}`, which is displayed as a percentage.

- p_value_digits:

  Decimal places for placeholders ending in `.p.value` or named
  `p.value`.

- locale:

  Output locale, `en` or `ru`.

## Value

A `tbl_descriptive` and `bq_table` object.
