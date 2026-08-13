# Define predictor variables

Define predictor variables

## Usage

``` r
set_predictor(.data, .cols, type = NULL, reference = NULL)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- type:

  An optional analytical variable type. If omitted, the current inferred
  or configured type is retained.

- reference:

  An optional reference value for binary, ordinal, or nominal
  predictors.

## Value

`.data` with updated variable metadata.
