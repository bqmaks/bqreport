# Define outcome variables

Define outcome variables

## Usage

``` r
set_outcome(.data, .cols, type = NULL, event = NULL, reference = NULL)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- type:

  An optional analytical variable type. If omitted, the current inferred
  or configured type is retained.

- event:

  An optional event value for binary outcomes.

- reference:

  Optional reference category for ordinal or nominal outcomes.

## Value

`.data` with updated variable metadata.
