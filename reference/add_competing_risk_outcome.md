# Register a composite competing-risk outcome

Register a composite competing-risk outcome

## Usage

``` r
add_competing_risk_outcome(.data, name, time, event, censor_value, time_unit)
```

## Arguments

- .data:

  A `bq_data` object.

- name:

  Bare name for the composite outcome.

- time:

  Exactly one follow-up time column selected with tidyselect.

- event:

  Exactly one event indicator column selected with tidyselect.

- censor_value:

  Scalar value identifying censoring. Every other observed event value
  is treated as an explicit competing event cause.

- time_unit:

  Non-empty unit string.

## Value

Updated `bq_data`.
