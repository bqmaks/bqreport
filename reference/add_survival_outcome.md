# Register a composite survival outcome

A survival outcome references separate follow-up time and event columns
by stable variable identifiers. The outcome name is an analytical
identifier; it does not create or mutate a data column.

## Usage

``` r
add_survival_outcome(.data, name, time, event, event_value, time_unit)
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

- event_value:

  Scalar value identifying an event.

- time_unit:

  Non-empty unit string.

## Value

Updated `bq_data`.
