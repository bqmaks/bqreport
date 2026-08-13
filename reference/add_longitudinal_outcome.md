# Register a repeated longitudinal outcome

Register a repeated longitudinal outcome

## Usage

``` r
add_longitudinal_outcome(
  .data,
  name,
  values,
  time = NULL,
  baseline = NULL,
  time_scale = NULL,
  type = "continuous",
  event_value = NULL
)
```

## Arguments

- .data:

  A `bq_data` object with a longitudinal design.

- name:

  Bare analytical outcome name.

- values:

  Source columns in explicit time order for wide data. For long data,
  select the single outcome column.

- time:

  Explicit time values corresponding to `values` for wide data.

- baseline:

  Optional baseline value; defaults to the design baseline.

- time_scale:

  Optional outcome-specific time scale.

- type:

  Analytical outcome type.

- event_value:

  Explicit event value for a binary repeated outcome.

## Value

Updated `bq_data`.
