# Register a longitudinal study design

Register a longitudinal study design

## Usage

``` r
set_longitudinal_design(
  .data,
  id,
  time = tidyselect::any_of(character()),
  group = tidyselect::any_of(character()),
  layout = c("long", "wide"),
  baseline = NULL,
  time_scale = c("categorical", "continuous")
)
```

## Arguments

- .data:

  A `bq_data` object.

- id:

  Exactly one subject identifier column.

- time:

  Exactly one observation-time column for long data.

- group:

  Optional treatment or grouping column.

- layout:

  Input layout, long or wide.

- baseline:

  Optional baseline time value.

- time_scale:

  Whether time is categorical or continuous.

## Value

Updated `bq_data`.
