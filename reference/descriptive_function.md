# Construct a custom descriptive statistic provider

A descriptive function receives a read-only context for one variable in
one population and returns one row per declared field. Providers are
explicit: they are compiled into the analysis plan and never discovered
from template names at runtime.

## Usage

``` r
descriptive_function(
  id,
  fields,
  compute,
  types = c("continuous", "count"),
  required_packages = character(),
  source = c("custom", "diagnostic", "model")
)
```

## Arguments

- id:

  Stable provider identifier.

- fields:

  Unique statistic names supplied by the provider.

- compute:

  Function accepting a `descriptive_context` and returning a data frame
  with `statistic`, `value`, and `statistic_method` columns.

- types:

  Supported analytical variable types.

- required_packages:

  Packages required to execute the provider.

- source:

  Result source: `custom`, `diagnostic`, or `model`.

## Value

A `descriptive_function` specification.
