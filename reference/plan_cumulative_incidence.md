# Compile an Aalen–Johansen cumulative-incidence plan

Compile an Aalen–Johansen cumulative-incidence plan

## Usage

``` r
plan_cumulative_incidence(
  .data,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95
)
```

## Arguments

- .data:

  A `bq_data` object.

- outcomes:

  Composite competing-risk outcomes selected with tidyselect.

- groups:

  Optional single grouping column.

- times:

  Optional positive evaluation times; omitted for the full curve.

- confidence_level:

  Confidence level.

## Value

An `analysis_plan` tibble.
