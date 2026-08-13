# Compile a descriptive analysis plan

`plan_descriptives()` creates one inspectable task per selected
variable. Results may be computed for the complete population, levels of
one grouping variable, or both. The grouping label used by a report is
deliberately kept out of the numerical result.

## Usage

``` r
plan_descriptives(
  .data,
  variables = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  overall = TRUE,
  confidence_level = 0.95,
  functions = list(),
  comparisons = FALSE,
  contrasts = NULL,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object.

- variables:

  Variables selected with tidyselect.

- groups:

  Optional single grouping variable selected with tidyselect.

- overall:

  Whether to include statistics for the complete population.

- confidence_level:

  Confidence level reserved for model-based providers.

- functions:

  A list of explicit `descriptive_function` providers.

- comparisons:

  Whether to estimate an effect between two groups.

- contrasts:

  Target group comparisons. Supports
  [`against_reference()`](https://bqmaks.github.io/bqreport/reference/against_reference.md),
  [`all_pairwise()`](https://bqmaks.github.io/bqreport/reference/all_pairwise.md),
  and
  [`consecutive_comparisons()`](https://bqmaks.github.io/bqreport/reference/consecutive_comparisons.md).

- adjust:

  Multiplicity adjustment accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

An `analysis_plan` tibble.
