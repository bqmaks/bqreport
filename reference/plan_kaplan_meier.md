# Compile a Kaplan–Meier analysis plan

Compile a Kaplan–Meier analysis plan

## Usage

``` r
plan_kaplan_meier(
  .data,
  outcomes = tidyselect::everything(),
  groups = tidyselect::any_of(character()),
  times = NULL,
  confidence_level = 0.95,
  quantiles = NULL,
  rmst_tau = NULL,
  estimates = "survival",
  comparisons = NULL,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object.

- outcomes:

  Composite survival outcomes selected with tidyselect.

- groups:

  Optional single grouping column.

- times:

  Optional positive evaluation times. If omitted, the full Kaplan–Meier
  step curve is returned.

- confidence_level:

  Confidence level.

- quantiles:

  Optional event-time distribution probabilities strictly between zero
  and one.

- rmst_tau:

  Optional positive restriction time for restricted mean survival time.
  RMST is not computed unless this estimand is explicit.

- estimates:

  One or both of `survival` and `cumulative_risk`. Cumulative risk is
  the single-event complement `1 - S(t)`; it is neither cumulative
  hazard nor a competing-risks cumulative incidence function.

- comparisons:

  Optional pairwise group-comparison specification. It is applied to
  pairwise log-rank tests and, when `rmst_tau` is supplied, to
  differences in restricted mean survival time.

- adjust:

  Multiplicity adjustment accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

An `analysis_plan` tibble.
