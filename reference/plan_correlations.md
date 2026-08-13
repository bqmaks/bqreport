# Compile a correlation analysis plan

Compile a correlation analysis plan

## Usage

``` r
plan_correlations(
  .data,
  variables = where_continuous(),
  with = NULL,
  adjust_for = tidyselect::any_of(character()),
  strata = tidyselect::any_of(character()),
  weights = tidyselect::any_of(character()),
  id = tidyselect::any_of(character()),
  interaction_test = FALSE,
  comparator = NULL,
  method = pearson_correlation(),
  missing = c("pairwise", "complete"),
  confidence_level = 0.95,
  adjust = "none"
)
```

## Arguments

- .data:

  A `bq_data` object.

- variables:

  Numeric variables selected with tidyselect.

- with:

  Optional second variable set. If omitted, unique pairs within
  `variables` are compiled.

- adjust_for:

  Optional numeric covariates for partial correlation.

- strata:

  Optional variables defining independent correlation strata.

- weights:

  Optional numeric analysis-weight column.

- id:

  Optional subject identifier for repeated-measures methods.

- interaction_test:

  Whether to test equality of Pearson correlations across strata and
  compute pairwise Fisher z contrasts.

- comparator:

  Optional comparator used when `interaction_test = TRUE`.

- method:

  A correlation method specification.

- missing:

  Pairwise or common complete-case analysis.

- confidence_level:

  Confidence level.

- adjust:

  Multiplicity adjustment accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

An `analysis_plan` with one row per unique variable pair.
