# Validate an analysis plan against data

Preflight resolves variables by stable identifiers, refreshes formulas
and counts complete observations while treating labelled special missing
values as missing only in the internal analysis view.

## Usage

``` r
validate_plan(plan, data)
```

## Arguments

- plan:

  An `analysis_plan`.

- data:

  A `bq_data` object.

## Value

A validated `analysis_plan`.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  bmi = c(21.4, 27.9, 24.2, 30.1, 26.6, 23.0),
  treatment = factor(rep(c("Control", "Treatment"), 3))
)) |>
  set_outcome(bmi, type = "continuous") |>
  set_predictor(treatment, type = "binary", reference = "Control")
plan <- plan_analysis(data, bmi, treatment) |> validate_plan(data)
plan[, c("outcome", "predictor", "method", "status", "n_analyzed")]
#> # A tibble: 1 × 5
#>   outcome predictor method       status n_analyzed
#>   <chr>   <chr>     <chr>        <chr>       <int>
#> 1 bmi     treatment linear_model ready           6
```
