# Add bootstrap intervals and permutation inference to a correlation method

Bootstrap replicates resample observations jointly with their analysis
weights. For subject-identified methods whole subjects are resampled
(cluster bootstrap) and permutation replicates permute outcome values
within subjects only, preserving the within-subject estimand.

## Usage

``` r
resampled_correlation(
  method = pearson_correlation(),
  bootstrap = 999L,
  permutations = 999L,
  seed
)
```

## Arguments

- method:

  A correlation method specification.

- bootstrap:

  Number of bootstrap replicates, or zero.

- permutations:

  Number of permutation replicates, or zero.

- seed:

  Required integer seed used without changing global RNG state.

## Value

A resampling `correlation_method_spec`.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  x = c(1, 4, 2, 8, 5, 9, 3, 7, 6, 10),
  y = c(2, 1, 5, 4, 8, 7, 3, 9, 6, 10)
))
method <- resampled_correlation(
  pearson_correlation(), bootstrap = 199, permutations = 199, seed = 42
)
result <- plan_correlations(data, x, with = y, method = method) |>
  validate_plan(data) |>
  run_analysis(data)
correlations(result)[, c("estimate", "conf_low", "conf_high", "p_value")]
#> # A tibble: 1 × 4
#>   estimate conf_low conf_high p_value
#>      <dbl>    <dbl>     <dbl>   <dbl>
#> 1    0.685    0.250     0.913    0.04
```
