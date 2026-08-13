# Construct built-in descriptive group comparisons

These specifications declare the estimand and scale before execution.
They support exactly two groups in the initial descriptive comparison
slice.

## Usage

``` r
mean_difference(ci_method = "welch_t")

standardized_mean_difference(ci_method = "large_sample")

risk_difference(ci_method = "wald")

risk_ratio(ci_method = "log_wald")

odds_ratio(ci_method = "log_wald")
```

## Arguments

- ci_method:

  Confidence interval implementation identifier.

## Value

A `group_comparison_spec`.
