# Construct custom correlation output

Construct custom correlation output

## Usage

``` r
correlation_output(
  estimate,
  std_error,
  std_error_scale,
  conf_low,
  conf_high,
  statistic,
  df,
  p_value
)
```

## Arguments

- estimate, std_error, conf_low, conf_high, statistic, df, p_value:

  Numeric scalars.

- std_error_scale:

  Scale on which `std_error` is defined.

## Value

A validated `correlation_method_output`.
