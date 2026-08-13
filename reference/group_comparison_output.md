# Construct custom group comparison output

Construct custom group comparison output

## Usage

``` r
group_comparison_output(
  estimate,
  conf_low,
  conf_high,
  p_value,
  statistic_method,
  statistic = NA_real_,
  df = NA_real_,
  test = NA_character_,
  std_error = NA_real_,
  std_error_scale = NA_character_
)
```

## Arguments

- estimate, conf_low, conf_high, p_value:

  Numeric scalar results.

- statistic_method:

  Non-empty method identifier.

- statistic, df:

  Optional test statistic and degrees of freedom.

- test:

  Optional test name; omit it when no separate test is returned.

- std_error:

  Optional standard error on `std_error_scale`.

- std_error_scale:

  Scale on which the standard error is defined.

## Value

A `group_comparison_output`.
