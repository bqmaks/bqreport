# Build a regression results table

Build a regression results table

## Usage

``` r
tbl_regression(
  x,
  locale = "en",
  digits = 2L,
  p_value_digits = 3L,
  missing = "NA"
)
```

## Arguments

- x:

  An `analysis_result`.

- locale:

  Output locale, `en` or `ru`.

- digits:

  Digits for estimates and confidence limits.

- p_value_digits:

  Digits for p values.

- missing:

  Missing-value text.

## Value

A backend-independent `bq_table`.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  response = c(0, 0, 1, 0, 1, 1, 1, 0),
  treatment = factor(rep(c("Control", "Treatment"), each = 4))
)) |>
  set_outcome(response, type = "binary", event = 1) |>
  set_predictor(treatment, type = "binary", reference = "Control")
result <- plan_analysis(data, response, treatment) |>
  validate_plan(data) |>
  run_analysis(data)
tbl_regression(result)
#> # A tibble: 2 × 8
#>   outcome  predictor term        level     estimate conf_int      p_value method
#>   <chr>    <chr>     <chr>       <chr>     <chr>    <chr>         <chr>   <chr> 
#> 1 response treatment (Intercept) NA        0.33     0.03 – 3.20   0.341   logis…
#> 2 response treatment treatment   Treatment 9.00     0.37 – 220.93 0.178   logis…
```
