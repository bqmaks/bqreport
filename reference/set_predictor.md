# Define predictor variables

Define predictor variables

## Usage

``` r
set_predictor(.data, .cols, type = NULL, reference = NULL)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- type:

  An optional analytical variable type. If omitted, the current inferred
  or configured type is retained.

- reference:

  An optional reference value for binary, ordinal, or nominal
  predictors.

## Value

`.data` with updated variable metadata.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  treatment = factor(c("Control", "Treatment", "Control", "Treatment")),
  age = c(44, 57, 51, 63)
)) |>
  set_predictor(treatment, type = "binary", reference = "Control") |>
  set_predictor(age, type = "continuous")
variables(data)
#> # A tibble: 2 × 22
#>   var_id       name  label unit  digits descriptive_templates colors role  type 
#>   <chr>        <chr> <chr> <chr>  <int> <list>                <list> <lis> <chr>
#> 1 var_treatme… trea… NA    NA        NA <NULL>                <NULL> <chr> bina…
#> 2 var_age      age   NA    NA        NA <NULL>                <NULL> <chr> cont…
#> # ℹ 13 more variables: storage_type <chr>, distribution <chr>,
#> #   reference <list>, coding <chr>, weight_type <chr>, cluster_type <chr>,
#> #   event_value <list>, transformation <list>, model_term <list>,
#> #   missing_policy <chr>, source <chr>, locked <lgl>, status <chr>
```
