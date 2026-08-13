# Define outcome variables

Define outcome variables

## Usage

``` r
set_outcome(.data, .cols, type = NULL, event = NULL, reference = NULL)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- type:

  An optional analytical variable type. If omitted, the current inferred
  or configured type is retained.

- event:

  An optional event value for binary outcomes.

- reference:

  Optional reference category for ordinal or nominal outcomes.

## Value

`.data` with updated variable metadata.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  response = c(0, 1, 1, 0),
  bmi = c(21.4, 27.9, 24.2, 30.1)
)) |>
  set_outcome(response, type = "binary", event = 1) |>
  set_outcome(bmi, type = "continuous")
variables(data)
#> # A tibble: 2 × 22
#>   var_id       name  label unit  digits descriptive_templates colors role  type 
#>   <chr>        <chr> <chr> <chr>  <int> <list>                <list> <lis> <chr>
#> 1 var_response resp… NA    NA        NA <NULL>                <NULL> <chr> bina…
#> 2 var_bmi      bmi   NA    NA        NA <NULL>                <NULL> <chr> cont…
#> # ℹ 13 more variables: storage_type <chr>, distribution <chr>,
#> #   reference <list>, coding <chr>, weight_type <chr>, cluster_type <chr>,
#> #   event_value <list>, transformation <list>, model_term <list>,
#> #   missing_policy <chr>, source <chr>, locked <lgl>, status <chr>
```
