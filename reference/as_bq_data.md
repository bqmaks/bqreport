# Convert data to a bq data object

`as_bq_data()` creates a tibble subclass and initializes the package's
metadata registries. Existing `bq_data` objects are returned without
regenerating variable identifiers.

## Usage

``` r
as_bq_data(x, metadata = NULL)
```

## Arguments

- x:

  A data frame.

- metadata:

  An optional metadata data frame. See
  [`apply_dictionary()`](https://bqmaks.github.io/bqreport/reference/apply_dictionary.md).

## Value

A `bq_data` tibble.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  age = c(44, 57, 51, 63),
  treatment = factor(c("Control", "Control", "Treatment", "Treatment"))
))
variables(data)
#> # A tibble: 2 × 22
#>   var_id       name  label unit  digits descriptive_templates colors role  type 
#>   <chr>        <chr> <chr> <chr>  <int> <list>                <list> <lis> <chr>
#> 1 var_age      age   NA    NA        NA <NULL>                <NULL> <chr> unkn…
#> 2 var_treatme… trea… NA    NA        NA <NULL>                <NULL> <chr> bina…
#> # ℹ 13 more variables: storage_type <chr>, distribution <chr>,
#> #   reference <list>, coding <chr>, weight_type <chr>, cluster_type <chr>,
#> #   event_value <list>, transformation <list>, model_term <list>,
#> #   missing_policy <chr>, source <chr>, locked <lgl>, status <chr>
```
