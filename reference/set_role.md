# Add an analytical role to variables

Roles are additive: a variable can simultaneously be, for example, a
`group` and a `predictor`. Assigning the same role repeatedly has no
effect.

## Usage

``` r
set_role(.data, .cols, role)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- role:

  A single supported role.

## Value

`.data` with an updated variable registry.

## Examples

``` r
data <- as_bq_data(tibble::tibble(
  patient_id = c("p1", "p2", "p3"),
  arm = factor(c("A", "B", "A"))
)) |>
  set_role(patient_id, "id") |>
  set_role(arm, "group")
variables(data)$role
#> [[1]]
#> [1] "id"
#> 
#> [[2]]
#> [1] "group"
#> 
```
