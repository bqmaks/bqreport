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
