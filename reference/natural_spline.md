# Construct a natural spline covariate term

Construct a natural spline covariate term

## Usage

``` r
natural_spline(df = NULL, knots = NULL, boundary_knots = NULL)
```

## Arguments

- df:

  Optional degrees of freedom, at least two.

- knots:

  Optional strictly increasing interior knots.

- boundary_knots:

  Optional two strictly increasing boundary knots.

## Value

A `model_term_spec`.
