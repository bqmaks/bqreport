# Configure the Shapiro–Wilk normality diagnostic

The provider returns `shapiro.statistic` and `shapiro.p.value`. It
records non-computable populations explicitly and never updates
distribution metadata or selects a downstream analysis method.

## Usage

``` r
shapiro_wilk(id = "shapiro_wilk")
```

## Arguments

- id:

  Stable provider identifier.

## Value

A `descriptive_function` specification.
