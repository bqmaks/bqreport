# Access the composite outcome registry

Component names are resolved from stable variable identifiers when
accessed. Missing components invalidate the returned registry without
mutating the source object.

## Usage

``` r
outcomes(x)
```

## Arguments

- x:

  A `bq_data` object.

## Value

A tidy outcome registry.
