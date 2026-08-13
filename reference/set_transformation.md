# Assign scalar transformations

Assign scalar transformations

## Usage

``` r
set_transformation(.data, .cols, transformation)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Numeric predictors or covariates selected with tidyselect.

- transformation:

  A `transformation_spec`.

## Value

Updated `bq_data`.
