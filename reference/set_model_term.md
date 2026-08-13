# Configure a nonlinear model term

Model terms expand one covariate into a fixed basis during preflight.
They are distinct from scalar transformations and are currently
supported only for adjustment covariates.

## Usage

``` r
set_model_term(.data, .cols, term)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Numeric columns selected with tidyselect.

- term:

  A `model_term_spec`.

## Value

Updated `bq_data`.
