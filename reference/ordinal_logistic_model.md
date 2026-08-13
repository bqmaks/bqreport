# Construct a proportional-odds ordinal logistic specification

Construct a proportional-odds ordinal logistic specification

## Usage

``` r
ordinal_logistic_model(ci_method = "wald", exponentiate = TRUE)
```

## Arguments

- ci_method:

  Confidence interval method.

- exponentiate:

  Whether estimates and compatible contrasts are returned on the
  exponentiated ratio scale.

## Value

A concrete custom `method_spec` using the optional `MASS` backend.
