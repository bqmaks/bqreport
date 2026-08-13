# Construct a negative-binomial count-regression specification

Construct a negative-binomial count-regression specification

## Usage

``` r
negative_binomial_model(ci_method = "wald", exponentiate = TRUE)
```

## Arguments

- ci_method:

  Confidence interval method.

- exponentiate:

  Whether estimates and compatible contrasts are returned on the
  exponentiated ratio scale.

## Value

A concrete custom `method_spec` using the optional `MASS` backend.
