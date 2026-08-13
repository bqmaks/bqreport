# Construct a Poisson count-regression specification

Construct a Poisson count-regression specification

## Usage

``` r
poisson_model(ci_method = "wald", exponentiate = TRUE)
```

## Arguments

- ci_method:

  Confidence interval method.

- exponentiate:

  Whether estimates and compatible contrasts are returned on the
  exponentiated ratio scale.

## Value

A concrete `method_spec` returning rate ratios by default.
