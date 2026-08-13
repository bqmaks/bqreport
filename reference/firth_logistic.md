# Construct Firth penalized logistic regression

Construct Firth penalized logistic regression

## Usage

``` r
firth_logistic(exponentiate = TRUE)
```

## Arguments

- exponentiate:

  Whether coefficients and confidence limits are returned as odds ratios
  rather than log odds.

## Value

A concrete custom `method_spec` using the optional `logistf` backend.
