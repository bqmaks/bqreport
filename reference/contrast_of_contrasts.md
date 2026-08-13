# Compare conditional effects across modifier levels

Compare conditional effects across modifier levels

## Usage

``` r
contrast_of_contrasts(modifier, inner, outer, exponentiate)
```

## Arguments

- modifier:

  Exactly one categorical effect modifier.

- inner:

  A contrast specification for levels of the main predictor.

- outer:

  A contrast specification for levels of `modifier`.

- exponentiate:

  Whether to exponentiate the model-scale contrast of contrasts. This
  must always be supplied explicitly.

## Value

A backend-independent contrast-of-contrasts specification.
