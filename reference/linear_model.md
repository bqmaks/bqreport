# Construct built-in method specifications

Construct built-in method specifications

## Usage

``` r
linear_model(ci_method = "t", exponentiate = FALSE)

logistic_model(ci_method = "wald", exponentiate = TRUE)
```

## Arguments

- ci_method:

  Confidence interval method.

- exponentiate:

  Whether estimates and compatible contrasts are returned on the
  exponentiated ratio scale.

## Value

A concrete `method_spec`.
