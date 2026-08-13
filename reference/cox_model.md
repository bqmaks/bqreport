# Construct a Cox proportional hazards method

Construct a Cox proportional hazards method

## Usage

``` r
cox_model(ties = c("efron", "breslow", "exact"))
```

## Arguments

- ties:

  Ties method passed to
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

## Value

A concrete method specification.
