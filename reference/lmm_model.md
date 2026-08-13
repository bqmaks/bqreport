# Construct longitudinal model specifications

Construct longitudinal model specifications

## Usage

``` r
lmm_model(reml = FALSE)

gee_model(correlation = c("exchangeable", "independence", "ar1"))

glmm_model()

binary_gee_model(correlation = c("exchangeable", "independence", "ar1"))
```

## Arguments

- reml:

  Whether LMM estimation uses REML. Defaults to FALSE so nested
  fixed-effect tests are comparable.

- correlation:

  Working correlation structure for GEE.

## Value

A concrete longitudinal method specification.
