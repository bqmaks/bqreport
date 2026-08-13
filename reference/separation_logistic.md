# Select ordinary or Firth logistic regression before fitting

The selector uses `detectseparation` during preflight. Finite maximum
likelihood estimates select
[`logistic_model()`](https://bqmaks.github.io/bqreport/reference/linear_model.md);
complete or quasi-complete separation selects
[`firth_logistic()`](https://bqmaks.github.io/bqreport/reference/firth_logistic.md).
The selected method is fixed in the validated plan.

## Usage

``` r
separation_logistic(exponentiate = TRUE, id = "separation_glm_or_firth")
```

## Arguments

- exponentiate:

  Whether either candidate returns odds ratios.

- id:

  Stable selector identifier.

## Value

A `method_selector` with `glm` and `firth` candidates.
