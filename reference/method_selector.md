# Construct a data-dependent method selector

A selector is evaluated once during
[`validate_plan()`](https://bqmaks.github.io/bqreport/reference/validate_plan.md)
and must choose one of its explicitly named candidate methods. It is
never used as a runtime fallback.

## Usage

``` r
method_selector(id, candidates, select, required_packages = character())
```

## Arguments

- id:

  Stable selector identifier.

- candidates:

  Named list of concrete `method_spec` objects.

- select:

  Function accepting an `analysis_context` and returning
  [`method_choice()`](https://bqmaks.github.io/bqreport/reference/method_choice.md).

- required_packages:

  Packages needed to evaluate the selector.

## Value

A `method_selector` specification.
