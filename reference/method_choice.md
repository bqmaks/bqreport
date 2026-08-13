# Record a method selector decision

Record a method selector decision

## Usage

``` r
method_choice(method, reason, diagnostics = tibble::tibble())
```

## Arguments

- method:

  Name of one candidate declared in
  [`method_selector()`](https://bqmaks.github.io/bqreport/reference/method_selector.md).

- reason:

  Human-readable basis for the decision.

- diagnostics:

  A tidy data frame of unrounded pre-fit diagnostics.

## Value

A `method_choice` object.
