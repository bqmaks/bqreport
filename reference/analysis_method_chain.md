# Construct an explicit runtime method chain

Methods are attempted in order. Advancement occurs only when a failed
method signals one of the condition classes declared in `advance_on`.
Every attempted method is retained in the result audit trail.

## Usage

``` r
analysis_method_chain(id, methods, advance_on)
```

## Arguments

- id:

  Stable identifier for the chain.

- methods:

  Uniquely named list of custom `method_spec` objects.

- advance_on:

  Non-empty character vector of condition classes that permit
  advancement.

## Value

A concrete `analysis_method_chain` and `method_spec` object.
