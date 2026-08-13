# Construct a correlation comparator

Construct a correlation comparator

## Usage

``` r
correlation_comparator(id, compare, methods, required_packages = character())
```

## Arguments

- id:

  Stable comparator identifier.

- compare:

  Function accepting a read-only comparison context and returning
  [`correlation_comparison_output()`](https://bqmaks.github.io/bqreport/reference/correlation_comparison_output.md).

- methods:

  Correlation method identifiers supported by the comparator.

- required_packages:

  Optional packages checked during preflight.

## Value

A `correlation_comparator_spec`.
