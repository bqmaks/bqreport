# Validate an analysis plan against data

Preflight resolves variables by stable identifiers, refreshes formulas
and counts complete observations while treating labelled special missing
values as missing only in the internal analysis view.

## Usage

``` r
validate_plan(plan, data)
```

## Arguments

- plan:

  An `analysis_plan`.

- data:

  A `bq_data` object.

## Value

A validated `analysis_plan`.
