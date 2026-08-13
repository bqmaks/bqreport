# Convert data to a bq data object

`as_bq_data()` creates a tibble subclass and initializes the package's
metadata registries. Existing `bq_data` objects are returned without
regenerating variable identifiers.

## Usage

``` r
as_bq_data(x, metadata = NULL)
```

## Arguments

- x:

  A data frame.

- metadata:

  An optional metadata data frame. See
  [`apply_dictionary()`](https://bqmaks.github.io/bqreport/reference/apply_dictionary.md).

## Value

A `bq_data` tibble.
