# Apply a variable metadata dictionary

The dictionary uses column names as initialization-time keys. Metadata
are stored in the variable registry; `digits` never changes the
underlying data and is intended only for presentation layers.

## Usage

``` r
apply_dictionary(.data, metadata)
```

## Arguments

- .data:

  A `bq_data` object.

- metadata:

  A data frame with a required `name` column and optional `label`,
  `unit`, and `digits` columns. Descriptive-statistic display templates
  can be supplied as a `descriptive_templates` list-column whose
  elements are character vectors. Labelled metadata can be supplied as
  list-columns named `value_labels`, `na_values`, and `na_range`.
  Additional, non-reserved columns are preserved in the variable
  registry. Missing property values leave the current metadata
  unchanged.

## Value

`.data` with an updated variable registry.
