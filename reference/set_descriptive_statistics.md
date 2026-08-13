# Configure descriptive statistic display templates

Templates are stored as metadata and are not evaluated at configuration
time. Each template represents one display row. Placeholders such as
`"{mean} ({sd})"` refer to unrounded statistics that will be supplied by
the descriptive analysis and formatted only by the presentation layer.
Model-based fields use the same contract, for example
`"{estimate} ({conf.low}; {conf.high})"`. Requesting such fields does
not store fitted values in variable metadata: a descriptive analysis
plan must declare and run the model that supplies them. For categorical
variables, `{n}` is the level count, `{N}` is the non-missing
denominator in the corresponding population, and `{p}` is the unrounded
proportion `n / N`. Quantitative templates may additionally request
`{mad}`, `{skewness}`, and `{kurtosis}`. Skewness uses the adjusted
Fisher–Pearson coefficient; kurtosis is bias-corrected excess kurtosis,
equal to zero for the reference normal distribution.

## Usage

``` r
set_descriptive_statistics(.data, .cols, templates)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- templates:

  A non-empty character vector of templates, or `NULL` to clear the
  configured templates. Each template must contain at least one simple
  `{statistic}` placeholder. Placeholder names may consist of identifier
  components separated by dots, as in `{conf.low}`.

## Value

`.data` with updated variable metadata.
