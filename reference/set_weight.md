# Configure a regression weight

Configure a regression weight

## Usage

``` r
set_weight(.data, .cols, type = c("ipw", "frequency", "precision"))
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Exactly one weight column selected with tidyselect.

- type:

  Weight semantics: frequency, precision, or IPW.

## Value

Updated `bq_data`.
