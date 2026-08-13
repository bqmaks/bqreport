# Register target comparisons

Register target comparisons

## Usage

``` r
set_comparisons(.data, .cols, comparisons, adjust = "none")
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Predictor columns selected with tidyselect.

- comparisons:

  A `contrast_spec`.

- adjust:

  A method accepted by
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Value

Updated `bq_data`.
