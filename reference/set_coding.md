# Configure model coding

Configure model coding

## Usage

``` r
set_coding(.data, .cols, coding = "treatment", reference)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Categorical predictors selected with tidyselect.

- coding:

  Currently only treatment coding is supported.

- reference:

  Reference value.

## Value

Updated `bq_data`.
