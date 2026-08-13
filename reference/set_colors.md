# Set colors for categorical variables

`colors` may be an unnamed character vector in level order, a named
vector keyed by level, or a function receiving the current character
vector of levels and returning either form. Functional specifications
are retained in metadata while their resolved mapping is snapshotted for
reproducibility.

## Usage

``` r
set_colors(.data, .cols, colors)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Categorical columns selected with tidyselect.

- colors:

  A character vector or palette function.

## Value

`.data` with updated variable metadata.
