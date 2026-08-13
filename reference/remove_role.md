# Remove an analytical role from variables

If the last substantive role is removed, the variable is assigned the
default `auxiliary` role.

## Usage

``` r
remove_role(.data, .cols, role)
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Columns selected using tidyselect syntax.

- role:

  A single supported role.

## Value

`.data` with an updated variable registry.
