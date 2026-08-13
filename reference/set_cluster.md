# Configure an observation cluster

Configure an observation cluster

## Usage

``` r
set_cluster(.data, .cols, type = "matched_set")
```

## Arguments

- .data:

  A `bq_data` object.

- .cols:

  Exactly one cluster identifier selected with tidyselect.

- type:

  Cluster semantics. Currently matched sets are supported.

## Value

Updated `bq_data`.
