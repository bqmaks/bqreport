# Select variables by analytical role

These helpers select variables solely from registry metadata. They do
not filter variables by validation status and can be composed with
ordinary tidyselect expressions.

## Usage

``` r
where_role(role)

all_outcomes()

all_predictors()

all_groups()
```

## Arguments

- role:

  A supported analytical role.

## Value

A tidyselect selection.
