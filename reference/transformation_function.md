# Construct a custom scalar transformation

Construct a custom scalar transformation

## Usage

``` r
transformation_function(
  id,
  transform,
  label,
  parameters = list(),
  required_packages = character()
)
```

## Arguments

- id:

  Stable identifier.

- transform:

  Function accepting `(x, context)` and returning one numeric vector of
  the same length.

- label:

  Human-readable effect interpretation.

- parameters:

  Serializable significant settings.

- required_packages:

  Required packages.

## Value

A `transformation_spec`.
