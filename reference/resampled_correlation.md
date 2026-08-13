# Add bootstrap intervals and permutation inference to a correlation method

Add bootstrap intervals and permutation inference to a correlation
method

## Usage

``` r
resampled_correlation(
  method = pearson_correlation(),
  bootstrap = 999L,
  permutations = 999L,
  seed
)
```

## Arguments

- method:

  A correlation method specification.

- bootstrap:

  Number of bootstrap replicates, or zero.

- permutations:

  Number of permutation replicates, or zero.

- seed:

  Required integer seed used without changing global RNG state.

## Value

A resampling `correlation_method_spec`.
