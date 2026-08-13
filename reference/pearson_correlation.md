# Construct correlation methods

Pearson intervals use the Fisher z transformation; Spearman intervals
use the Bonett-Wright standard error on the Fisher z scale. Kendall
intervals rely on a coarse normal approximation on the raw tau scale,
clipped to `[-1, 1]`; when the Kendall interval matters, prefer wrapping
the method in
[`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md)
for bootstrap inference.

## Usage

``` r
pearson_correlation()

spearman_correlation()

kendall_correlation()
```

## Value

A concrete `correlation_method_spec`.
