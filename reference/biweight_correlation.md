# Construct robust and latent-correlation methods

`biweight_correlation()` reports a point estimate only: it has no
analytic standard error, confidence interval, or p-value. Validated plan
rows using it therefore receive status `review` and must either be
approved explicitly with
[`approve_plan()`](https://bqmaks.github.io/bqreport/reference/approve_plan.md)
or wrapped in
[`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md)
to obtain bootstrap inference.

## Usage

``` r
biweight_correlation()

polychoric_correlation()

tetrachoric_correlation()
```

## Value

A concrete `correlation_method_spec`.
