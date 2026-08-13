# Changelog

## bqreport (development version)

### Reproducibility

- All identifiers (`var_id`, `outcome_id`, `design_id`, `analysis_id`,
  `contrast_id`, correlation family and interaction ids) are now
  deterministic content digests instead of random UUIDs. Identical
  inputs compile to identical plans and results across sessions, so
  plans and results can be diffed between runs. The `uuid` dependency
  was dropped.

### Correlation analysis

- [`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md)
  bootstrap replicates now resample analysis weights together with their
  observations. Previously the weight vector stayed in its original
  order, silently pairing resampled observations with the wrong weights.
- [`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md)
  of subject-identified methods
  ([`repeated_measures_correlation()`](https://bqmaks.github.io/bqreport/reference/weighted_pearson_correlation.md))
  now performs a cluster bootstrap that resamples whole subjects, and
  permutation replicates permute outcome values within subjects only,
  preserving the within-subject estimand.
- Spearman confidence intervals now use the Bonett-Wright (2000)
  standard error `sqrt((1 + r^2 / 2) / (n - k - 3))` on the Fisher z
  scale instead of the anti-conservative Pearson formula. The declared
  `ci_method` is `"fisher_z_bonett_wright"`.
- Methods that report a point estimate without inference
  ([`biweight_correlation()`](https://bqmaks.github.io/bqreport/reference/biweight_correlation.md))
  now validate to status `review` and require either explicit
  [`approve_plan()`](https://bqmaks.github.io/bqreport/reference/approve_plan.md)
  or wrapping in
  [`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md).
- Resampled methods keep the minimum-observations rule of their base
  method during preflight.
- Kendall interval limitations (normal approximation on the raw tau
  scale) are documented;
  [`resampled_correlation()`](https://bqmaks.github.io/bqreport/reference/resampled_correlation.md)
  is recommended when the Kendall interval matters.

### Package infrastructure

- Core exported functions gained runnable examples.
- Added an `R CMD check` GitHub Actions workflow.
- Real package author contact in `DESCRIPTION`.

## bqreport 0.0.0.9000

- Initial development version: `bq_data` metadata registries, tidyselect
  selectors, validated analysis plans, descriptive statistics, group
  comparisons, univariable linear/logistic/count/ordinal/multinomial
  models, survival and competing-risk analysis, longitudinal
  LMM/GLMM/GEE engines, correlation workflows, custom method contracts
  with explicit fallback chains, reporting tables and plots, vignettes,
  and a pkgdown site.
