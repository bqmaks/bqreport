# bqreport 0.0.0.9000

## Review follow-up

- Type constructors are now `type_continuous()`, `type_count()`,
  `type_binary()`, `type_ordinal()` and `type_nominal()`; the bare names
  collided with `dplyr::count()`.
- `run_comparison()` accepts tidyselect column selections when `data` is
  supplied, sorts numeric groups numerically, rejects ordinal outcomes for
  providers that do not declare them, and returns a `bq_result_comparison`
  carrying the executed `specification`.
- In `comparisons`, `p_value` is the unadjusted value and `p_value_adjusted`
  the family-adjusted one. New `ci_clamped` and `effect_ci_clamped` columns
  mark intervals truncated at the boundary of a bounded estimand; the
  Brunner-Munzel family previously dropped that flag.
- The `test` column of `tests` names the procedure only (`welch_t`,
  `hodges_lehmann`, `kruskal_wallis`, ...); `inference` records how it was
  evaluated.
- Provider `capabilities` keep only the fields something reads.
- Replacing a subset of rows in a `bq_data` column keeps its metadata;
  `cbind()` and `merge()` are rejected instead of silently dropping the
  registry.
- Preflight, result and table objects print compact summaries;
  `as_tibble()` on a composed table gives a wide layout for inspection.
- Shared validation helpers replace per-provider copies of argument, engine
  input and control-specification checks.

## Comparison analyses

- Added standalone multiple-comparison providers with a common `comparisons`
  result schema and explicit `comparison - reference` orientation.
- Comparison-family providers are independent from omnibus tests and return only
  `comparisons` and `sample_flow`; omnibus analyses are declared and executed
  separately.
- Added `t_family()`, `mann_whitney_family()` and
  `brunner_munzel_family()` with pairwise, reference and consecutive
  families and family-specific p-value adjustment.
- Character group values are ordered lexicographically; explicit factor level
  order remains authoritative.
- Added Tukey all-pairs comparisons through `tukey_test()` and PMCMRplus-backed
  Dunn, Dunnett and Games-Howell providers. Specialized procedures keep their
  intrinsic family and multiplicity policy.
- Added `run_comparison()` for executing all five terminal comparison
  specifications directly from vectors or ordinary data frames.
- Added compact printing for analytic function specifications.
- Isolated permutation and bootstrap RNG stages in `t_test()` and
  `mann_whitney_test()`; fixed the fractional-bootstrap dependency reported by
  `t_test()`.
- Added finite-result guards for Kruskal-Wallis and one-way ANOVA.

## Data and summary workflow

- Named tidyselect aliases now resolve to canonical registry names.
- `bq_data` rejects duplicate and empty names during renaming.
- Ordinal design axes compile from their declared level registry, including
  unobserved levels.
- Preflight now diagnoses non-atomic design axes, stale binary or ordinal
  domains and invalid continuous storage before computation.
- Summary engines build a plain numeric model vector without changing source
  data.

## Package engineering

- Internal session files and local agent directories are excluded from source
  packages.
- Added direct-use documentation, a development roadmap and regression tests
  for the new contracts.
