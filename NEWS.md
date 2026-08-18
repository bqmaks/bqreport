# bqreport 0.0.0.9000

## Comparison analyses

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
