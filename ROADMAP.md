# Development roadmap

The order below preserves the package architecture: decisions are declared
and inspected before an engine runs, and computation stays separate from
presentation.

## 1. Complete the comparison contract

- Keep omnibus tests and multiple-comparison families as separate analytic
  entities with independent result objects.
- Keep `t_family()`, `mann_whitney_family()` and
  `brunner_munzel_family()` as explicit providers for now. A future generic
  `test_family(test, comparisons, reference, p_adjust)` may compile a supported
  two-group test specification into a family, but it must not wrap intrinsic
  post hoc procedures such as Dunn, Tukey, Dunnett or Games-Howell.
- Limit test families to two-sided analytical inference until family-level
  permutation, bootstrap, directional and equivalence policies are designed
  explicitly.
- Extend the implemented comparison-family providers with explicit effect
  sizes and a separately designed resampling policy where statistically
  justified.
- Separate hypothesis-test intervals from estimand/bootstrap intervals in
  result schemas.
- Add runtime coverage for BCa and basic bootstrap intervals and extend RNG
  isolation tests to every resampling provider.
- Add stable accessors for tests, estimates and sample flow.

### Approved order for the remaining multiple-comparison work

All four providers exist and agree numerically with their reference
implementations; what remains is completing their estimate contracts (standard
errors, simultaneous intervals, optional effect sizes). Extend one provider at
a time in this order: `dunn_test()`, `tukey_test()`, `dunnett_test()` and
`games_howell_test()`. Across all providers, keep the
primary estimand and its uncertainty separate from a standardized effect and
its uncertainty. An individual effect-size interval must never be presented as
a simultaneous familywise interval.

1. `dunn_test()` keeps the pooled mean-rank difference as its primary
   estimate and returns the tie-corrected Dunn standard error. Its primary
   interval remains missing because general adjustment methods such as Holm or
   false-discovery-rate control do not define matching simultaneous intervals.
   Cliff's delta is returned as a pairwise descriptive companion, not as a
   transformation of the globally ranked Dunn statistic; its standard error
   and interval remain missing.
2. `tukey_test()` returns the mean difference, its standard error and the
   simultaneous Tukey interval. Optional `"cohens_d"` and `"hedges_g"`
   effects use the common ANOVA residual standard deviation; `"none"` remains
   the default. Any noncentral-t effect interval is individual and unadjusted,
   while `effect_std_error` remains missing because that interval is not built
   from a returned same-scale standard error.
3. `dunnett_test()` returns every reference contrast, its pooled-model
   standard error and a simultaneous Dunnett interval. Optional Cohen's d or
   Hedges' g uses the common residual standard deviation and an explicitly
   individual effect interval. Inspect the public PMCMRPlus API before coding;
   if it cannot supply the interval contract, choose an explicit Suggested
   engine such as `mvtnorm` or `multcomp` rather than calling package internals.
4. `games_howell_test()` returns the mean difference, Welch standard error,
   pair-specific degrees of freedom and a simultaneous Games-Howell interval.
   Optional Cohen's d or Hedges' g follows the existing Welch convention and
   uses an unpooled standard deviation. Any effect interval is individual and
   remains separate from Games-Howell familywise inference.

After all four providers are complete, update README, NEWS and the handoff,
verify the common typed column schema and standalone examples, then run the
full tests, source build and `R CMD check --no-manual` before requesting commit
and push authorization.

### Metadata-aware comparisons and categorical summaries

Both are designed before they are coded, in this order:

- `plan_comparison()`: a comparison plan built from `bq_data`, with outcome and
  group taken from the registry, `type` and `reference` respected, a preflight
  method that checks provider capabilities against declared types, and
  `run_analysis()` executing the same providers `run_comparison()` uses.
- Categorical descriptives for `binary`, `nominal` and `ordinal` variables in
  summary plans (`n (%)` with declared denominators), followed by the matching
  contingency-table comparisons.

## 2. Regression models

- Specify model plans for linear, binary, count and ordinal outcomes.
- Add explicit contrast and reference-level declarations.
- Define diagnostics, convergence postconditions and model-frame coercion
  before implementing engines.

## 3. Time-to-event and repeated-measure analyses

- Add survival outcomes, censoring/event contracts and Cox-model plans.
- Design longitudinal data roles, within-subject correlation and missing-data
  policies.
- Add mixed-model engines only after their estimands and failure policies are
  inspectable in preflight.

## 4. Publication outputs

- Stabilize renderer-neutral table schemas and add renderer adapters.
- Add publication plots sourced from raw result objects rather than formatted
  tables.
- Add vignettes covering summary, comparison and model workflows.

## 5. Release engineering

- Add cross-platform continuous integration and minimum supported dependency
  versions.
- Add reverse-dependency and optional-engine test matrices.
- Publish a versioned website after the public APIs above stabilize.
