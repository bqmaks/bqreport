# Development roadmap

The order below preserves the package architecture: decisions are declared
and inspected before an engine runs, and computation stays separate from
presentation.

## 1. Complete the comparison contract

- Design post hoc comparisons for omnibus tests, including the comparison
  family, multiplicity adjustment, compatible effect-size scale and
  resampling policy.
- Separate hypothesis-test intervals from estimand/bootstrap intervals in
  result schemas.
- Add runtime coverage for BCa and basic bootstrap intervals and extend RNG
  isolation tests to every resampling provider.
- Add stable accessors for tests, estimates and sample flow.

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
