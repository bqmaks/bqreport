# Examples gallery

This page is a compact map from common questions to the corresponding
`bqreport` workflow.

## Baseline characteristics with an overall column

``` r

result <- data |>
  plan_descriptives(
    variables = c(age, sex, biomarker),
    groups = treatment,
    overall = TRUE,
    comparisons = TRUE
  ) |>
  validate_plan(data) |>
  run_analysis(data)

tbl_descriptive(result, overall_label = "All patients") |> as_gt()
```

## Adjusted model with treatment contrasts

``` r

data <- data |>
  set_comparisons(treatment, all_pairwise(), adjust = "holm")

result <- data |>
  plan_analysis(response, treatment, covariates = c(age, baseline_score)) |>
  validate_plan(data) |>
  run_analysis(data)

tbl_regression(result) |> as_flextable()
tbl_comparison(result)
plot_forest(result, component = "contrasts")
```

## Partial correlations with interaction across strata

``` r

result <- data |>
  plan_correlations(
    biomarker_a, with = biomarker_b,
    adjust_for = age,
    strata = treatment,
    interaction_test = TRUE,
    adjust = "holm"
  ) |>
  validate_plan(data) |>
  run_analysis(data)

correlations(result)
tests(result)
contrasts(result)
```

## Repeated outcomes supplied in wide format

``` r

data <- raw |>
  as_bq_data() |>
  set_longitudinal_design(id = patient_id, group = treatment, layout = "wide") |>
  add_longitudinal_outcome(
    bmi,
    values = c(bmi_v0, bmi_v1, bmi_v2),
    time = c("V0", "V1", "V2"),
    baseline = "V0",
    time_scale = "categorical"
  )

result <- data |>
  plan_longitudinal(bmi, method = lmm_model(), comparisons = TRUE) |>
  validate_plan(data) |>
  run_analysis(data)
```

## Survival probabilities, quantiles, and RMST

``` r

result <- data |>
  plan_kaplan_meier(
    overall_survival,
    groups = treatment,
    times = c(6, 12, 24),
    quantiles = c(.25, .5),
    rmst_tau = 24,
    comparisons = against_reference("Control"),
    adjust = "holm"
  ) |>
  validate_plan(data) |>
  run_analysis(data)

tbl_survival(result)
plot_survival(result)
```

## Project-specific palette

``` r

data <- data |>
  set_colors(treatment, c(
    Control = "#4477AA",
    Dose_1 = "#DDCC77",
    Dose_2 = "#CC6677"
  ))
```

Unnamed vectors follow factor-level order. Named vectors map levels
explicitly, and functions may generate a palette from the current
levels. The resolved mapping is snapshotted in the analysis plan.
