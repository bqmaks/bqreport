# This demo follows one continuous summary analysis through every implemented
# layer. The final object is a renderer-neutral table specification: rendering
# to gt, flextable or another output format is intentionally outside the
# current package scope.

# Start with ordinary rectangular data. as_bq_data() keeps the tibble workflow
# while attaching a flat variable registry with stable variable identifiers.
study <- as_bq_data(data.frame(
  age = c(40, 55, 61, 48, 52, NA, 70, 66),
  bmi = c(22.4, 31.2, 27.8, NA, 24.9, 29.1, 26.2, 28.4),
  treatment = factor(
    c("A", "A", "B", "B", "A", "B", "A", "B"),
    levels = c("A", "B")
  ),
  centre = factor(
    c("North", "South", "North", "South", "North", "South", "North", "South"),
    levels = c("North", "South")
  )
))

# A dictionary records analytic metadata without changing column values.
# Binary event and nominal reference levels are explicit decisions. Rounding
# is presentation metadata: calculations continue to use unrounded numbers.
dictionary <- data.frame(
  name = c("age", "bmi", "treatment", "centre"),
  label = c("Age", "Body mass index", "Treatment", "Centre"),
  role = c("predictor", "predictor", "group", "group"),
  type = c("continuous", "continuous", "binary", "nominal"),
  event = c(NA, NA, "B", NA),
  reference = c(NA, NA, NA, "North"),
  unit = c("years", "kg/m^2", NA, NA),
  rounding = c("decimal", "decimal", NA, NA),
  digits = c(0, 1, NA, NA)
)
study <- apply_dictionary(study, dictionary)

# The registries expose all decisions before analysis. Values remain intact.
variables(study)
study

# Presentation templates are separate from calculations. All variable
# selectors use tidyselect syntax; here one call configures both continuous
# columns. Template names later become row labels.
study <- set_summary_format(
  study,
  c(age, bmi),
  c(
    "Mean (SD)" = "{mean} ({sd})",
    "Median (Q1; Q3)" = "{median} ({q1}; {q3})"
  )
)
summary_formats(study)

# plan_summary() fixes group, strata and raw Overall axes. add_statistic()
# selects summary variables and records their calculation specification. Its
# ordinary default is continuous_descriptives(). Overall always pools raw
# values and is never model-based.
plan <- plan_summary(
  study,
  group = treatment,
  strata = centre,
  overall = c("group", "strata")
) |>
  add_statistic(c(age, bmi))

# Small cells may list their observed values and retain statistics at the same
# time. This rule changes presentation only, never the calculation.
plan <- add_display_rule(
  plan,
  c(age, bmi),
  enumerate_values(max_n = 2L, display_statistics = TRUE)
)
plan

# preflight() validates the entire plan and compiles analysis cells without
# computing estimates. Blocking diagnostics make ok FALSE; warnings remain
# inspectable but do not prevent execution.
checked <- preflight(plan)
checked$ok
checked$diagnostics
checked$cells

# The engine receives original within-cell vectors, including missing values.
# Built-in descriptives apply their declared missing-value policy. Raw results
# contain sample sizes and unrounded long-form estimates.
result <- run_analysis(plan)
result$sample_sizes
head(result$estimates, 14L)

# Presentation preparation decides whether each cell shows statistics, values,
# or both. It still preserves numeric values.
presentation <- prepare_presentation(result)
presentation$display_cells
presentation$display_values

# Formatting is the first stage that turns numbers into text. Decimal mark,
# separators and trailing-zero removal are reporting choices; units remain
# separate variable metadata rather than being repeated after every value.
formatted <- format_presentation(
  presentation,
  decimal_mark = ",",
  trim_trailing_zeros = TRUE
)
head(formatted$formatted_estimates, 14L)
formatted$enumerations

# compose_table() arranges formatted content into flat rows, columns, headers
# and body registries. These registries are ready for a future renderer.
table <- compose_table(formatted)
table$rows
table$columns
table$column_axes
head(table$body, 20L)

# The extended built-in set can be supplied to add_statistic() without
# duplicating the basic components. It adds IQR, MAD, adjusted type-2 skewness
# and excess kurtosis. datawizard is optional, so this branch is explicit.
if (requireNamespace("datawizard", quietly = TRUE)) {
  extended <- continuous_descriptives_extended()
  extended <- set_component_rounding(
    extended,
    c("skewness", "excess_kurtosis"),
    digits = 3L
  )
  extended_plan <- plan_summary(study) |>
    add_statistic(c(age, bmi), extended)
  extended_result <- run_analysis(extended_plan)
  extended_result$estimates[
    extended_result$estimates$component %in%
      c("iqr", "mad", "skewness", "excess_kurtosis"),
  ]
}

# A custom function owns its missing-value policy, must handle numeric(0), and
# returns one data-frame row with a stable numeric schema. Different calls can
# configure different variable subsets in one pipeline.
range_and_n <- continuous_statistic(
  "range_and_n",
  function(x) {
    observed <- x[!is.na(x)]
    data.frame(
      range = if (length(observed) == 0L) {
        NA_real_
      } else {
        as.double(diff(range(observed)))
      },
      observed = as.integer(length(observed))
    )
  },
  scale = c(range = "variable", observed = "count")
)

custom_study <- set_summary_format(
  study,
  c(age, bmi),
  c("Range" = "{range}", "Observed" = "{observed}")
)
custom_plan <- plan_summary(custom_study) |>
  add_statistic(c(age, bmi), range_and_n)
custom_result <- run_analysis(custom_plan)
custom_result$estimates
