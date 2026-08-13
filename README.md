# bqreport

`bqreport` is a metadata-aware framework for reproducible biomedical data
analysis. It separates study metadata, inspectable analysis plans, statistical
execution, tidy numerical results, and reporting.

```r
library(bqreport)

data <- tibble::tibble(
  response = c(0, 0, 1, 0, 1, 1, 1, 0),
  treatment = factor(rep(c("Control", "Treatment"), each = 4)),
  age = c(44, 57, 51, 63, 46, 55, 60, 49)
) |>
  as_bq_data() |>
  set_outcome(response, type = "binary", event = 1) |>
  set_predictor(treatment, type = "binary", reference = "Control") |>
  set_role(treatment, "group") |>
  set_predictor(age, type = "continuous") |>
  set_colors(treatment, c(Control = "#4477AA", Treatment = "#CC6677"))

plan <- data |>
  plan_analysis(response, treatment, covariates = age) |>
  validate_plan(data)

result <- run_analysis(plan, data)
estimates(result)
tbl_regression(result)
```

The validated plan records the chosen method, estimand, scale, confidence
interval method, transformations, required packages, and provenance before
the model is run. Engine failures are collected as issues and never trigger a
silent fallback.

## Documentation

- [Get started](https://bqmaks.github.io/bqreport/articles/get-started.html)
- [Descriptive analysis](https://bqmaks.github.io/bqreport/articles/descriptive-analysis.html)
- [Models and contrasts](https://bqmaks.github.io/bqreport/articles/models-and-contrasts.html)
- [Correlation analysis](https://bqmaks.github.io/bqreport/articles/correlation-analysis.html)
- [Longitudinal and survival analysis](https://bqmaks.github.io/bqreport/articles/longitudinal-survival.html)
- [Custom functions and engines](https://bqmaks.github.io/bqreport/articles/custom-functions.html)

The package is under active development. Numerical components are kept
unrounded; formatting is applied only by the reporting layer.
