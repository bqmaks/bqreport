# bqreport

`bqreport` is an R package for reproducible biomedical data analysis. It keeps
analytic decisions inspectable before computation and separates raw results
from table and presentation layers.

The package is under active development and is not yet released on CRAN.

## Direct comparisons

Terminal comparison functions can be used with ordinary vectors, data frames
and tibbles. A metadata registry or analysis plan is not required.

```r
library(bqreport)

trial <- data.frame(
  response = c(8, 10, 12, 1, 2, 6),
  arm = rep(c("new", "control"), each = 3)
)

analysis <- t_test(var_equal = FALSE)
analysis

result <- run_comparison(
  analysis,
  outcome = "response",
  group = "arm",
  data = trial,
  reference = "control"
)

result$tests
result$sample_flow
```

The same runner accepts vectors directly:

```r
run_comparison(
  mann_whitney_test(),
  outcome = trial$response,
  group = trial$arm,
  reference = "control"
)
```

Supported terminal comparisons are `t_test()`, `mann_whitney_test()`,
`brunner_munzel_test()`, `kruskal_wallis_test()` and `oneway_anova()`.

## Metadata-aware summaries

For repeatable analysis and reporting, data can be paired with metadata and an
inspectable plan:

```r
data <- as_bq_data(data.frame(
  age = c(40, 55, 61, NA),
  arm = c("control", "control", "new", "new")
))
data <- set_type(data, age, continuous())
data <- set_type(data, arm, binary("new"))
data <- set_rounding(data, age, digits = 1)

plan <- plan_summary(data, group = arm) |>
  add_statistic(age)

preflight(plan)
result <- run_analysis(plan)
```

The plan is the source of analytic decisions. Computation, diagnostics,
formatting and renderer-neutral table composition remain separate stages.

## Development checks

```r
devtools::test()
devtools::check(args = "--no-manual")
```

See [ROADMAP.md](ROADMAP.md) for planned work.
