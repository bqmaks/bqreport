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
  outcome = response,
  group = arm,
  data = trial,
  reference = "control"
)

result
result$tests
result$sample_flow
```

With `data`, `outcome` and `group` accept tidyselect syntax: bare column
names or character strings. Every result carries the executed `specification`
next to its tables.

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

## Multiple comparisons

Comparison-family providers can be used as planned multiple comparisons or in
a post hoc workflow. They use the same standalone runner but remain independent
from omnibus tests. They return the selected family in `result$comparisons`
and observation accounting in `result$sample_flow`; they do not compute or
return `tests` or `estimates`.

| Provider | Comparison method | Comparison family |
|---|---|---|
| `t_family()` | Student or Welch t | pairwise, reference or consecutive |
| `mann_whitney_family()` | Mann-Whitney | pairwise, reference or consecutive |
| `brunner_munzel_family()` | Brunner-Munzel | pairwise, reference or consecutive |
| `dunn_test()` | Dunn rank comparison | pairwise, reference or consecutive |
| `tukey_test()` | Tukey HSD | all pairs, fixed Tukey adjustment |
| `dunnett_test()` | Dunnett | reference, fixed Dunnett adjustment |
| `games_howell_test()` | Games-Howell | all pairs, fixed Games-Howell adjustment |

```r
trial3 <- data.frame(
  response = c(8, 10, 12, 1, 2, 6, 4, 5, 7),
  arm = factor(
    rep(c("control", "low", "high"), each = 3),
    levels = c("control", "low", "high")
  )
)

reference_result <- run_comparison(
  t_family(
    comparisons = "reference",
    reference = "control",
    var_equal = FALSE,
    p_adjust = "holm"
  ),
  outcome = response,
  group = arm,
  data = trial3
)

consecutive_result <- run_comparison(
  mann_whitney_family(
    comparisons = "consecutive",
    exact = "auto",
    p_adjust = "holm"
  ),
  outcome = response,
  group = arm,
  data = trial3
)

reference_result$comparisons
consecutive_result$comparisons
```

All estimates and statistics are oriented as `comparison - reference`.
Factor levels determine the orientation of pairwise comparisons and define
which levels are adjacent for a consecutive family. Other group vectors are
sorted: character groups lexicographically, numeric groups ascending.
Multiplicity correction applies only to the declared family; it is not
conditional on or coupled to a separate omnibus analysis. In `comparisons`,
`p_value` is always the unadjusted value and `p_value_adjusted` the
family-adjusted one; `ci_clamped` marks intervals truncated at the boundary of
a bounded estimand.

The specialized procedures can be declared directly:

```r
tukey_test()
dunnett_test(reference = "control")
games_howell_test()
dunn_test(comparisons = "pairwise", p_adjust = "holm")
```

`dunn_test()`, `dunnett_test()` and `games_howell_test()` require the
suggested package `PMCMRplus` version 1.9.12 or later. There is no runtime
fallback to another method when that engine is unavailable.

## Metadata-aware summaries

For repeatable analysis and reporting, data can be paired with metadata and an
inspectable plan:

```r
data <- as_bq_data(data.frame(
  age = c(40, 55, 61, NA),
  arm = c("control", "control", "new", "new")
))
data <- set_type(data, age, type_continuous())
data <- set_type(data, arm, type_binary("new"))
data <- set_rounding(data, age, digits = 1)

plan <- plan_summary(data, group = arm) |>
  add_statistic(age)

preflight(plan)
result <- run_analysis(plan)
table <- compose_table(format_presentation(prepare_presentation(result)))
table
tibble::as_tibble(table)
```

The plan is the source of analytic decisions. Computation, diagnostics,
formatting and renderer-neutral table composition remain separate stages.
`compose_table()` returns flat registries; printing it, or calling
`as_tibble()`, lays them out as one wide tibble for inspection.

Analytic types are declared with `type_continuous()`, `type_count()`,
`type_binary()`, `type_ordinal()` and `type_nominal()`.

## Development checks

```r
devtools::test()
devtools::check(args = "--no-manual")
```

See [ROADMAP.md](ROADMAP.md) for planned work.
