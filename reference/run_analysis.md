# Run a validated analysis plan

Only validated tasks with status `ready` are executed. Other tasks are
retained in the result plan and represented in the issues component.
Engine failures never trigger an undeclared fallback method. Explicit
[`analysis_method_chain()`](https://bqmaks.github.io/bqreport/reference/analysis_method_chain.md)
objects retain every runtime attempt.

## Usage

``` r
run_analysis(plan, data, error = c("collect", "stop", "warn"))
```

## Arguments

- plan:

  A validated `analysis_plan`.

- data:

  A `bq_data` object.

- error:

  Runtime engine error handling: collect, stop, or warn.

## Value

An `analysis_result`.
