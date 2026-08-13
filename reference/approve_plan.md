# Approve reviewed analysis tasks

Approval is explicit and can only promote tasks that have completed
preflight validation. Invalid tasks cannot be approved.

## Usage

``` r
approve_plan(plan, analysis_id = NULL)
```

## Arguments

- plan:

  A validated `analysis_plan`.

- analysis_id:

  Optional character vector of analysis identifiers. If omitted, all
  tasks currently in `review` are selected.

## Value

An updated `analysis_plan`.
