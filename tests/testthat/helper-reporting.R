reporting_model_result <- function() {
  data <- as_bq_data(tibble::tibble(
    y = c(1, 2, 3, 5, 6, 8), x = c(0, 0, 1, 1, 2, 2)
  )) |>
    set_outcome(y, type = "continuous") |>
    set_predictor(x, type = "continuous")
  data |> plan_analysis(y, x) |> validate_plan(data) |> run_analysis(data)
}
