prepare_sensitivity_predictors <- function(
    data,
    parameter_metadata
) {

  predictor_variables <- parameter_metadata |>
    filter(include_sensitivity) |>
    pull(parameter)

  data |>
    select(
      all_of(c(
        predictor_variables,
        "log_totalIndividuals",
        "survival",
        "residence_rule"
      ))
    ) |>
    na.omit()
}
