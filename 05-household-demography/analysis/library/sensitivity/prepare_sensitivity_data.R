prepare_sensitivity_data <- function(
    data,
    parameter_metadata,
    model_version,
    variables_to_keep
) {

  par_met <- parameter_metadata
  if (!grepl("v1.2.2", model_version) && !grepl("v1.4.2", model_version)) {
    par_met <- par_met |>
      filter(parameter != "labour_demand_per_capita")
  }
  
  if (!grepl("v1.2", model_version) && !grepl("v1.4", model_version)) {
    par_met <- par_met |>
      filter(parameter != "carrying_capacity") |>
      filter(parameter != "density_effect_scaling_factor") |>
      filter(parameter != "labour_demand_per_capita")
  }

  if (!grepl("v1.3", model_version) && !grepl("v1.4", model_version)) {
    par_met <- par_met |>
      filter(parameter != "amenorrhea_period_in_days")
  }

  predictor_variables <- par_met |>
    filter(include_sensitivity) |>
    pull(parameter)

  data |>
    select(
      all_of(c(
        predictor_variables,
        variables_to_keep
      ))
    )
}
