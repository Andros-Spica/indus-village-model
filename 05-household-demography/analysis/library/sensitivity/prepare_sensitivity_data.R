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
      model_version,
      all_of(c(
        predictor_variables,
        variables_to_keep
      ))
    ) |>
    # guarantees there are no empty categories in survival
    mutate(
      survival = droplevels(survival)  
    )
}

prepare_sensitivity_data_by_residence_rule <- function(
    endstates,
    response_variables,
    parameter_metadata
) {
  list(
    matri =
      endstates |>
      filter(
        residence_rule ==
          "matrilocal-matrilineal"
      ) |>
      prepare_sensitivity_data(
        parameter_metadata,
        model_version = endstates$model_version[1],
        variables_to_keep = c(
          response_variables,
          "residence_rule"
        )
      ),

    patri =
      endstates |>
      filter(
        residence_rule ==
          "patrilocal-patrilineal"
      ) |>
      prepare_sensitivity_data(
        parameter_metadata,
        model_version = endstates$model_version[1],
        variables_to_keep = c(
          response_variables,
          "residence_rule"
        )
      )
  )
}

filter_sensitivity_data <- function(
    sensitivity_data,
    this_model_version
) {
    list(
        matri = sensitivity_data$matri |>
            filter(model_version == this_model_version),
        patri = sensitivity_data$patri |>
        filter(model_version == this_model_version)
    )
}
