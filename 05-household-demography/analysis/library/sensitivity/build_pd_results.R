build_pd_results <- function(
    sensitivity_data,
    rf_models,
    parameter,
    response_variable,
    response_class = NULL
) {
    source("library/sensitivity/compute_partial_dependence.R")

    pd_results <- bind_rows(
        compute_partial_dependence(
            rf_model = rf_models$matri,
            data = sensitivity_data$matri,
            parameter = parameter,
            response_variable = response_variable,
            response_class = response_class
        ) |>
        mutate(
            residence_rule = "matrilocal-matrilineal"
            ),
        patri = compute_partial_dependence(
            rf_model = rf_models$patri,
            data = sensitivity_data$patri,
            parameter = parameter,
            response_variable = response_variable,
            response_class = response_class
        ) |>
        mutate(
            residence_rule = "patrilocal-patrilineal"
        )
    )

    pd_results
}