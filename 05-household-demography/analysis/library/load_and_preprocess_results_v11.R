load_and_preprocess_endstates_v11 <- function() {
    load_endstates("v1.1") |>
        preprocess_simdata(
            model_version = "v1.1",
            growth_threshold = 5000
        )
}

load_and_preprocess_trajectories_v11 <- function() {
    load_trajectories("v1.1") |>
        preprocess_simdata(
            model_version = "v1.1",
            full_trajectory = TRUE
        )
}

load_rf_importance_and_pd_results_v11 <- function() {

    rf_models <- load_or_build_rf_models(
        model_version = "v1.1",
        overwrite = FALSE
    )

    rf_importance <- bind_rows(
            extract_importance(
                rf_models$survival$matri
            ) |>
            mutate(
            residence_rule = "matrilocal-matrilineal"
            ),    
            extract_importance(
                rf_models$survival$patri
            ) |>
            mutate(
            residence_rule = "patrilocal-patrilineal"
            )
        )

    pd_results <- load_or_build_pd_results(
        model_version = "v1.1",
        overwrite = FALSE,
        response_variable = "survival"
    )

    list(rf_importance = rf_importance, pd_results = pd_results)
}
