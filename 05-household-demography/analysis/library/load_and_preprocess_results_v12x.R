load_and_preprocess_endstates_v12x <- function() {
    model_versions <- c("v1.2", "v1.2.1", "v1.2.2")
    model_versions |>
        lapply(function(model_version) {
            load_and_preprocess_endstates_v12x_single(model_version)
        }) |>
        bind_rows() |>
        mutate(
            model_version = factor(
                model_version,
                levels = MODEL_VERSION_LEVELS
            )
        )
}

load_and_preprocess_endstates_v12x_single <- function(model_version) {
    load_endstates(model_version) |>
        preprocess_simdata(
            model_version = model_version,
            full_trajectory = FALSE
        )
}

load_and_preprocess_trajectories_v12x <- function(
    burn_in_fraction = 0.5,
    overshoot_magnitude_threshold = 0,
    overshoot_time_threshold = 0.1,
    oscillatory_threshold = 0.05,
    stress_threshold = 0.5,
    full_trajectory = TRUE
) {
    model_versions <- c("v1.2", "v1.2.1", "v1.2.2")
    model_versions |>
        lapply(function(model_version) {
            load_and_preprocess_trajectories_v12x_single(
                model_version = model_version,
                burn_in_fraction = burn_in_fraction,
                overshoot_magnitude_threshold = overshoot_magnitude_threshold,
                overshoot_time_threshold = overshoot_time_threshold,
                oscillatory_threshold = oscillatory_threshold,
                stress_threshold = stress_threshold,
                full_trajectory = full_trajectory
            )
        }) |>
        bind_rows() |>
        mutate(
            model_version = factor(
                model_version,
                levels = MODEL_VERSION_LEVELS
            )
        ) |>
        simplify_regulation_regime()
}

load_and_preprocess_trajectories_v12x_single <- function(
    model_version,
    burn_in_fraction = 0.5,
    overshoot_magnitude_threshold = 0,
    overshoot_time_threshold = 0.1,
    oscillatory_threshold = 0.05,
    stress_threshold = 0.5,
    full_trajectory = TRUE
) {
    load_trajectories(model_version) |>
        preprocess_simdata(
            model_version = model_version,
            full_trajectory = full_trajectory
        ) |>
        compute_density_metrics(
            burn_in_fraction = burn_in_fraction
        ) |>
        classify_regulation_regimes(
            overshoot_magnitude_threshold = overshoot_magnitude_threshold,
            overshoot_time_threshold = overshoot_time_threshold,
            oscillatory_threshold = oscillatory_threshold,
            stress_threshold = stress_threshold
        )
}

load_rf_importance_and_pd_results_v12x <- function() {
    model_versions <- c("v1.2", "v1.2.1", "v1.2.2")
    model_versions |>
        lapply(function(model_version) {
            load_rf_importance_and_pd_results_v12x_single(
                model_version = model_version
            )
        }) |>
        setNames(model_versions)
}

load_rf_importance_and_pd_results_v12x_single <- function(
    model_version
) {
    rf_models <- load_or_build_rf_models(
        model_version = model_version,
        overwrite = FALSE
    )

    rf_importance <- bind_rows(
            extract_importance(
                rf_models$pressure$matri
            ) |>
            mutate(
            residence_rule = "matrilocal-matrilineal"
            ),    
            extract_importance(
                rf_models$pressure$patri
            ) |>
            mutate(
            residence_rule = "patrilocal-patrilineal"
            )
        )

    pd_results <- load_or_build_pd_results(
        model_version = model_version,
        overwrite = FALSE,
        response_variable = "pressure"
    )

    list(rf_importance = rf_importance, pd_results = pd_results)
}
