load_and_preprocess_labour_endstates <- function() {

    v122 <-
        load_and_preprocess_endstates_v12x_single(
            model_version = "v1.2.2"
        )

    v142 <-
        load_and_preprocess_endstates_v13_v14x_single(
            model_version = "v1.4.2"
        )

    dplyr::bind_rows(v122, v142) |>
        dplyr::mutate(
            model_version = factor(
                model_version,
                levels = c("v1.2.2", "v1.4.2")
            )
        )
}

load_and_preprocess_labour_trajectories <- function(
    sample_interval_days = 365,
    burn_in_fraction = 0.5,
    overshoot_magnitude_threshold = 0,
    overshoot_time_threshold = 0.1,
    oscillatory_threshold = 0.05,
    stress_threshold = 0.5
) {

    v122 <-
        load_and_preprocess_trajectories_v12x_single(
            model_version = "v1.2.2",
            burn_in_fraction = burn_in_fraction,
            overshoot_magnitude_threshold =
                overshoot_magnitude_threshold,
            overshoot_time_threshold =
                overshoot_time_threshold,
            oscillatory_threshold =
                oscillatory_threshold,
            stress_threshold =
                stress_threshold
        ) |>
        dplyr::mutate(
            currentYear = step,
            currentDayOfYear = 1
        ) |>
        dplyr::select(
            dplyr::any_of(c(
                "model_version",
                "run_unique_id",
                "residence_rule",
                "step",
                "currentYear",
                "labour_ratio",
                "labour_demand_per_capita",
                "survival",
                "regulation_regime"
            ))
        )

    invisible(gc())

    v142 <-
        load_and_preprocess_trajectories_v13_v14x_single(
            model_version = "v1.4.2",
            sample_interval_days = sample_interval_days,
            burn_in_fraction = burn_in_fraction,
            overshoot_magnitude_threshold =
                overshoot_magnitude_threshold,
            overshoot_time_threshold =
                overshoot_time_threshold,
            oscillatory_threshold =
                oscillatory_threshold,
            stress_threshold =
                stress_threshold,
            ignored_variables = c(
                "womenAgeStructure",
                "menAgeStructure"
            ),
            keep_variables = c(
                "model_version",
                "run_unique_id",
                "run_number",
                "residence_rule",
                "step",
                "currentYear",
                "currentDayOfYear",
                "log_totalIndividuals",
                "totalIndividuals",
                "survival",
                "labour_ratio",
                "labour_demand_per_capita",
                "regulation_regime",
                "max_population",
                "carrying_capacity",
                "pressure"
            )
        ) |>
        dplyr::select(
            dplyr::any_of(c(
                "model_version",
                "run_unique_id",
                "residence_rule",
                "step",
                "currentYear",
                "currentDayOfYear",
                "labour_ratio",
                "labour_demand_per_capita",
                "survival",
                "regulation_regime"
            ))
        )

    combined <- dplyr::bind_rows(v122, v142)
    
    stopifnot(
        sum(
            combined$model_version == "v1.4.2" &
            is.finite(combined$labour_ratio)
        ) > 0
    )

    combined <- combined |>
        dplyr::mutate(
            model_version = factor(
                model_version,
                levels = c("v1.2.2", "v1.4.2")
            )
        ) |>
        mutate(
            run_unique_id_comparison =
                interaction(
                    model_version,
                    run_unique_id,
                    drop = TRUE
                )
        ) |>
        simplify_regulation_regime()

    stopifnot(
        sum(
            combined$model_version == "v1.4.2" &
            is.finite(combined$labour_ratio)
        ) > 0
    )

    combined
}

load_or_build_trajectories_labour <- function(
    cache_file =
        "data/labour_trajectories_figure.rds",
    overwrite = FALSE,
    ...
) {
    if (file.exists(cache_file) && !overwrite) {
        return(readRDS(cache_file))
    }

    trajectories <-
        load_and_preprocess_labour_trajectories(...)

    dir.create(
        dirname(cache_file),
        recursive = TRUE,
        showWarnings = FALSE
    )

    saveRDS(
        trajectories,
        cache_file
    )

    trajectories
}

validate_labour_ratio <- function(data, model_version) {
    if (!"labour_ratio" %in% names(data)) {
        stop(
            "`labour_ratio` is absent for ",
            model_version,
            "."
        )
    }

    finite_n <- sum(is.finite(data$labour_ratio))

    if (finite_n == 0) {
        stop(
            "`labour_ratio` contains no finite values for ",
            model_version,
            ". Required source variables may have been excluded during loading."
        )
    }

    data
}

load_rf_importance_and_pd_results_labour <- function() {
    model_versions <- c("v1.2.2", "v1.4.2")
    model_versions |>
        lapply(function(model_version) {
            load_rf_importance_and_pd_results_labour_single(
                model_version = model_version
            )
        }) |>
        setNames(model_versions)
}

load_rf_importance_and_pd_results_labour_single <- function(
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
