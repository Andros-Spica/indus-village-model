# Daily model family:
# v1.3   = daily baseline
# v1.4   = daily density dependence
# v1.4.1 = daily demand-weighted density dependence
# v1.4.2 = daily demand- and labour-weighted density dependence


DAILY_MODEL_VERSIONS <- c(
    "v1.3",
    "v1.4",
    "v1.4.1",
    "v1.4.2"
)


# -------------------------------------------------------------------------
# End states
# -------------------------------------------------------------------------

load_and_preprocess_endstates_v13_v14x <- function(
    growth_threshold = 5000
) {
    DAILY_MODEL_VERSIONS |>
        lapply(
            load_and_preprocess_endstates_v13_v14x_single,
            growth_threshold = growth_threshold
        ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
            model_version = factor(
                model_version,
                levels = MODEL_VERSION_LEVELS
            )
        )
}


load_and_preprocess_endstates_v13_v14x_single <- function(
    model_version,
    growth_threshold = 5000
) {
    validate_daily_model_version(model_version)

    endstates <- load_endstates(model_version)

    # v1.3 retains the baseline extinction/persistence/growth classification.
    # v1.4.x uses density-regulated outcome categories and does not require
    # growth_threshold in preprocessing.
    if (model_version == "v1.3") {
        endstates <- endstates |>
            preprocess_simdata(
                model_version = model_version,
                growth_threshold = growth_threshold,
                full_trajectory = FALSE
            )
    } else {
        endstates <- endstates |>
            preprocess_simdata(
                model_version = model_version,
                full_trajectory = FALSE
            )
    }

    endstates |>
        normalise_survival()
}

load_or_build_reduced_trajectories_v13_v14x <- function(
    cache_file =
        "data/v13_v14x_trajectories_figure_behaviour.rds",
    overwrite = FALSE,
    ...
) {
    if (file.exists(cache_file) && !overwrite) {
        return(readRDS(cache_file))
    }

    reduced_trajectories <-
        load_and_preprocess_trajectories_v13_v14x(...)

    dir.create(
        dirname(cache_file),
        recursive = TRUE,
        showWarnings = FALSE
    )

    saveRDS(
        reduced_trajectories,
        cache_file
    )

    reduced_trajectories
}

# -------------------------------------------------------------------------
# Lightweight trajectories for manuscript figures
# -------------------------------------------------------------------------

load_and_preprocess_trajectories_v13_v14x <- function(
    growth_threshold = 5000,
    burn_in_fraction = 0.5,
    overshoot_magnitude_threshold = 0,
    overshoot_time_threshold = 0.1,
    oscillatory_threshold = 0.05,
    stress_threshold = 0.5,
    sample_interval_days = 365,
    step_start = 1,
    step_end = 182500,
    selected_variables = NULL,
    ignored_variables = c(
        "womenAgeStructure",
        "menAgeStructure"
    )
) {
    trajectory_list <- vector(
        mode = "list",
        length = length(DAILY_MODEL_VERSIONS)
    )

    names(trajectory_list) <- DAILY_MODEL_VERSIONS

    for (model_version in DAILY_MODEL_VERSIONS) {
        message("Loading and reducing trajectories for ", model_version)

        trajectory_list[[model_version]] <-
            load_and_preprocess_trajectories_v13_v14x_single(
                model_version = model_version,
                growth_threshold = growth_threshold,
                burn_in_fraction = burn_in_fraction,
                overshoot_magnitude_threshold =
                    overshoot_magnitude_threshold,
                overshoot_time_threshold =
                    overshoot_time_threshold,
                oscillatory_threshold = oscillatory_threshold,
                stress_threshold = stress_threshold,
                sample_interval_days = sample_interval_days,
                step_start = step_start,
                step_end = step_end,
                selected_variables = selected_variables,
                ignored_variables = ignored_variables
            )

        # Encourage release of memory before loading the next model version.
        invisible(gc())
    }

    trajectory_list |>
        dplyr::bind_rows() |>
        dplyr::mutate(
            model_version = factor(
                model_version,
                levels = MODEL_VERSION_LEVELS
            )
        ) |>
        simplify_regulation_regime() |>
        normalise_survival()
}

load_and_preprocess_trajectories_v13_v14x_single <- function(
    model_version,
    suffix = "",
    growth_threshold = 5000,
    burn_in_fraction = 0.5,
    overshoot_magnitude_threshold = 0,
    overshoot_time_threshold = 0.1,
    oscillatory_threshold = 0.05,
    stress_threshold = 0.5,
    sample_interval_days = 365,
    step_start = 1,
    step_end = 182500,
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
        "regulation_regime",
        "max_population",
        "carrying_capacity",
        "pressure"
    )
) {
    validate_daily_model_version(model_version)

    trajectories <- load_daily_trajectories_raw(
        model_version = model_version,
        suffix = suffix,
        selected_variables = NULL,
        ignored_variables = ignored_variables,
        step_start = step_start,
        step_end = step_end
    ) |>
    add_daily_calendar()

    if (model_version == "v1.3") {
        trajectories <- trajectories |>
            preprocess_simdata(
                model_version = model_version,
                growth_threshold = growth_threshold,
                full_trajectory = TRUE
            )
    } else {
        trajectories <- trajectories |>
            preprocess_simdata(
                model_version = model_version,
                full_trajectory = TRUE
            ) |>
            compute_density_metrics(
                burn_in_fraction = burn_in_fraction
            ) |>
            classify_regulation_regimes(
                overshoot_magnitude_threshold =
                    overshoot_magnitude_threshold,
                overshoot_time_threshold =
                    overshoot_time_threshold,
                oscillatory_threshold =
                    oscillatory_threshold,
                stress_threshold =
                    stress_threshold
            )
    }

    trajectories |>
        reduce_daily_trajectories_for_behaviour_figure(
            sample_interval_days = sample_interval_days,
            keep_variables = keep_variables
        )
}

load_daily_trajectories_raw <- function(
    model_version,
    suffix = "",
    selected_variables = NULL,
    ignored_variables = c(
        "womenAgeStructure",
        "menAgeStructure"
    ),
    step_start = 1,
    step_end = 182500
) {
    common_args <- list(
        model_version = model_version,
        step_start = step_start,
        step_end = step_end
    )

    if (!is.null(selected_variables)) {
        return(
            do.call(
                load_trajectories,
                c(
                    common_args,
                    list(
                        suffix = suffix,
                        selected_variables = selected_variables
                    )
                )
            )
        )
    }

    if (!is.null(ignored_variables)) {
        do.call(
            load_trajectories,
            c(
                common_args,
                list(
                    suffix = suffix,
                    ignored_variables = ignored_variables
                )
            )
        )
    }
    
}

add_daily_calendar <- function(
    data,
    year_length_in_days = 365
    ) {

    if (!"X.step." %in% names(data)) {
        stop("`X.step.` is required to construct daily calendar variables.")
    }

    reconstructed_year <- floor((data$X.step. - 1) / year_length_in_days)
    reconstructed_day  <- ((data$X.step. - 1) %% year_length_in_days) + 1

    if (!"currentYear" %in% names(data)) {
        data$currentYear <- reconstructed_year
    } else {
        data$currentYear <- dplyr::coalesce(
            as.numeric(data$currentYear),
            reconstructed_year
        )
    }

    if (!"currentDayOfYear" %in% names(data)) {
        data$currentDayOfYear <- reconstructed_day
    } else {
        data$currentDayOfYear <- dplyr::coalesce(
            as.numeric(data$currentDayOfYear),
            reconstructed_day
        )
    }

    data
}

reduce_daily_trajectories_for_behaviour_figure <- function(
    trajectories,
    sample_interval_days = 365,
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
        "regulation_regime",
        "max_population",
        "carrying_capacity",
        "pressure"
    )
) {
    if (!"step" %in% names(trajectories)) {
        stop("The trajectory data must contain `step`.")
    }

    if (
        length(sample_interval_days) != 1 ||
        is.na(sample_interval_days) ||
        sample_interval_days < 1
    ) {
        stop("`sample_interval_days` must be one positive number.")
    }

    available_keep_variables <- intersect(
        keep_variables,
        names(trajectories)
    )

    trajectories |>
        # Always preserve the first observation and then retain the requested
        # temporal interval. This avoids dropping the initial state if steps
        # begin at 1 rather than 0.
        dplyr::group_by(
            dplyr::across(
                dplyr::any_of(
                    c(
                        "model_version",
                        "residence_rule",
                        "run_unique_id"
                    )
                )
            )
        ) |>
        dplyr::filter(
            dplyr::row_number() == 1L |
                ((step - min(step, na.rm = TRUE)) %%
                    sample_interval_days == 0)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
            dplyr::all_of(available_keep_variables)
        )
}

validate_daily_model_version <- function(model_version) {
    if (
        length(model_version) != 1 ||
        !model_version %in% DAILY_MODEL_VERSIONS
    ) {
        stop(
            "`model_version` must be one of: ",
            paste(
                DAILY_MODEL_VERSIONS,
                collapse = ", "
            )
        )
    }

    invisible(TRUE)
}

normalise_survival <- function(dataset) {
    dataset |>
        mutate(
            behaviour_outcome = case_when(
            model_version == "v1.3" &
                survival == "Growth" ~ "Growth",
            
            model_version == "v1.3" &
                survival == "Persistence" ~ "Persistence",
            
            survival == "Persistence\n(low pressure)" ~
                "Persistence\n(low pressure)",
            
            survival == "Persistence\n(high pressure)" ~
                "Persistence\n(high pressure)",
            
            survival == "Overshooting" ~ "Overshooting",
            
            survival == "Extinction" ~ "Extinction",
            
            TRUE ~ as.character(survival)
            )
        ) |>
        mutate(
            survival = factor(
            survival,
            levels = SURVIVAL_LEVELS_MIXED
            )
        )
}
