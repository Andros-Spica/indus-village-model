compute_age_structure_indicators <- function(
    age_structure_df,
    group_vars = NULL
) {
    # check if age_structure_df has the required columns
    required_cols <- c(
        "run_id",
        "age_bin",
        "count"
    )
    missing_cols <- setdiff(required_cols, colnames(age_structure_df))
    if (length(missing_cols) > 0) {
        stop(
            paste(
                "The following required columns are missing from age_structure_df:",
                paste(missing_cols, collapse = ", ")
            )
        )
    }
    # check if group_vars are valid
    if (!is.null(group_vars)) {
        invalid_group_vars <- setdiff(group_vars, colnames(age_structure_df))
        if (length(invalid_group_vars) > 0) {
            stop(
                paste(
                    "The following group_vars are not columns in age_structure_df:",
                    paste(invalid_group_vars, collapse = ", ")
                )
            )
        }
    }
    
    # compute age structure indicators
    indicators_df <- age_structure_df |>

        mutate(

            age_lower = stringr::str_extract(
                age_bin,
                "\\d+"
            ) |>
            as.numeric()
        ) |>

        group_by(
            run_id,
            !!!rlang::syms(group_vars)
        ) |>

        summarise(

            total_population =
                sum(count),

            youth_share =
                sum(
                    count[age_lower < 15]
                ) / total_population,

            reproductive_share =
                sum(
                    count[
                        age_lower >= 15 &
                        age_lower < 45
                    ]
                ) / total_population,

            elderly_share =
                sum(
                    count[age_lower >= 65]
                ) / total_population,

            dependency_ratio =
                (
                    sum(count[age_lower < 15]) +
                    sum(count[age_lower >= 65])
                ) /

                sum(
                    count[
                        age_lower >= 15 &
                        age_lower < 65
                    ]
                ),

            median_age =
                weighted.mean(
                    age_lower,
                    count
                ),

            .groups = "drop"
        )

    indicators_df
}

indicators_summary <- function(
    indicators_df,
    group_vars = NULL
) {
    
    indicator_summary <- indicators_df |>

    group_by(
        !!!rlang::syms(group_vars)
    ) |>

    summarise(

        mean_median_age =
            mean(median_age),

        mean_youth_share =
            mean(youth_share),

        mean_dependency_ratio =
            mean(dependency_ratio),

        .groups = "drop"
    )

    indicator_summary
}