plot_pressure_distributions <- function(
    trajectories,
    colour_var = "regulation_regime",
    shape_var = "residence_rule",
    stress_threshold = 0.5,
    oscillatory_threshold = 0.05,
    thresholds_linetype = 2,
    point_alpha = 0.8
) {
    required_cols <- c(
        "run_unique_id",
        "equilibrium_pressure_mean", 
        "equilibrium_pressure_cv", 
        colour_var, 
        shape_var
    )
    
    # Validate all columns at once
    if (!all(required_cols %in% colnames(trajectories))) {
        missing_cols <- setdiff(required_cols, colnames(trajectories))
        stop(paste("The following required columns are missing:", paste(missing_cols, collapse = ", ")))
    }

    trajectories |>
        distinct(
            run_unique_id,
            .keep_all = TRUE
        ) |>
        ggplot(
            aes(
            x = equilibrium_pressure_cv,
            y = equilibrium_pressure_mean,
            colour = .data[[colour_var]],
            shape = .data[[shape_var]]
            )
        ) +
        geom_point(alpha = point_alpha) +
        geom_hline(
            yintercept = c(stress_threshold, 1), 
            linetype = thresholds_linetype
        ) +
        geom_vline(
            xintercept = oscillatory_threshold, 
            linetype = thresholds_linetype
        ) +
        scale_color_viridis_d(
            option = "turbo",
            guide = guide_legend(
            title = "",
            override.aes = list(size = 5, alpha = 1),
            reverse = TRUE
            )
        ) +
        scale_shape(
            guide = guide_legend(
            title = "",
            override.aes = list(size = 5, alpha = 1)
            )
        ) +
        labs(
            x = "Coefficient of variance in population pressure",
            y = "Mean population pressure"
        )
}
