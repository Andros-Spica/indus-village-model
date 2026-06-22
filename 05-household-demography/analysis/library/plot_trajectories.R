plot_trajectories <- function(
    trajectories,
    y_var = "log_totalIndividuals",
    trajectory_id = "run_unique_id",
    line_alpha = 0.035,
    median_alpha = 0.8,
    median_linewidth = 1,
    y_axis_log10 = TRUE,
    y_axis_label = "number of individuals",
    group_var = NULL,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed",
    y_threshold_var = NULL,
    thresholds_linetype = 2
) {
    trajectories_plot <- NULL

    if (!is.null(group_var)) {
        trajectories_plot <- trajectories |>
            ggplot(
                aes(
                    step, 
                    .data[[y_var]],
                    group = .data[[trajectory_id]],
                    color = .data[[group_var]]
                    )) +
            geom_line(alpha = line_alpha) +
            stat_summary(
                aes(group = .data[[group_var]]),
                fun = median,
                geom = "line",
                linewidth = median_linewidth,
                alpha = median_alpha,
                show.legend = TRUE
            )
        
        if (group_var == "regulation_regime") {
            # use colour scheme for regulation_regime
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = "",
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE
                    )
                )
        } else if (
            is.factor(trajectories[[group_var]]) ||
            is.character(trajectories[[group_var]])
            ) {
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    option = "viridis",
                    drop = FALSE,
                    guide = guide_legend(
                        title = "",
                        override.aes = list(linewidth = 5, alpha = 1)
                    )
                )
        } else {
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_c(
                    option = "viridis",
                    guide = guide_legend(
                    title = "",
                    override.aes = list(linewidth = 5, alpha = 1),
                    reverse = TRUE
                    )
                )
        } # schemes for other variables can be added here or after this function

    } else {
        # TO-DO: add check
        trajectories_plot <- trajectories |>
            ggplot(
                aes(
                    step, 
                    .data[[y_var]],
                    group = .data[[trajectory_id]]
                    )) +
            geom_line(alpha = line_alpha) +
            stat_summary(
                aes(group = 1),
                fun = median,
                geom = "line",
                linewidth = median_linewidth,
                alpha = median_alpha
            )
    }

    if (!is.null(facet_var)) {
        # TO-DO: add check
        trajectories_plot <- trajectories_plot +
            facet_wrap(
                as.formula(paste("~", facet_var)),
                ncol = facet_ncol,
                scales = facet_scales
            )
    }

    if (!is.null(y_threshold_var)) {
        # TO-DO: add checks
        threshold_value <- NULL
        if (y_threshold_var == "max_population") {
            threshold_value <- if_else(
                y_axis_log10,
                log10(first(trajectories$max_population)),
                first(trajectories$max_population)
            )
        } else if (y_threshold_var == "carrying_capacity") {
            threshold_value <- if_else(
                y_axis_log10,
                log10(first(trajectories$carrying_capacity)),
                first(trajectories$carrying_capacity)
            )
        } else if (is.numeric(y_threshold_var)) {
            threshold_value <- y_threshold_var
        } else {
            stop(
                paste("y_threshold_var", y_threshold_var, "is not valid. Choose 'max_population', 'carrying_capacity', or NULL instead.")
            )
        }
        trajectories_plot <- trajectories_plot +
            geom_hline(
                yintercept = threshold_value,
                linetype = thresholds_linetype
            )
    }

    if (y_axis_log10) {
        y_max <- ceiling(max(trajectories[[y_var]]))
        trajectories_plot <- trajectories_plot +
            scale_y_continuous(
                breaks = c(0, 1:y_max),
                labels = c(0, 10^(1:y_max))
                )
    }

    trajectories_plot <- trajectories_plot +
        labs(
            y = y_axis_label
        )
    
    trajectories_plot
}