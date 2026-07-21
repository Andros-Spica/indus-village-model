plot_pressure_distributions <- function(
    trajectories,
    colour_var = "regulation_regime",
    shape_var = "residence_rule",
    stress_threshold = 0.5,
    oscillatory_threshold = 0.05,
    thresholds_linetype = 2,
    point_alpha = 0.8,
    show_thresholds_labels = TRUE,
    thresholds_labels_size = 2.5
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

    max_pressure_mean <- 1
    max_pressure_cv <- max(trajectories$equilibrium_pressure_cv, na.rm = TRUE)

    pressure_distribution_plot <- trajectories |>
        distinct(
            across(any_of(c("model_version", "run_unique_id"))),
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
        )
    
    if (show_thresholds_labels) {
        pressure_distribution_plot <- pressure_distribution_plot +
            # Add labels to the quadrants of the plot, near the threshold lines
            # low pressure | high pressure (horizontal line at stress_threshold)
            geom_text(
                aes(
                    x = max_pressure_cv, 
                    y = stress_threshold, 
                    label = "low pressure"
                ),
                inherit.aes = FALSE,
                hjust = 1.1,
                vjust = 1.5,
                size = thresholds_labels_size
            ) +
            geom_text(
                aes(
                    x = max_pressure_cv, 
                    y = stress_threshold, 
                    label = "high pressure"
                ),
                inherit.aes = FALSE,
                hjust = 1.1,
                vjust = -0.5,
                size = thresholds_labels_size
            ) +
            # stable | oscillatory (vertical line at oscillatory_threshold)
            geom_text(
                aes(
                    x = oscillatory_threshold, 
                    y = max_pressure_mean, 
                    label = "stable"
                ),
                inherit.aes = FALSE,
                hjust = 1.1,
                vjust = -0.5,
                size = thresholds_labels_size,
                angle = 90
            ) +
            geom_text(
                aes(
                    x = oscillatory_threshold, 
                    y = max_pressure_mean, 
                    label = "oscillatory"
                ),
                inherit.aes = FALSE,
                hjust = 1.1,
                vjust = 1.5,
                size = thresholds_labels_size,
                angle = 90
            )
    }

    pressure_distribution_plot <- pressure_distribution_plot +
        # Color
        scale_color_viridis_d(
            option = "turbo",
            guide = guide_legend(
            title = "",
            override.aes = list(size = 5, alpha = 1),
            reverse = TRUE
            )
        ) +
        # Shape
        scale_shape(
            guide = guide_legend(
            title = "",
            override.aes = list(size = 5, alpha = 1)
            )
        ) +
        # axis labels
        labs(
            x = "Coefficient of variance in population pressure",
            y = "Mean population pressure"
        )

    pressure_distribution_plot
}

plot_pressure_distributions_facet <- function(
    trajectories,
    colour_var = "regulation_regime",
    shape_var = "residence_rule",
    stress_threshold = 0.5,
    oscillatory_threshold = 0.05,
    thresholds_linetype = 2,
    point_alpha = 0.8,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed",
    show_thresholds_labels = TRUE,
    thresholds_labels_size = 2.5
) {
    pressure_plot <- plot_pressure_distributions(
        trajectories = trajectories,
        colour_var = colour_var,
        shape_var = shape_var,
        stress_threshold = stress_threshold,
        oscillatory_threshold = oscillatory_threshold,
        thresholds_linetype = thresholds_linetype,
        point_alpha = point_alpha,
        show_thresholds_labels = show_thresholds_labels,
        thresholds_labels_size = thresholds_labels_size
    ) +
    # Add boundary lines to better visualize the quadrants of the plot
    geom_vline(
        xintercept = 0, 
        linetype = 1
    ) +
    geom_hline(
        yintercept = 0, 
        linetype = 1
    )

    if (!is.null(facet_var)) {
        if (!all(facet_var %in% colnames(trajectories))) {
            stop("Facet variable(s) not found in data frame")
        } 
        if (length(facet_var) == 1) {
            pressure_plot <- pressure_plot +
            facet_wrap(
                as.formula(
                    paste("~", facet_var)
                ),
                ncol = facet_ncol,
                scales = facet_scales
            )
        } else if (length(facet_var) == 2) {
            pressure_plot <- pressure_plot +

            facet_grid(
                as.formula(
                    paste(facet_var[1], "~", facet_var[2])
                ),
                scales = facet_scales
            )
        } else {
            stop("Facet variable must be of length 1 or 2")
        } 
    }

    pressure_plot
}
