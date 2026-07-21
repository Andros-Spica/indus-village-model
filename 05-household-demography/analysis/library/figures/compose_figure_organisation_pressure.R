compose_figure_organisation_pressure <- function(
    endstates,
    sampled_trajectories,
    colour_var = "regulation_regime",
    shape_var = "residence_rule",
    facet_var = "residence_rule",
    stress_threshold = 0.5,
    oscillatory_threshold = 0.05,
    bins = 40,
    file_name = paste0("figure_organisation_pressure_", model_version),
    width = 12,
    height = 10,
    heights = c(1, 1),
    strip_text_size = 8
) {
    pressure_annotations <- build_pressure_annotation_data(
        x_cv = oscillatory_threshold,
        y_pressure = stress_threshold
    )

    # distribution of population pressure across simulations (effective carrying capacity varies across versions)
    endstates_pressure_plot <- endstates |>
        filter(survival != "Extinction") |>
        plot_endstate_variable_distribution(
            main_var = "pressure",
            fill_var = "survival",
            facet_var = facet_var,
            bins = bins,
            x_axis_label = "Equilibrium population pressure",
            y_axis_label = "Simulation count",
            threshold_values = c(stress_threshold, 1),
            thresholds_linetype = c(2, 1),
            x_axis_log10 = FALSE,
            y_axis_log10 = FALSE
        )
    
    # Mean population pressure vs. CV of pressure
    mean_vs_cv_pressure_plot <- sampled_trajectories |>
        plot_pressure_distributions_facet(
            colour_var = colour_var,
            shape_var = shape_var,
            stress_threshold = stress_threshold,
            oscillatory_threshold = oscillatory_threshold,
            thresholds_linetype = 2,
            point_alpha = 0.8,
            facet_var = facet_var,
            facet_ncol = 2,
            facet_scales = "fixed",
            show_thresholds_labels = FALSE
        ) +
        scale_color_viridis_d(
            begin = 0.4,
            end = 0.9,
            option = "turbo",
            guide = guide_legend(
            title = "",
            override.aes = list(size = 5, alpha = 1),
            reverse = TRUE
            )
        ) +
        guides(
            shape = "none"
        ) +
        geom_text(
            data = pressure_annotations,
            aes(
                x,
                y,
                label = label,
                hjust = hjust,
                vjust = vjust,
                angle = angle
            ),
            size = 2,
            inherit.aes = FALSE
        )

    # combine the two panels into a single figure (using patchwork)
    combined_figure <- endstates_pressure_plot /
        mean_vs_cv_pressure_plot +
        plot_layout(
            heights = heights
        ) +
        plot_annotation(
            tag_levels = "A"
        ) &
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.tag = element_text(size = 14, face = "bold"),
            plot.tag.position = "topleft",
            plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8),
            strip.text = element_text(size = strip_text_size)
        )

    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        width = width,
        height = height
    )
}

build_pressure_annotation_data <- function(
    x_cv = 0.05,
    y_pressure = 0.5,
    x_max = 0.19,
    y_max = 1
) {
    tibble::tribble(
        ~model_version, ~residence_rule, ~x, ~y, ~label, ~hjust, ~vjust, ~angle,

        "v1.2", "matrilocal-matrilineal", x_cv, y_max, "Stable", 1.1, -0.7, 90,

        "v1.2", "matrilocal-matrilineal", x_cv, y_max, "Oscillatory", 1.1, 1.7, 90,

        "v1.2", "matrilocal-matrilineal", x_max, y_pressure, "High pressure", 1.1, -0.7, 0,

        "v1.2", "matrilocal-matrilineal", x_max, y_pressure, "Low pressure", 1.1, 1.7, 0
    )
}
