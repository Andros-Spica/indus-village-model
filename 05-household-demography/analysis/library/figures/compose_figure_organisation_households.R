compose_figure_organisation_households <- function(
    endstates,
    sampled_trajectories,
    facet_var = "residence_rule",
    file_name = "figure_organisation_households",
    width = 12,
    height = 10,
    widths = c(1, 1),
    heights = c(1, 1)
) {
    # generate panel A figure
    household_structure_plot <- endstates |> 
        plot_household_structure_faceted_fixed(
            color_var = "residence_rule",
            point_alpha = 0.5,
            facet_var = facet_var,
            facet_ncol = 2,
            facet_scales = "fixed",
            xy_log10 = TRUE,
            x_axis_label = "number of households",
            y_axis_label = "number of individuals",
            color_label = "Residence rule",
            legend_position = "bottom",
            rich_text_size = 2
        ) +
        guides( color = "none")
    
    # generate panel B figure
    trajectories_household_structure_plot <- sampled_trajectories |>
        group_by(residence_rule) |>
        mutate(
            household_structure_ratio = (totalIndividuals / totalHouseholds)
        ) |>
        ungroup() |>
        plot_trajectories(
            y_var = "household_structure_ratio",
            trajectory_id = "run_unique_id",
            line_alpha = 0.035,
            median_alpha = 0.8,
            median_linewidth = 1,
            y_axis_log10 = FALSE,
            y_axis_label = "Mean household size",
            group_var = NULL,
            facet_var = facet_var,
            facet_ncol = 2,
            facet_scales = "free_x",
            y_threshold_var = NULL
        )

    # combine the two panels into a single figure (using patchwork)
    combined_figure <- household_structure_plot /
        trajectories_household_structure_plot +
        plot_layout(
            widths = widths,
            heights = heights
        ) +
        plot_annotation(
            tag_levels = "A"
        ) &
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.tag = element_text(size = 14, face = "bold"),
            plot.tag.position = "topleft",
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 5),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8),
            strip.text = element_text(
                size = 8
            )
        )

    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        width = width,
        height = height
    )
}