compose_figure_labour <- function(
    endstates,
    sampled_trajectories,
    group_var = "survival",
    facet_var = c("residence_rule", "model_version"),
    sensitivity_data,
    rf_importance_and_pd_results,
    response_variable = "pressure",
    pd_label = "Expected change in response variable",
    pd_title = "Labour demand per capita",
    file_name = "figure_labour",
    width = 12,
    height = 10,
    heights = c(1, 1, 1),
    outcome_palette = SURVIVAL_PALETTE,
    legend_title = "",
    legend_position = "right",
    strip_text_size_y = 8,
    strip_text_size_x = 8
) {
    # preparations
    model_versions <- names(rf_importance_and_pd_results)

    # Panel A: labour ratio violin plot
    endstates_violin_plot <- endstates |>
        plot_labour_ratio_distrib(
            x_var = "residence_rule",
            fill_var = group_var,
            facet_var = "model_version",
            pos_doge_width = 1.0,
            legend_position = legend_position,
            threshold_linetype = 2,
            threshold_linewidth = 0.8,
            threshold_alpha = 0.4
        ) +
        labs(tag = "A") +
        theme(
            strip.text.x = element_text(
                size = strip_text_size_x,
                face = "bold"
            ),
            plot.tag.position = "topright",
            plot.tag = element_text(size = 14, face = "bold")
        )

    # Panel B: labour ratio trajectories
    pop_trajectories_plot <- sampled_trajectories |>
        plot_trajectories(
            x_var = "currentYear",
            y_var = "labour_ratio",
            trajectory_id = "run_unique_id",
            line_alpha = 0.15,
            median_alpha = 0.8,
            median_linewidth = 1.2,
            y_axis_log10 = FALSE,
            y_axis_label = "Labour ratio",
            group_var = group_var,
            facet_var = facet_var,
            facet_ncol = 2,
            facet_scales = "free_x",
            y_threshold_var = 1,
            thresholds_linetype = 2,
            thresholds_linewidth = 0.8,
            thresholds_alpha = 0.4,
            legend_position = legend_position
        ) +
        labs(tag = "B") +
        theme(
            strip.text.x = element_blank(),
            plot.tag.position = "topright",
            plot.tag = element_text(size = 14, face = "bold")
        )

    # Panel C: PD labour demand per capita on pressure
    pd_plots <- lapply(
        model_versions,
        function(this_model_version) {
            plot_parameter_dependence_pair(
                filter_sensitivity_data(
                    sensitivity_data = sensitivity_data,
                    this_model_version = this_model_version),
                rf_importance_and_pd_results[[this_model_version]]$pd_results,
                "labour_demand_per_capita",
                response_variable = response_variable,
                pd_label = pd_label,
                plot_title_margin_bottom = -5,
                plot_title_hjust = 0.5,
                show_pd_legend = FALSE,
                legend_position = "bottom"
            ) +
            theme(
                legend.key.width = unit(0.5, "cm"),
                legend.key.height = unit(0.2, "cm")
            )
        }
    )

    pd_panel  <- (pd_plots[[1]] +
        labs(tag = "C", subtitle = pd_title) +
        theme(
            plot.tag.position = "topright",
            plot.tag = element_text(size = 14, face = "bold")
        )) | pd_plots[[2]]

    # combine patchwork figure
    combined_figure <-
        endstates_violin_plot /
        pop_trajectories_plot /
        pd_panel +
        plot_layout(
            #guides = "collect",
            heights = heights
        ) &
        theme(
            plot.title = element_text(
                size = 0,
                face = "bold"
            ),
            plot.subtitle = element_text(
                size = 9,
                face = "bold"
            ),
            plot.tag = element_text(
                size = 12 ,
                face = "bold"
            ),
            plot.tag.position = "topleft",
            plot.margin = ggplot2::margin(
                t = 5 ,
                r = 5 ,
                b = 5 ,
                l = 5 
            ),
            axis.title = element_text(
                size = 9 
            ),
            axis.text = element_text(
                size = 6
            ),
            legend.title = element_text(
                size = 8
            ),
            legend.text = element_text(
                size = 6 
            ),
            strip.text.y = element_text(
                size = strip_text_size_y,
                face = "bold"
            )
        )
    
    # export it
    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        width = width,
        height = height
    )
}
