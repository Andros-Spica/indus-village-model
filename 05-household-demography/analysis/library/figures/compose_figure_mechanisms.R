compose_figure_mechanisms <- function(
    rf_importance,
    sensitivity_data,
    pd_results,
    response_variable,
    highlighted_parameters = c(
        "cdmlt_level",
        "c1_fert"
    ),
    pd_label = "P(Extinction)",
    width = 12,
    height = 10,
    heights = c(1, 1),
    file_name = "figure_mechanisms"
) {

    importance_dumbbell_plot <- plot_importance_dumbbell(
        rf_importance, 
        groups = "residence_rule", 
        param_color = parameter_metadata$color[match(rf_importance$parameter, parameter_metadata$parameter)],
        parameter_names_size = 8,
        point_size = 1.2,
        line_width = 1,
        point_label_size = 2.5
        ) +
        labs(tag = "A")
    
    param_pd_plots <- plot_parameter_dependence_composite(
        sensitivity_data,
        pd_results,
        highlighted_parameters,
        response_variable = response_variable,
        pd_label = pd_label,
        ncol = 2,
        plot_title_margin_bottom = -5,
        plot_title_hjust = 0.5
        )

    combined_figure <- importance_dumbbell_plot /
        patchwork::free(param_pd_plots, side = "l") +
        patchwork::plot_layout(
            heights = heights,
            tag_level = "new"
        ) +
        patchwork::plot_annotation(
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
            legend.text = element_text(size = 8)
        )

    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        height = height
    )
}

compose_figure_mechanisms_comparison <- function(
    sensitivity_data,
    rf_importance_and_pd_results,
    response_variable,
    highlighted_parameters = c(
        "density_effect_scaling_factor",
        "cdmlt_level"
    ),
    pd_label = "Expected change in response variable",
    width = 16,
    height = 12,
    widths = c(1, 1, 1),
    heights = c(1, 1, 1),
    file_name = "figure_mechanisms"
) {
    model_versions <- names(rf_importance_and_pd_results)

    # A: importance dumbbell plots
    global_x_max <- max(sapply(model_versions, function(m_ver) {
        max(rf_importance_and_pd_results[[m_ver]]$rf_importance$`%IncMSE`, na.rm = TRUE)
    }), na.rm = TRUE)
    global_x_max <- global_x_max * 1.05 

    importance_plots <- lapply(
        model_versions,
        function(model_version) {

            plot_importance_dumbbell(
                rf_importance_and_pd_results[[model_version]]$rf_importance,
                groups = "residence_rule",
                param_color =
                    parameter_metadata$color[
                        match(
                            rf_importance_and_pd_results[[model_version]]$rf_importance$parameter,
                            parameter_metadata$parameter
                        )
                    ],
                metric = "%IncMSE",
                parameter_names_size = 10,
                point_size = 1.5,
                line_width = 1,
                point_label_size = 3.5,
                point_label_dist = 0.05
            ) + 
            xlim(0, global_x_max) +
            ggtitle(model_version) +
            theme(
                plot.title.position = "plot",
                plot.title = element_text(size = 14, face = "bold")
            )

        }
    )

    importance_panel <- (
        (importance_plots[[1]] + 
            labs(tag = "A") + 
            theme(
            plot.tag.position = "top",
                plot.tag = element_text(size = 16, face = "bold")
            )
        ) /
        importance_plots[[2]] /
        importance_plots[[3]]
    )

    # B,C: PD plots
    
    pd_plots <- lapply(
        highlighted_parameters,
        function(this_parameter) {
            lapply(
                model_versions,
                function(this_model_version) {
                    plot_parameter_dependence_pair(
                        filter_sensitivity_data(
                            sensitivity_data = sensitivity_data,
                            this_model_version = this_model_version),
                        rf_importance_and_pd_results[[this_model_version]]$pd_results,
                        this_parameter,
                        response_variable = response_variable,
                        pd_label = pd_label,
                        plot_title_size = if_else(this_model_version == first(model_versions), (14), 0),
                        plot_title_margin_bottom = -5,
                        plot_title_hjust = 0.5,
                        show_pd_legend = FALSE
                    )
                }
            )
        }
    )

    pd_matrix_panel  <- build_patchwork_3x2matrix(pd_plots)

    combined_figure <- (importance_panel | pd_matrix_panel) +
        patchwork::plot_layout(
            widths = widths,
            heights = heights
        ) &
        theme(
            plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)
        )

    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        width = width,
        height = height
    )
    
}

build_patchwork_3x2matrix <- function(
    nested_plot_list
) {
    flat_plot_list <- unlist(
        nested_plot_list,
        recursive = FALSE
    )

    # 1. Map individual plots from your flat list for clarity
    # Column 1 (Left Plots)
    c1_r1 <- flat_plot_list[[1]] + 
        labs(tag = "B") +
        theme(
            plot.tag.position = "topright",
            plot.tag = element_text(size = 14, face = "bold")
        )
    c1_r2 <- flat_plot_list[[2]]
    c1_r3 <- flat_plot_list[[3]]

    # Column 2 (Right Plots with bottom legends)
    c2_r1 <- flat_plot_list[[4]]
    c2_r2 <- flat_plot_list[[5]]
    c2_r3 <- flat_plot_list[[6]]

    # 2. Assemble rows manually to protect the independent bottom legends
    row1 <- c1_r1 | c2_r1
    row2 <- c1_r2 | c2_r2
    row3 <- c1_r3 | c2_r3

    # 3. Stack the rows vertically and force the multi-panel tags
    final_panel <- (row1 / row2 / row3)

    # 4. Render final plot
    final_panel
}
