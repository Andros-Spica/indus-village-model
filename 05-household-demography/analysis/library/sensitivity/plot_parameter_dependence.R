plot_parameter_dependence <- function(
    data,
    pd_data,
    parameter,
    response,
    pd_y = "yhat",
    pd_label = "Partial dependence",
    bins = 30,
    max_count = NULL,
    facet = NULL,
    lab_x = parameter,
    lab_y = response,
    plot_title = NULL
) {

    # Check that parameter and response are in data
    if (!parameter %in% colnames(data)) {

        stop(
            paste("Parameter", parameter, "not found in data")
        )
    }
    if (!response %in% colnames(data)) {

        stop(
            paste("Response", response, "not found in data")
        )
    }

    # Check that pd_y is in pd_data
    if (!pd_y %in% colnames(pd_data)) {

        stop(
            paste("pd_y", pd_y, "not found in pd_data")
        )
    }

    # Check that facet is in data and pd_data
    if (!is.null(facet)) {

        if (!facet %in% colnames(data)) {

            stop(
                paste("Facet", facet, "not found in data")
            )
        }
        if (!facet %in% colnames(pd_data)) {

            stop(
                paste("Facet", facet, "not found in pd_data")
            )
        }
    }

     if (is.null(plot_title)) {

        plot_title <- paste(
            "Partial dependence of",
            response,
            "on",
            parameter
        )
    }

    max_response <- max(data[[response]], na.rm = TRUE)

    dependence_plot <- ggplot() +
  
        geom_bin2d(
            data = data,
            aes(
            .data[[parameter]],
            .data[[response]] / max_response
            ),
            bins = bins,
            alpha = 0.6
        ) +

        geom_line(
            data = pd_data,
            aes(
            .data[[parameter]],
            .data[[pd_y]],
            color = pd_label
            ),
            linewidth = 1.5
        ) +

        coord_cartesian(
            ylim = c(0, 1)
        ) +

        scale_color_discrete(
            name = NULL
            ) +

        labs(
            x = lab_x,
            y = lab_y
        ) +

        ggtitle(plot_title)

    if (!is.null(max_count)) {
        
        dependence_plot <- dependence_plot +

        scale_fill_viridis_c(
            name = "Count",
            limits = c(0, max_count),
            trans = "sqrt"
        )
    } else {

        dependence_plot <- dependence_plot +

        scale_fill_viridis_c(
            name = "Count",
            trans = "sqrt"
        )
    }

    if (!is.null(facet)) {

        residence_rule_labels <- c(
            "matrilocal-matrilineal" = "\u2640",
            "patrilocal-patrilineal" = "\u2642"
        )
        
        dependence_plot <- dependence_plot +

        facet_wrap(
            as.formula(paste("~", facet)),
            ncol = 2,
            scales = "fixed",
            labeller = labeller(
                residence_rule = residence_rule_labels
            )
        )
    }

    dependence_plot
}

compute_global_max_bin_count <- function(
    data,
    parameters,
    response,
    bins = 30
) {

    max_counts <- c()

    for (param in parameters) {

        binned <- ggplot2::ggplot_build(

            ggplot(
                data,
                aes(
                    .data[[param]],
                    .data[[response]]
                )
            ) +

            geom_bin2d(
                bins = bins
            )

        )$data[[1]]

        max_counts <- c(
            max_counts,
            max(binned$count, na.rm = TRUE)
        )
    }

    max(max_counts)
}

plot_parameter_dependence_composite <- function(
    sensitivity_data,
    pd_results,
    top_parameters,
    response_variable,
    pd_label = "Partial dependence",
    bins = 30,
    lab_x = NULL,
    lab_y = NULL,
    ncol = 2,
    legend_key_width = unit(1.5, "cm"),
    plot_title_size = 10,
    plot_title_margin_bottom = 5
) {
    GLOBAL_MAX_COUNT <- compute_global_max_bin_count(
        data = bind_rows(
            sensitivity_data$matri,
            sensitivity_data$patri
            ),
        parameters = top_parameters,
        response = response_variable,
        bins = bins
    )

    pd_plots <- list()

    for (param in top_parameters) {
    
    pd_plots[[param]] <- plot_parameter_dependence(
        data = bind_rows(
        sensitivity_data$matri,
        sensitivity_data$patri
        ),
        pd_data = pd_results[[param]],
        parameter = param,
        response = response_variable,
        facet = "residence_rule",
        pd_label = pd_label,
        lab_x = lab_x,
        lab_y = lab_y,
        plot_title = param,
        bins = bins,
        max_count = GLOBAL_MAX_COUNT
    )
    
    }

    param_pd_plots <- wrap_plots(
        pd_plots,
        ncol = ncol
    ) +
    plot_layout(guides = "collect") &
    theme(
        legend.position = "bottom",
        legend.key.width = legend_key_width,
        plot.title = element_text(
            size = plot_title_size,
            margin = ggplot2::margin(b = plot_title_margin_bottom)
        )
    )

    param_pd_plots
}
