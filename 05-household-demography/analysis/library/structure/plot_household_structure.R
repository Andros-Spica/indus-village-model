plot_household_structure <- function(
    endstates,
    color_var = "max_couple_count_distribution_max",
    point_alpha = 0.08,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed",
    xy_log10 = TRUE,
    x_axis_label = "number of households",
    y_axis_label = "number of individuals",
    color_label = "Max. couple count allowed in a household\n(parametric maximum)",
    legend_position = "bottom"
) {
    x_var <- "log_totalHouseholds"
    y_var <- "log_totalIndividuals"
    x_max <- ceiling(max(sampled_trajectories$log_totalHouseholds))
    y_max <- ceiling(max(sampled_trajectories$log_totalIndividuals))
    if (!xy_log10) {
        x_var <- "totalHouseholds"
        y_var <- "totalIndividuals"
    }

    household_structure_plot <- endstates |>
        filter(survival != "Extinction") |>
    
        ggplot(
            aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            color = .data[[color_var]]
            )
        ) +
        geom_point(alpha = point_alpha)

    if (xy_log10) {
        household_structure_plot <- household_structure_plot +
        scale_y_continuous(
            breaks = c(0, 1:y_max),
            labels = c(0, 10^(1:y_max))
            ) +
        scale_x_continuous(
            breaks = c(0, 1:x_max),
            labels = c(0, 10^(1:x_max))
            )
    }

    # Optional faceting
    if (!is.null(facet_var)) {

        if (!all(facet_var %in% colnames(endstates))) {
            stop("Facet variable(s) not found in data frame")
        }
        
        if (length(facet_var) == 1) {
            household_structure_plot <- household_structure_plot +

            facet_wrap(
                as.formula(
                    paste("~", facet_var)
                ),
                ncol = facet_ncol
            )
        } else if (length(facet_var) == 2) {
            household_structure_plot <- household_structure_plot +

            facet_grid(
                as.formula(
                    paste(facet_var[1], "~", facet_var[2])
                ),
                ncol = facet_ncol
            )
        } else {
            stop("Facet variable must be of length 1 or 2")
        }
    }

    household_structure_plot <- household_structure_plot +
        labs(
            x = x_axis_label,
            y = y_axis_label,
            color = color_label
        ) +
        theme(
            legend.position = legend_position
        )
    
    household_structure_plot
}