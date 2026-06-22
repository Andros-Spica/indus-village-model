plot_labour_balance <- function(
    endstates,
    color_var = "labour_demand_per_capita",
    point_alpha = 0.08,
    facet_var = "survival",
    facet_ncol = 1,
    facet_scales = "fixed",
    x_log10 = TRUE,
    x_axis_label = "Labour required",
    y_axis_label = "Labour ratio (available / required)",
    color_label = "Labour demand\nper capita",
    legend_position = "bottom"
) {
    endstates <- endstates |>
        filter(survival != "Extinction") |>
        mutate(
            log_labour_required = log10(labour_required)
        )

    x_var <- "log_labour_required"
    y_var <- "labour_ratio"
    x_max <- ceiling(max(endstates$log_totalHouseholds))
    if (!x_log10) {
        x_var <- "labour_required"
    }

    labour_balance_plot <- endstates |>
    
        ggplot(
            aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            color = .data[[color_var]]
            )
        ) +
        geom_point(alpha = point_alpha) +

        geom_hline(
          yintercept = 1,
          linetype = 2
        )

    if (x_log10) {
        labour_balance_plot <- labour_balance_plot +
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
            labour_balance_plot <- labour_balance_plot +

            facet_wrap(
                as.formula(
                    paste("~", facet_var)
                ),
                ncol = facet_ncol
            )
        } else if (length(facet_var) == 2) {
            labour_balance_plot <- labour_balance_plot +

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

    labour_balance_plot <- labour_balance_plot +
        labs(
            x = x_axis_label,
            y = y_axis_label,
            color = color_label
        ) +
        theme(
            legend.position = legend_position
        )
    
    labour_balance_plot
}