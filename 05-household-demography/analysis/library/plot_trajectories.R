plot_trajectories <- function(
    trajectories,
    x_var = "step",
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
    thresholds_linetype = 2,
    thresholds_linewidth = 1,
    thresholds_alpha = 1,
    legend_position = "right",
    legend_title = NULL
) {

    plot_data <- trajectories |>
        filter(
            is.finite(.data[[x_var]]),
            is.finite(.data[[y_var]])
        )

    if (nrow(plot_data) == 0) {
        stop(
            "No finite observations remain for `",
            x_var,
            "` and `",
            y_var,
            "`. ",
            "Finite x values: ",
            sum(is.finite(trajectories[[x_var]])),
            "; finite y values: ",
            sum(is.finite(trajectories[[y_var]])),
            "."
        )
    }

    median_trajectories <- compute_median_trajectories(
        trajectories = trajectories,
        x_var = x_var,
        y_var = y_var,
        group_var = group_var,
        facet_var = facet_var
    )

    trajectories_plot <- NULL

    if (!is.null(group_var)) {
            
        trajectories_plot <- plot_data |>
        dplyr::filter(
            is.finite(.data[[x_var]]),
            is.finite(.data[[y_var]])
        ) |>
        ggplot2::ggplot(
            ggplot2::aes(
                x = .data[[x_var]],
                y = .data[[y_var]]
            )
        ) +

        ggplot2::geom_line(
            ggplot2::aes(
                group = .data[[trajectory_id]],
                colour = .data[[group_var]]
            ),
            alpha = line_alpha,
            na.rm = TRUE
        ) +

        ggplot2::geom_line(
            data = median_trajectories,
            ggplot2::aes(
                x = .data[[x_var]],
                y = .median_y,
                group = .data[[group_var]],
                colour = .data[[group_var]]
            ),
            linewidth = median_linewidth,
            alpha = median_alpha,
            show.legend = TRUE,
            inherit.aes = FALSE,
            na.rm = TRUE
        )
        
        if (group_var == "regulation_regime") {
            # use colour scheme for regulation_regime
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
                    )
                )
        } else if (group_var == "survival") {
            # use colour scheme for survival
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    begin = 1,
                    end = 0,
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
                    )
                )
        } else if (group_var == "residence_rule") {
            # use colour scheme for residence_rule
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    begin = 0.65,
                    end = 0,
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
                    )
                )
        } else if (group_var == "regulation_rule") {
            # use colour scheme for regulation_rule
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_d(
                    begin = 0.65,
                    end = 0,
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
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
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
                    )
                )
        } else {
            trajectories_plot <- trajectories_plot +
                scale_color_viridis_c(
                    option = "viridis",
                    guide = guide_legend(
                        title = legend_title,
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE,
                        position = legend_position
                    )
                )
        } # schemes for other variables can be added here or after this function

    } else {

        trajectories_plot <- plot_data |>
            dplyr::filter(
                is.finite(.data[[x_var]]),
                is.finite(.data[[y_var]])
            ) |>
            ggplot2::ggplot(
                ggplot2::aes(
                    x = .data[[x_var]],
                    y = .data[[y_var]]
                )
            ) +

            ggplot2::geom_line(
                ggplot2::aes(
                    group = .data[[trajectory_id]]
                ),
                alpha = line_alpha,
                na.rm = TRUE
            ) +

            ggplot2::geom_line(
                data = median_trajectories,
                ggplot2::aes(
                    x = .data[[x_var]],
                    y = .median_y,
                    group = 1
                ),
                linewidth = median_linewidth,
                alpha = median_alpha,
                inherit.aes = FALSE,
                na.rm = TRUE
            )
    }

    if (!is.null(facet_var)) {

      if (!all(facet_var %in% colnames(plot_data))) {
          stop("Facet variable(s) not found in data frame")
      }
      
      if (length(facet_var) == 1) {
          trajectories_plot <- trajectories_plot +

          facet_wrap(
              as.formula(
                  paste("~", facet_var)
              ),
              ncol = facet_ncol,
              scales = facet_scales
          )
      } else if (length(facet_var) == 2) {
          trajectories_plot <- trajectories_plot +

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

    if (!is.null(y_threshold_var)) {
        # TO-DO: add checks
        threshold_value <- NULL
        if (y_threshold_var == "max_population") {
            threshold_value <- if_else(
                y_axis_log10,
                log10(first(plot_data$max_population)),
                first(plot_data$max_population)
            )
        } else if (y_threshold_var == "carrying_capacity") {
            threshold_value <- if_else(
                y_axis_log10,
                log10(first(plot_data$carrying_capacity)),
                first(plot_data$carrying_capacity)
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
                linetype = thresholds_linetype,
                linewidth = thresholds_linewidth,
                alpha = thresholds_alpha
            )
    }

    if (y_axis_log10) {
        y_max <- ceiling(max(plot_data[[y_var]], na.rm = TRUE))
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

compute_median_trajectories <- function(
    trajectories,
    x_var,
    y_var,
    group_var = NULL,
    facet_var = NULL
) {
    grouping_vars <- unique(
        c(
            facet_var,
            group_var,
            x_var
        )
    )

    grouping_vars <- grouping_vars[
        !is.na(grouping_vars) &
        nzchar(grouping_vars)
    ]

    trajectories |>
        dplyr::filter(
            is.finite(.data[[x_var]]),
            is.finite(.data[[y_var]]),
            if (!is.null(group_var)) !is.na(.data[[group_var]]) else TRUE
        ) |>
        dplyr::group_by(
            dplyr::across(
                dplyr::all_of(grouping_vars)
            )
        ) |>
        dplyr::summarise(
            .median_y = stats::median(
                .data[[y_var]],
                na.rm = TRUE
            ),
            .groups = "drop"
        )
}
