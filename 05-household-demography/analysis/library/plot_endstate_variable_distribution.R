plot_endstate_variable_distribution <- function(
    endstates,
    main_var = "log_totalIndividuals",
    fill_var = "survival",
    facet_var = "residence_rule",
    bins = 40,
    x_axis_label = "Final population size",
    y_axis_label = "Simulation count",
    threshold_values = NULL,
    thresholds_linetype = 2:6,
    x_axis_log10 = TRUE,
    x_max = 10^4,
    y_axis_log10 = TRUE,
    legend_position = "right",
    legend_title = NULL
) {
  variable_distribution_plot <- NULL

  if (!is.null(fill_var)) {
    variable_distribution_plot <- endstates |>
      ggplot(
        aes(
          x = .data[[main_var]],
          fill = .data[[fill_var]]
          )
      )

    if (fill_var == "survival" & any(endstates$survival == "Extinction")) {
      # use colour scheme for survival
      variable_distribution_plot <- variable_distribution_plot +
        scale_fill_viridis_d(
          begin = 1,
          end = 0,
          option = "turbo",
          guide = guide_legend(
            title = legend_title,
            override.aes = list(linewidth = 5, alpha = 1),
            reverse = TRUE,
            position = legend_position
          )
        )
    } else if (fill_var == "survival" & any(endstates$survival != "Extinction")) {
      cut_limit <- if_else(
        nlevels(endstates$survival) == 3, # base model
        0.5,
        0.65
      )
      variable_distribution_plot <- variable_distribution_plot +
        scale_fill_viridis_d(
          begin = cut_limit,
          end = 0,
          option = "turbo",
          guide = guide_legend(
            title = legend_title,
            override.aes = list(linewidth = 5, alpha = 1),
            reverse = TRUE,
            position = legend_position
          )
        )
    }
    
    # schemes for other variables can be added here or after this function
  } else {
    variable_distribution_plot <- endstates |>
    ggplot(
      aes(
        x = .data[[main_var]]
        )
    )
  }

  variable_distribution_plot <- variable_distribution_plot +
    geom_histogram(
      bins = bins
      ) +
    labs(
      x = x_axis_label,
      y = y_axis_label,
      fill = ""
    )

  # Optional faceting
  if (!is.null(facet_var)) {

      if (!all(facet_var %in% colnames(endstates))) {
          stop("Facet variable(s) not found in data frame")
      }
      
      if (length(facet_var) == 1) {
          variable_distribution_plot <- variable_distribution_plot +

          facet_wrap(
              as.formula(
                  paste("~", facet_var)
              )
          )
      } else if (length(facet_var) == 2) {
          variable_distribution_plot <- variable_distribution_plot +

          facet_grid(
              as.formula(
                  paste(facet_var[1], "~", facet_var[2])
              )
          )
      } else {
          stop("Facet variable must be of length 1 or 2")
      } 
  }
    
  if (!is.null(threshold_values)) {
    # TO-DO: add check
    for (i in 1:length(threshold_values)) {
      variable_distribution_plot <- variable_distribution_plot +
        geom_vline(
            xintercept = threshold_values[i],
            linetype = thresholds_linetype[i]
        )
    }
  }

  if (x_axis_log10) {
      variable_distribution_plot <- variable_distribution_plot +
          scale_x_continuous(
              breaks = c(0, 1:x_max),
              labels = c(0, 10^(1:x_max))
              )
  }

  if (y_axis_log10) {
      variable_distribution_plot <- variable_distribution_plot +
          scale_y_log10(labels = comma)
  }
  
  variable_distribution_plot
}