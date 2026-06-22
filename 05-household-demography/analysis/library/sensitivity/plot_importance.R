plot_importance <- function(
    importance_df,
    metric = "MeanDecreaseGini",
    facet_var = NULL,
    facet_ncol = 2,
    facet_scales = "fixed",
    param_color = "steelblue"
) {

  if (!metric %in% colnames(importance_df)) {
    stop(paste("Metric", metric, "not found."))
  }

  # Handle fill colours
  if (length(param_color) == 1) {

    importance_df$param_color <- param_color

  } else if (length(param_color) == nrow(importance_df)) {

    importance_df$param_color <- param_color

  } else {

    stop(
      "Parameter color must be length 1 or equal to nrow(importance_df)."
    )
  }

  # Global ordering across facets
  parameter_order <- importance_df %>%
    group_by(parameter) %>%
    summarise(
      mean_importance =
        mean(.data[[metric]], na.rm = TRUE)
    ) %>%
    arrange(mean_importance) %>%
    pull(parameter)

  importance_df$parameter <- factor(
    importance_df$parameter,
    levels = parameter_order
  )

  importance_plot <- ggplot(
    importance_df,
    aes(
      parameter,
      .data[[metric]],
      fill = param_color
    )
  ) +

    geom_col() +

    coord_flip() +

    scale_fill_identity() +

    labs(
      x = NULL,
      y = metric
    )

  if (!is.null(facet_var)) {

    importance_plot <- importance_plot +

      facet_wrap(
          as.formula(paste("~", facet_var)),
          ncol = facet_ncol,
          scales = facet_scales
      )
  }

  importance_plot
}

plot_importance_dumbbell <- function(
    importance_df,
    metric = "MeanDecreaseGini",
    groups,
    param_color = "steelblue"
) {

  
  if (length(param_color) == 1) {

    importance_df$param_color <- param_color

  } else if (length(param_color) == nrow(importance_df)) {

    importance_df$param_color <- param_color

  } else {

    stop(
      "Color must be length 1 or equal to nrow(importance_df)"
    )
  }

  importance_wide <- importance_df %>%
    select(
      parameter,
      .data[[groups]],
      .data[[metric]],
      param_color
    ) %>%
    pivot_wider(
      names_from = .data[[groups]],
      values_from = .data[[metric]]
    )

  left_group <- colnames(importance_wide)[3]
  right_group <- colnames(importance_wide)[4]

  parameter_order <- importance_wide %>%
    mutate(
      mean_importance =
        (.data[[left_group]] +
         .data[[right_group]]) / 2
    ) %>%
    arrange(mean_importance) %>%
    pull(parameter)

  importance_wide$parameter <- factor(
    importance_wide$parameter,
    levels = parameter_order
  )

  ggplot(importance_wide) +

    geom_segment(
      aes(
        x = .data[[left_group]],
        xend = .data[[right_group]],
        y = parameter,
        yend = parameter,
        color = param_color
      ),
      linewidth = 1
    ) +

    geom_point(
      aes(
        x = .data[[left_group]],
        y = parameter,
        color = param_color
      ),
      size = 3
    ) +

    geom_point(
      aes(
        x = .data[[right_group]],
        y = parameter,
        color = param_color
      ),
      size = 3
    ) +

    geom_text(
      aes(
        # plot text slightly to the right or left of the points, depending on which group has the lowest or highest importance
        x = ifelse(
          .data[[left_group]] < .data[[right_group]],
          .data[[left_group]] - 0.02 * max(.data[[left_group]], .data[[right_group]]),
          .data[[left_group]] + 0.02 * max(.data[[left_group]], .data[[right_group]])
        ),
        y = parameter
      ),
      label = "\u2640",
      size = 5
    ) +

    geom_text(
      aes(
        x = ifelse(
          .data[[right_group]] < .data[[left_group]],
          .data[[right_group]] - 0.02 * max(.data[[left_group]], .data[[right_group]]),
          .data[[right_group]] + 0.02 * max(.data[[left_group]], .data[[right_group]])
        ),
        y = parameter
      ),
      label = "\u2642",
      size = 5
    ) +

    labs(
      x = metric,
      y = NULL
    )
}
