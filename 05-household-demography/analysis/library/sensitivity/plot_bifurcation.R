plot_bifurcation <- function(
    df,
    parameter,
    response,
    colour = NULL
) {

  ggplot(
    df,
    aes(
      .data[[parameter]],
      .data[[response]]
    )
  ) +

    geom_point(
      alpha = 0.15,
      size = 1
    ) +

    geom_smooth(
      method = "loess",
      linewidth = 1
    ) +

    labs(
      x = parameter,
      y = response
    )
}
