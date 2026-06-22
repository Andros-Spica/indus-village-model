plot_labour_ratio_distrib <- function(
    endstates,
    x_var = NULL,
    fill_var = NULL,
    facet_var = NULL,
    pos_doge_width = 1.0,
    label_fill = ""
) {
    endstates <- endstates |>
        filter(survival != "Extinction")
    
    labour_plot <- NULL

    if (is.null(x_var)) {
        labour_plot <- ggplot(
            endstates,
            aes(
                x = 1,
                y = labour_ratio
            )
        )
    } else if (is.null(fill_var)) {
        labour_plot <- ggplot(
            endstates,
            aes(
                x = !!rlang::sym(x_var),
                y = labour_ratio
            )
        )
    } else {
        labour_plot <- ggplot(
            endstates,
            aes(
                x = !!rlang::sym(x_var),
                y = labour_ratio,
                fill = !!rlang::sym(fill_var)
            )
        )
    }

    dodge <- position_dodge(width = pos_doge_width)

    labour_plot <- labour_plot +

    geom_violin(
        position = dodge,
        alpha = 0.5
    ) +

    geom_boxplot(
        position = dodge,
        width = 0.15,
        outlier.alpha = 0.1,
        color = "black"
    ) +

    labs(
        x = NULL,
        y = "Labour ratio",
        fill = label_fill
    ) +

    theme_minimal()

    if (!is.null(facet_var)) {

        labour_plot <- labour_plot + 

            facet_wrap(
                facets = rlang::sym(facet_var)
            )
    }

    labour_plot
}