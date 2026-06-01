plot_age_structure_indicators <- function(
    indicators_df,
    x_var = NULL,
    fill_var = NULL,
    facet_var = NULL,
    pos_doge_width = 1.0
) {

    indicators_plot <- NULL

    if (is.null(x_var)) {
        indicators_plot <- ggplot(
            indicators_df,
            aes(
                x = 1,
                y = median_age
            )
        )
    } else if (is.null(fill_var)) {
        indicators_plot <- ggplot(
            indicators_df,
            aes(
                x = !!rlang::sym(x_var),
                y = median_age
            )
        )
    } else {
        indicators_plot <- ggplot(
            indicators_df,
            aes(
                x = !!rlang::sym(x_var),
                y = median_age,
                fill = !!rlang::sym(fill_var)
            )
        )
    }

    dodge <- position_dodge(width = pos_doge_width)

    indicators_plot <- indicators_plot +

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
        y = "Median age"
    ) +

    theme_minimal()

    if (!is.null(facet_var)) {

        indicators_plot <- indicators_plot + 

            facet_wrap(
                facets = rlang::sym(facet_var)
            )
    }

    indicators_plot
}