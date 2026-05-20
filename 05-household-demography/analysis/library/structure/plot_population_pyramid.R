library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ============================================================
# Prepare pyramid plotting data
# ============================================================

prepare_pyramid_plot <- function(
    agg_df
) {

    agg_df %>%
        mutate(

            median_plot =
                ifelse(
                    sex == "Male",
                    -median,
                    median
                ),

            mean_plot =
                ifelse(
                    sex == "Male",
                    -mean,
                    mean
                ),

            q25_plot =
                ifelse(
                    sex == "Male",
                    -q25,
                    q25
                ),

            q75_plot =
                ifelse(
                    sex == "Male",
                    -q75,
                    q75
                )
        )
}

# ============================================================
# Plot aggregated population pyramids
# ============================================================

plot_population_pyramid <- function(
    plot_df,
    facet = NULL,
    show_mean = TRUE,
    show_legend = TRUE,
    fill_alpha = 0.35,
    line_width = 1.2
) {

    plot_df <- plot_df %>%

        arrange(sex, age_bin)

    pyramid_plot <- ggplot(
        plot_df,
        aes(
            y = age_bin,
            group = sex
        )
    ) +

        # IQR ribbon
        geom_ribbon(
            aes(
                xmin = q25_plot,
                xmax = q75_plot,
                fill = sex
            ),
            alpha = fill_alpha
        ) +

        # Median profile
        geom_step(
            aes(
                x = median_plot,
                colour = sex
            ),
            linewidth = line_width,
            direction = "mid"
        ) +

        scale_x_continuous(
            labels = abs
        ) +

        scale_fill_manual(
            values = c(
                Female = "#c44e52",
                Male = "#4c72b0"
            )
        ) +

        scale_colour_manual(
            values = c(
                Female = "#c44e52",
                Male = "#4c72b0"
            )
        ) +

        labs(
            x = "Population share",
            y = "Age group",
            fill = NULL,
            colour = NULL
        ) +

        theme_minimal() +

        theme(
            panel.grid.minor = element_blank(),
            legend.position =
                ifelse(
                    show_legend,
                    "right",
                    "none"
                )
        )

    # Optional mean profile
    if (show_mean) {

        pyramid_plot <- pyramid_plot +

            geom_step(
                aes(
                    x = mean_plot,
                    colour = sex
                ),
                linewidth = 0.8,
                linetype = "dashed",
                direction = "mid"
            )
    }

    # Optional faceting
    if (!is.null(facet)) {

        if (!all(facet %in% colnames(plot_df))) {
            stop("Facet variable(s) not found in plot_df")
        }
        
        if (length(facet) == 1) {
            pyramid_plot <- pyramid_plot +

            facet_wrap(
                as.formula(
                    paste("~", facet)
                )
            )
        } else if (length(facet) == 2) {
            pyramid_plot <- pyramid_plot +

            facet_grid(
                as.formula(
                    paste(facet[1], "~", facet[2])
                )
            )
        } else {
            stop("Facet variable must be of length 1 or 2")
        }
        
    }

    return(pyramid_plot)
}
